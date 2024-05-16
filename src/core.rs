
use rustc_hir::def_id::LOCAL_CRATE;
use rustc_hir::definitions::DefPath;
use rustc_hir::definitions::DefPathData;
use rustc_hir::hir_id::OwnerId;

use rustc_hir::Node;
use rustc_hir::ItemKind;
use rustc_hir::TyKind;
use rustc_hir::QPath;
use rustc_hir::ImplItem;
use rustc_hir::FnSig;
use rustc_hir::FnRetTy;
use rustc_hir::Ty;
use rustc_hir::Generics;
use rustc_hir::GenericArg;
use rustc_hir::ImplItemKind;

use tracing::{info, debug, warn, trace};

use crate::{ModuleDecl, TypeCategory, TypeDecl, FnDecl};
use crate::support::ProcessContext;

type WalkResult<T> = std::result::Result<T, String>;

pub struct ProcessHandler;

impl ProcessHandler {
    pub fn handle_extract(&self, ctx: &ProcessContext) -> Vec<FnDecl> {
        ctx.walk_item(|hir_id, _def_id, item| {
            match item.kind {
                ItemKind::Impl(impl_def) if impl_def.items.len() > 0 => {
                    info!("## ItemKind::Impl");

                    debug!("impl_def: {:?}", impl_def);

                    let owner = 
                        walk_impl_owner(ctx, &impl_def)
                        .map_err(|err| warn!(err))
                        .ok()
                    ;
                    info!("ItemKind::Impl: Owner type: {:?}", owner);
                    

                    return Some(impl_def.items.iter()
                        .filter_map(|item| walk_impl_item(ctx, &ctx.impl_item(item.id), &owner))
                        .collect::<Vec<_>>()
                    );
                }
                ItemKind::Fn(FnSig { decl, .. }, generics, _) => {
                    info!("## ItemKind::Fn (global function)");

                    let parent_id = ctx.parent_hir_id(hir_id);
                    let defpath = ctx.def_path(&parent_id.owner.to_def_id());
                    let owner_crate = ctx.crate_name_of(defpath.krate);

                    let owner = TypeDecl {
                        category: TypeCategory::Symbol(symbol_from_defpath(&defpath, &vec![])),
                        module: ModuleDecl { path: mod_from_defpath(&defpath), generics: None, krate: Some(owner_crate) },
                        deprecation: None,
                    };

                    debug!("Global function (name: {}, mod: {:?})", owner.category, owner.module);

                    let prototype_name = format_prototype_name(item.ident.as_str(), &generics);
                    
                    return walk_function_item(ctx, item.owner_id, decl, &prototype_name, &Some(owner)).map(|x| vec![x]);
                }
                // ItemKind::TyAlias(ty, _) => {
                //     info!("## ItemKind::TyAlias");

                //     debug!("type_alias.decl: {:?}", ty);

                //     if let Ok(ref decl) = walk_type_item(ctx, &ty) {
                //         let lhs_parent_id = ctx.parent_hir_id(hir_id);
                //         let lhs_parent_defpath = ctx.def_path(&lhs_parent_id.owner.to_def_id());
                //         debug!("type_alias.path.lhs: {lhs_parent_defpath:?}");

                //         let lhs_mod = ModuleDecl {
                //             path: mod_from_defpath(&lhs_parent_defpath),
                //             krate: Some(ctx.crate_name_of(lhs_parent_defpath.krate)),
                //         };

                //         let lhs_deprecated = ctx.lookup_deprecation(lhs_parent_id.owner.to_def_id());
                //         debug!("type_alias.deprecated: {:?}", lhs_deprecated);

                //         info!("TyAlias: {} <- {}", 
                //             format!("{}::{}", lhs_mod, item.ident.as_str()), 
                //             decl.category.prefix_with(Some(&decl.module))
                //         );
                //     }
                // }
                ItemKind::TyAlias(_, _) |

                ItemKind::Use(_, _) |
                ItemKind::Mod(_) |
                ItemKind::ForeignMod { .. } |
                ItemKind::Const(_, _, _) |
                ItemKind::Macro(_, _) |
                ItemKind::Static(_, _, _) |
                ItemKind::Struct(_, _) |
                ItemKind::Enum(_, _) |
                ItemKind::Union(_, _) |
                ItemKind::GlobalAsm(_) |
                ItemKind::Impl(_) |
                ItemKind::ExternCrate(_) => {
                    trace!("skip: unsupported item of {:?}", item.kind)
                }

                ItemKind::TraitAlias(_, _) |
                ItemKind::Trait(_, _, _, _, _) |
                ItemKind::OpaqueTy(_) => {
                    warn!("todo: {:?}", item.kind);
                }
            }
            None    
        })
    }
}

fn walk_impl_owner(ctx: &ProcessContext, impl_def: &rustc_hir::Impl) -> WalkResult<TypeDecl> {

    let decl = match impl_def.of_trait {
        Some(rustc_hir::TraitRef { path, hir_ref_id, .. }) => {
            let mut decl = resolve_qualified_type(ctx, &path.res, path.segments);

            if let Some(generic_params) = pick_bound_generic_params(ctx, path.segments.first()) {
                decl.category = decl.category.append_generic_params(generic_params);
            }

            let impl_generics = pick_defined_generic_params(impl_def.generics);
            if !impl_generics.is_empty() {
                decl.module.generics = Some(impl_generics);
            }

            trace!("(*1) Impl owner deprecation (trait): {:?}", decl.deprecation);
            decl.deprecation = decl.deprecation.or_else(|| ctx.lookup_deprecation(hir_ref_id.owner.to_def_id()));
            trace!("(*2) Impl owner deprecation (trait): {:?}", decl.deprecation);
            decl
        }
        None => {
            let mut decl = walk_type_item(ctx, &impl_def.self_ty)?;

            let impl_generics = pick_defined_generic_params(impl_def.generics);
            if !impl_generics.is_empty() {
                decl.module.generics = Some(impl_generics);
            }

            trace!("(*1) Impl owner deprecation (type): {:?}", decl.deprecation);
            decl.deprecation = decl.deprecation.or_else(|| ctx.lookup_deprecation(impl_def.self_ty.hir_id.owner.to_def_id()));
            trace!("(*2) Impl owner deprecation (type): {:?}", decl.deprecation);
            decl
        }
    };

    Ok(decl)
}

fn walk_impl_item(ctx: &ProcessContext, impl_item: &ImplItem, owner: &Option<TypeDecl>) -> Option<FnDecl>  {
    match impl_item.kind {
        ImplItemKind::Fn(FnSig{ decl, .. }, _) => {
            trace!("Impl Function Decl: {:?}", decl);
            trace!("Fn generics: {:?}", impl_item.generics);

            let prototype_name = format_prototype_name(impl_item.ident.name.as_str(), impl_item.generics);

            return walk_function_item(ctx, impl_item.owner_id, decl, &prototype_name, owner);
        }
        ImplItemKind::Const(_, _) => {
            trace!("skipped Impl Const");
        }
        ImplItemKind::Type(_) => {
            trace!("skipped Impl Type");
        }
    }

    None
}

fn walk_function_item(ctx: &ProcessContext, owner_id: OwnerId, decl: &rustc_hir::FnDecl, proto_symbol: &str, owner: &Option<TypeDecl>) -> Option<FnDecl> {
    let ret_decl = match walk_return_type_item(ctx, &decl.output) {
        Ok(ty) => ty,
        Err(err) => {
            warn!("(fn.return) {}", err);
            Some(TypeDecl::unknown())
        }
    };
    let args = decl.inputs.iter().enumerate()
        .filter_map(|(i, p)| {
            debug!("(fn.arg[{}]): {:?}", i, p);

            match walk_type_item(ctx, &p) {
                Ok(ty) => Some(ty),
                Err(err) => {
                    warn!("unprocessed (fn.arg[{}]) {}", i, err);
                    Some(TypeDecl::unknown())
                }
            }
        })
        .collect::<Vec<TypeDecl>>()
    ;

    let deprecation = 
        ctx.lookup_deprecation(owner_id.to_def_id())
        .or(
            owner.as_ref()
            .and_then(|x| x.deprecation)
        )
    ;

    debug!("(fn.deprecated) {:?}", deprecation);

    let fn_decl = FnDecl {
        proto: TypeDecl {
            category: TypeCategory::Symbol(proto_symbol.to_string()),
            module: ModuleDecl::none(),
            deprecation,
        },
        ret_decl,
        args,
        owner: owner.clone(),
    };
    info!("(fn.qual): {}", fn_decl);
    debug!("(fn.decl): {:?}", fn_decl);

    Some(fn_decl)
}

fn walk_return_type_item(ctx: &ProcessContext, type_item: &FnRetTy) -> WalkResult<Option<TypeDecl>> {
    debug!("fn.ret_decl: {:?}", type_item);
    match type_item {
        FnRetTy::Return(ty) => walk_type_item(ctx, &ty).map(Option::Some),
        FnRetTy::DefaultReturn(_) => {
            info!("no returining");
            Ok(None)
        }
    }
}

fn walk_type_item(ctx: &ProcessContext, type_item: &Ty) -> WalkResult<TypeDecl> {
    match type_item.kind {
        TyKind::Path(QPath::Resolved(Some(mut_ty), rustc_hir::Path {..})) => {
            info!("(Path) retry `walk_type_item` with internal type item (omit lifecycle parameter)");
            walk_type_item(ctx, mut_ty)
        }
        TyKind::Path(QPath::Resolved(None, rustc_hir::Path {res, segments, ..})) => {
            let resolved_type = resolve_qualified_type(ctx, res, segments);
            trace!("type.resolved: {:?}", resolved_type);

            Ok(resolved_type)
        }
        TyKind::Path(QPath::TypeRelative(ty, _)) => {
            info!("(TypeRelative) retry `walk_type_item` with internal type item");
            walk_type_item(ctx, ty)
        }
        TyKind::Ref(_, mut_ty) => {
            info!("(Ref) retry `walk_type_item` with internal type item");
            walk_type_item(ctx, mut_ty.ty)
        }
        TyKind::Tup(tup_items) if tup_items.len() == 0 => {
            Ok(TypeDecl {
                category: TypeCategory::Tuple(vec![]),
                module: ModuleDecl::none(),
                deprecation: None,
            })
        }
        TyKind::Tup(tup_items) => {
            walk_tuple_items(ctx, &tup_items)
        }
        TyKind::Array(ty, _) |
        TyKind::Slice(ty) => {
            walk_type_item(ctx, &ty).map(TypeDecl::as_slice)
        }
        TyKind::Ptr(rustc_hir::MutTy {ty, mutbl: _ }) => {
            walk_type_item(ctx, &ty).map(TypeDecl::as_ptr)
        }
        TyKind::BareFn(rustc_hir::BareFnTy { decl: fn_decl, .. }) => {
            debug!("[BareFn] walk into function");

            match walk_function_item(ctx, type_item.hir_id.owner, fn_decl, "", &None) {
                Some(fn_decl) => {
                    debug!("[BareFn] resolved: {:?}", fn_decl);
                    Ok(TypeDecl {
                        category: TypeCategory::Symbol("{BareFn}".to_string()),
                        // category: TypeCategory::Symbol(fn_decl.to_string()),
                        module: ModuleDecl::none(),
                        deprecation: fn_decl.proto.deprecation,
                    })
                }
                None => Err("BareFn".to_string())
            }
        }
        TyKind::TraitObject(_, _, _) => {
            warn!("todo: TraitObject is complecated... so retired!");
            Ok(TypeDecl {
                category: crate::TypeCategory::Symbol("{DynTrait}".to_string()),
                module: ModuleDecl::none(),
                deprecation: None,
            })
        }
        TyKind::Never => {
            Ok(TypeDecl {
                category: crate::TypeCategory::Symbol("!".to_string()),
                module: ModuleDecl::none(),
                deprecation: None,
            })
        }

        TyKind::InferDelegation(_, _) => Err("InferDelegation".to_string()),
        TyKind::AnonAdt(_) => Err("AnonAdt".to_string()),
        TyKind::OpaqueDef(item_id, _, _) => {
            let item = ctx.item_of(item_id);

            match item.kind {
                ItemKind::OpaqueTy(rustc_hir::OpaqueTy{ bounds, .. }) => {
                    warn!("todo: ImplTrait is complecated... so retired!");
                    debug!("OpaqueDef: bounds: {bounds:?}");

                    Ok(TypeDecl {
                        category: crate::TypeCategory::Symbol("{ImplTrait}".to_string()),
                        module: ModuleDecl::none(),
                        deprecation: None,
                    })
                }
                _ => {
                    Err("not implemented: OpaqueDef".to_string())
                }
            }
        }
        TyKind::Typeof(_) => Err("Typeof".to_string()),
        TyKind::Infer => Err("Infer".to_string()),
        TyKind::Err(_) => Err("Err".to_string()),
        TyKind::Path(p) => Err(format!("another Path {:?}", p).to_string()),
    }
}

fn walk_tuple_items(ctx: &ProcessContext, tup_items: &[rustc_hir::Ty]) -> WalkResult<TypeDecl> {
    let members = tup_items.iter().enumerate()
        .map(|(i, item)| {
            match walk_type_item(ctx, &item) {
                Ok(ty) => ty,
                Err(err) => {
                    warn!("[TupleItem][{}] unresolved {}", i, err);
                    TypeDecl::unknown()
                }
            }
        })
        // .unzip()
        .collect::<Vec<_>>()
    ;

    trace!("Tuple items: {:?}", members);

    Ok(TypeDecl {
        category: TypeCategory::Tuple(members),
        module: ModuleDecl::none(),
        deprecation: None,
    })
}

fn symbol_from_segments(segments: &[rustc_hir::PathSegment]) -> String {
    segments.last()
        .map(|seg| seg.ident.name.to_ident_string())
        .unwrap_or("".to_string())
}

fn mod_from_segments(segments: &[rustc_hir::PathSegment]) -> Option<String> {
    match segments.len() {
        0 | 1 | 2 => None,
        _ => Some(
            segments.iter()
                .skip(1)
                .take(segments.len()-2)
                .map(|seg| seg.ident.name.as_str().to_string())
                .collect::<Vec<_>>()
                .join("::")
        )
    }
}

fn crate_from_segments(segments: &[rustc_hir::PathSegment]) -> Option<String> {
    match segments.len() {
        0 | 1 => None,
        _ => Some(segments[0].ident.name.as_str().to_string())
    }
}

fn use_path_from_defpath(defpath: &DefPath) -> Option<String> {
    let path = defpath.data.iter()
        .filter_map(|x| match x.data {
            DefPathData::TypeNs(ns) => Some(ns.as_str().to_string()),
            DefPathData::ValueNs(ns) => Some(ns.as_str().to_string()),
            _ => None,
        })
        .collect::<Vec<String>>()
    ;

    match path.len() > 0 {
        true => Some(path.join("::")),
        false => None,
    }
}

fn symbol_from_defpath(defpath: &DefPath, segments: &[rustc_hir::PathSegment]) -> String {
    defpath.data.iter().last()
    .and_then(|x| match x.data {
        DefPathData::TypeNs(ns) => Some(ns.as_str().to_string()),
        DefPathData::ValueNs(ns) => Some(ns.as_str().to_string()),
        _ => None
    })
    .unwrap_or_else(|| symbol_from_segments(segments))
}

fn mod_from_defpath(defpath: &DefPath) -> Option<String> {
    match defpath.data.len() {
        0 | 1 => None,
        _ => {
            Some(
                defpath.data.iter()
                .take(defpath.data.len()-1)
                .filter_map(|x| match x.data {
                    DefPathData::TypeNs(ns) => Some(ns.as_str().to_string()),
                    DefPathData::ValueNs(ns) => Some(ns.as_str().to_string()),
                    _ => None,
                })
                .collect::<Vec<String>>()
                .join("::")
            )
        }
    }
}

fn resolve_qualified_type(ctx: &ProcessContext, res: &rustc_hir::def::Res, segments: &[rustc_hir::PathSegment]) -> TypeDecl {
    match resolve_qualified_type_opt(ctx, res, segments) {
        Some(decl) => {
            // if let Some(import_mod) = ctx.reexport_module(&decl) {
            //     debug!("re-export type replaced: from: {:?}, to: {:?}", decl, import_mod);
            //     decl.module = import_mod;
            // }

            decl
        }
        None => {
            TypeDecl {
                category: TypeCategory::Symbol(format!("***{:?}***", res)),
                module: ModuleDecl::none(),
                deprecation: None,
            }
        }
    }
}

fn resolve_qualified_type_opt(ctx: &ProcessContext, res: &rustc_hir::def::Res, segments: &[rustc_hir::PathSegment]) -> Option<TypeDecl> {
    match res {
        rustc_hir::def::Res::Def(_, def_id) => {
            let deprecation = ctx.lookup_deprecation(*def_id);
            debug!("(qual_type.deprecated) {:?}", deprecation);
        
            let defpath = ctx.def_path(def_id);
            let crate_name = ctx.crate_name_of(defpath.krate);
            debug!("type.qual.defpath: {:?}", defpath);

            let mod_path = mod_from_defpath(&defpath);
            let symbol = symbol_from_defpath(&defpath, segments);

            Some(TypeDecl {
                category: TypeCategory::Symbol(symbol),
                module: ModuleDecl {
                    path: mod_path,
                    generics: None,
                    krate: Some(crate_name),
                },
                deprecation,
            })
        }
        rustc_hir::def::Res::SelfTyAlias {alias_to: def_id, .. } => {
            match ctx.node_of(*def_id) {
                Node::Item(rustc_hir::Item { kind: ItemKind::Impl( rustc_hir::Impl { self_ty, .. }), .. }) => {
                    info!("(SelfTyAlias) retry `walk_type_item` for resolving type");
                    if let Ok(type_decl) = walk_type_item(ctx, self_ty) {
                        return Some(TypeDecl { category: TypeCategory::SelfAlias(Box::new(type_decl.category)), ..type_decl });
                    };
                }
                _ => {}
            }
            
            Some(TypeDecl {
                category: TypeCategory::Symbol(symbol_from_segments(segments)),
                module: ModuleDecl::none(),
                deprecation: None,
            })
        }
        rustc_hir::def::Res::PrimTy(ty) => {
            debug!("prim crate: {} {:?}", ty.name_str().to_string(), Some(ctx.crate_name_of(LOCAL_CRATE)));
            Some(TypeDecl {
                category: TypeCategory::Primitive(ty.name_str().to_string()),
                module: ModuleDecl::none(),
                deprecation: None,
            })
        }
        rustc_hir::def::Res::Err if segments.len() > 0 => {
            Some(TypeDecl {
                category: TypeCategory::Symbol(symbol_from_segments(segments)),
                module: ModuleDecl { path: mod_from_segments(segments), generics: None, krate: crate_from_segments(segments) },
                deprecation: None,
            })
        }
        _ => None,
    }
}

fn pick_defined_generic_params(generics: &Generics) -> Vec<String> {
    generics.params.into_iter().filter_map(|p| match (p.name, p.kind) {
        (_, rustc_hir::GenericParamKind::Lifetime { .. }) => None,
        (rustc_hir::ParamName::Plain(param_name), _) => {
            Some(param_name.as_str().to_string())
        }
        _ => None,
    })
    .collect::<Vec<_>>()
}

fn pick_bound_generic_params(ctx: &ProcessContext, generics: Option<&rustc_hir::PathSegment>) -> Option<String> {
    match generics {
        Some(rustc_hir::PathSegment { args: Some(rustc_hir::GenericArgs { args, .. }), .. }) => {
            let bound_types = args.into_iter()
                .filter_map(|arg| match arg {
                    GenericArg::Type(ty) => walk_type_item(ctx, ty).ok(),
                    _ => None,
                })
                .map(|x| x.category.prefix_with(None)) // remap issue
                // .map(|x| x.category.prefix_with(Some(&x.module)))
                .collect::<Vec<_>>()
                .join(",")
            ;
            trace!("(bound_args) {bound_types:?}");
            Some(bound_types)
        }
        _ => None,
    }
}


fn format_prototype_name(prototype_name: &str, generics: &Generics) -> String {
    let impl_item_generics = pick_defined_generic_params(generics);
    
    match impl_item_generics.len() > 0 {  
        true => format!("{}<{}>", prototype_name.to_string(), impl_item_generics.as_slice().join(", ")),
        false => prototype_name.to_string(),
    }
}

pub mod import_handler {
    use rustc_hir::def::DefKind;
    use rustc_hir::def::Res;
    use rustc_hir::def_id::DefId;
    use rustc_hir::def_id::DefIdSet;
    use rustc_hir::HirId;
    use rustc_hir::UseKind;
    use rustc_middle::ty::Visibility;
    use tracing::{info, debug, warn};
    use std::collections::HashMap;
    use super::ItemKind;

    use crate::ModuleDecl;
    use crate::support::*;
    use crate::TypeDecl;

    pub fn handle_extract(ctx: &ProcessContext) -> HashMap<String, ImportConfig> {
        ctx.walk_import_item(|hir_id, _def_id, item| {
            match item.kind {
                ItemKind::Use(path, kind) => {
                    info!("# ItemKind::Use: {:?}", item.vis_span);
                    walk_use(ctx, hir_id, path, kind)
                }
                ItemKind::Mod(rustc_hir::Mod { item_ids, .. }) => {
                    info!("# ItemKind::Mod: {:?}", item.vis_span);
                    walk_mod(ctx, hir_id, item_ids)
                }
                _ => {
                    debug!("skip import kind: {:?}", item.kind);
                    None
                }
            }
        })
    }

    fn walk_mod(ctx: &ProcessContext, hir_id: HirId, item_ids: &[rustc_hir::ItemId]) -> Option<Vec<ImportConfig>> {
        let defpath = ctx.def_path(&hir_id.owner.to_def_id());
        debug!("mod.decl.defpath: {:?}", defpath);

        if is_prelude_mod(&defpath) {
            info!("skip: (mod.decl) prelude module");
            return None;
        }

        let module = ModuleDecl {
            path: super::use_path_from_defpath(&defpath),
            generics: None,
            krate: Some(ctx.crate_name_of(defpath.krate)),
        };

        walk_mod_internal(ctx, hir_id, item_ids, &module, &mut DefIdSet::new())
    }

    fn walk_mod_internal(ctx: &ProcessContext, hir_id: HirId, item_ids: &[rustc_hir::ItemId], module: &ModuleDecl, visited: &mut DefIdSet) -> Option<Vec<ImportConfig>> {
        if Visibility::<DefId>::Public != ctx.visibility(&hir_id.owner.to_def_id()) {
            info!("skip: private mod");
            return None;
        }

        let configs = item_ids.into_iter()
            .filter_map(|item_id| {
                let child_item = ctx.item_of(*item_id);
                debug!("child_item's parent: {:?}", ctx.parent_module_of(child_item.hir_id()));

                let def_id = hir_id.owner.to_def_id();

                match child_item.kind {
                    ItemKind::Use(path, kind) => {
                        walk_use_internal(ctx, child_item.hir_id(), path, kind, module, visited)
                    }
                    ItemKind::Mod(rustc_hir::Mod { item_ids, .. }) => {
                        if ! visited.insert(def_id) { return None; }

                        debug!("# Mod import from Mod: {:?}", child_item.ident);
                        
                        let child_module_path = match &module.path {
                            Some(path) => format!("{path}::{}", child_item.ident.as_str()),
                            None => child_item.ident.as_str().to_string(),
                        };
                        let module = ModuleDecl { 
                            path: Some(child_module_path), 
                            generics: None,
                            krate: module.krate.clone(),
                        };

                        walk_mod_internal(ctx, child_item.hir_id(), item_ids, &module, visited)
                    }
                    _ => None
                }
            })
            .flatten()
            .collect::<Vec<_>>()
        ;
        return Some(configs);
    }

    fn walk_use(ctx: &ProcessContext, hir_id: HirId, path: &rustc_hir::UsePath, kind: UseKind) -> Option<Vec<ImportConfig>> {
        let defpath = ctx.def_path(&hir_id.owner.to_def_id());
        debug!("use.decl.defpath: {:?}", defpath);

        if is_prelude_mod(&defpath) {
            info!("skip: (use.decl) prelude module");
            return None;
        }

        let module = ModuleDecl {
            path: super::use_path_from_defpath(&defpath),
            generics: None,
            krate: Some(ctx.crate_name_of(defpath.krate)),
        };
        
        walk_use_internal(ctx, hir_id, path, kind, &module, &mut DefIdSet::new())
    }

    fn walk_use_internal(ctx: &ProcessContext, hir_id: HirId, path: &rustc_hir::UsePath, kind: UseKind, module: &ModuleDecl, visited: &mut DefIdSet) -> Option<Vec<ImportConfig>> {
        debug!("use.decl: {kind:?}, {path:?}");
        debug!("use.decl.attrs: {:?}", ctx.attrs_of(hir_id).into_iter().map(|attr| attr.meta_item_list()).collect::<Vec<_>>());
        debug!("use.decl.module: {:?}", module);

        if Visibility::<DefId>::Public != ctx.visibility(&hir_id.owner.to_def_id()) {
            info!("skip: private import {:?}", path.span);
            return None;
        }
        
        if is_import_from_prelude(ctx.attrs_of(hir_id)) {
            info!("skip: because of prelude {:?}", path.span);
            return None;
        }

        match kind {
            UseKind::Single |
            UseKind::ListStem => {
                return walk_use_item(ctx, path.res.first(), module, visited);
            }
            UseKind::Glob => {
                return walk_glob_use(ctx, path.res.first(), module, visited);   
            }
        }
    }

    fn walk_glob_use<ResId>(ctx: &ProcessContext, res: Option<&Res<ResId>>, module: &ModuleDecl, visited: &mut DefIdSet) -> Option<Vec<ImportConfig>> 
        where ResId: std::fmt::Debug    
    {
        match res {
            Some(Res::Def(DefKind::Mod, def_id)) => {
                if def_id.is_local() { 
                    info!("skipped because import from private mod: {:?}", def_id);
                    return None; 
                }

                let children = ctx.mod_items_of(def_id);
                debug!("use.mod.child.len: {}", children.len());

                let configs = children.into_iter()
                    .filter_map(|c| {
                        debug!("use.mod.child: {c:?}");
                        if c.vis != Visibility::Public { return None; }

                        if let Res::Def(_, _) = c.res {
                            if let Some(mod_def_id) = c.res.mod_def_id() {
                                if mod_def_id != *def_id { return None; }

                                if let Some(mod_def_id) = c.res.opt_def_id() {
                                    info!("avoiding recursive child mod: {res:?}");
                                    if ! visited.insert(mod_def_id) { return None; }
                                }
                            }

                            info!("## Processing child import");
                            return walk_use_item(ctx, Some(&c.res), module, visited);
                        }
                        None
                    })
                    .flatten()
                    .collect::<Vec<_>>()
                ;

                return Some(configs);
            }
            _ => {
                warn!("(todo: no entry) UseKind::Glob: {:?}", res);
                None
            }
        }     
    }

    fn walk_use_item<ResId>(ctx: &ProcessContext, res: Option<&Res<ResId>>, module: &ModuleDecl, visited: &mut DefIdSet) -> Option<Vec<ImportConfig>>
        where ResId: std::fmt::Debug
    {
        if let Some(Res::Def(kind, def_id)) = res {
            match kind {
                DefKind::Struct |
                DefKind::Union |
                DefKind::Enum |
                DefKind::Trait |
                DefKind::TyAlias => {
                    if let Some(rhs_type) = super::resolve_qualified_type_opt(ctx, &Res::Def(*kind, *def_id), &Vec::new()) {
                        debug!("use.decl.symbol: {}#", rhs_type.category);
                        debug!("use.decl.from: {}, decl: {rhs_type:?}", rhs_type.category.prefix_with(Some(&rhs_type.module)));
            
                        let lhs_type = TypeDecl {
                            category: rhs_type.category.clone(),
                            module: module.clone(),
                            deprecation: None,
                        };
                        debug!("use.decl.to: {}, decl: {lhs_type:?}", lhs_type.category.prefix_with(Some(&lhs_type.module)));
                        info!("use.decl.accepted from: {:?}, to: {:?}", rhs_type, lhs_type);

                        return Some(vec![ImportConfig::new(&ctx.root_crate(), rhs_type, lhs_type)]);
                    }
                }
                DefKind::Mod => {
                    if ! visited.insert(*def_id) {
                        info!("avoiding recursive mod: {res:?}");
                        return None;
                    }

                    let defpath = ctx.def_path(&def_id);

                    let module = ModuleDecl {
                        path: super::use_path_from_defpath(&defpath),
                        generics: None,
                        krate: module.krate.clone(),
                    };
                                
                    info!("treat mod use as glob: defpath: {defpath:?}, res: {res:?}, xlen: {}", visited.len()+1);
                    
                    let configs = walk_glob_use(ctx, res, &module, visited); 

                    return configs;
                }

                DefKind::Variant |
                DefKind::ForeignTy |
                DefKind::TraitAlias |
                DefKind::AssocTy |
                DefKind::TyParam |
                DefKind::Const |
                DefKind::ConstParam |
                DefKind::Static { .. } |
                DefKind::Ctor(_, _) |
                DefKind::AssocFn |
                DefKind::AssocConst |
                DefKind::Macro(_) |
                DefKind::ExternCrate |
                DefKind::Use |
                DefKind::ForeignMod |
                DefKind::AnonConst |
                DefKind::InlineConst |
                DefKind::OpaqueTy |
                DefKind::Field |
                DefKind::LifetimeParam |
                DefKind::GlobalAsm |
                DefKind::Impl { .. } |
                DefKind::Fn |
                DefKind::Closure => {
                    warn!("todo: not implemented variants: {:?}, res: {:?}", kind, res);
                }
            }
        }
        None
    }
}