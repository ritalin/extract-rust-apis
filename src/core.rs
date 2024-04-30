
use rustc_hir::PathSegment;
use rustc_middle::ty::TyCtxt;

use rustc_span::def_id::DefId;
use rustc_hir::def_id::CrateNum;
use rustc_hir::definitions::DefPath;
use rustc_hir::definitions::DefPathData;
use rustc_hir::hir_id::OwnerId;

use rustc_hir::HirId;
use rustc_hir::Node;
use rustc_hir::Item;
use rustc_hir::ItemKind;
use rustc_hir::Ty;
use rustc_hir::TyKind;
use rustc_hir::QPath;
use rustc_hir::ImplItemId;
use rustc_hir::ImplItem;
use rustc_hir::ImplItemKind;
use rustc_hir::FnSig;
use rustc_hir::FnRetTy;
use rustc_hir::Generics;
use rustc_hir::GenericArg;

use tracing::{info, debug, warn, trace};

use crate::ModuleDecl;

type FnDecl = super::FnDecl;
type TypeDecl = super::TypeDecl;
type TypeCategory = super::TypeCategory;

type WalkResult<T> = std::result::Result<T, String>;

pub struct ProcessContext<'ctx> {
    raw_context: TyCtxt<'ctx>,
    root_crate_symbol: String,
}

pub enum WalkOutput {
    Fn(Vec<FnDecl>),
    TypeAlias { target: String, alias_decl: TypeDecl },
    None,
}

impl ProcessContext<'_> {
    pub fn new<'ctx>(raw_context: TyCtxt<'ctx>, root_crate: &str) -> ProcessContext<'ctx> {
        ProcessContext { 
            raw_context, 
            root_crate_symbol: root_crate.to_string(),
        }
    }

    pub fn walk_item<'hir, F>(&self, f: F) -> Vec<FnDecl>
        where F: Fn(HirId, DefId, &Item) -> WalkOutput
    {
        let mut fns = vec![];

        for id in self.raw_context.hir().items() {
            let item = self.raw_context.hir().item(id);

            match f(id.hir_id(), item.owner_id.def_id.to_def_id(), &item) {
                WalkOutput::Fn(mut decls) => {
                    fns.append(&mut decls);
                }
                WalkOutput::TypeAlias { target: _target, alias_decl: _alias_decl } => {

                },
                WalkOutput::None => {},
            };
        };

        fns
    }

    pub fn is_hidden_item(&self, def_id: DefId) -> bool {
        !self.raw_context.visibility(def_id).is_public()
    }

    pub fn node_of(&self, def_id: DefId) -> Node {
        self.raw_context.hir_node_by_def_id(def_id.expect_local())
    }

    pub fn crate_name_of(&self, krate: CrateNum) -> String {
        match krate.index() {
            0 => self.root_crate_symbol.to_string(),
            _ => self.raw_context.crate_name(krate).to_string(),
        }
    }

    pub fn parent_hir_id(&self, hir_id: HirId) -> HirId {
        self.raw_context.parent_hir_id(hir_id)
    }

    pub fn def_path(&self, def_id: &DefId) -> DefPath {
        self.raw_context.def_path(*def_id)
    }

    pub fn impl_item(&self, id: ImplItemId) -> &ImplItem {
        self.raw_context.hir().impl_item(id)
    }

    pub fn lookup_deprecation(&self, def_id: DefId) -> Option<rustc_attr::Deprecation> {
        self.raw_context.lookup_deprecation(def_id)
    }
}

pub struct ProcessHandler;

impl ProcessHandler {
    pub fn handle_extract(&self, ctx: &ProcessContext) -> Vec<FnDecl> {
        ctx.walk_item(|hir_id, _def_id, item| {
            match item.kind {
                ItemKind::Impl(impl_def) if impl_def.items.len() > 0 => {
                    debug!("impl_def: {:?}", impl_def);

                    let owner = 
                        walk_impl_owner(ctx, &impl_def)
                        .map_err(|err| warn!(err))
                        .ok()
                    ;
                    info!("Owner type: {:?}", owner);

                    return WalkOutput::Fn(impl_def.items.iter()
                        .filter_map(|item| walk_impl_item(ctx, &ctx.impl_item(item.id), &owner))
                        .collect::<Vec<_>>()
                    );
                }
                ItemKind::Fn(FnSig { decl, .. }, generics, _) => {
                    let parent_id = ctx.parent_hir_id(hir_id);
                    let defpath = ctx.def_path(&parent_id.owner.to_def_id());
                    let owner_crate = ctx.crate_name_of(defpath.krate);

                    let owner = TypeDecl {
                        category: TypeCategory::Symbol(symbol_from_defpath(&defpath, &vec![])),
                        module: ModuleDecl { path: mod_from_defpath(&defpath), krate: Some(owner_crate) },
                        deprecation: None,
                    };

                    debug!("Global function (name: {}, mod: {:?})", owner.category, owner.module);

                    let prototype_name = format_prototype_name(item.ident.as_str(), &generics);
                    let fn_decl = walk_function_item(ctx, item.owner_id, decl, &prototype_name, &Some(owner));
                    
                    return match fn_decl {
                        Some(x) => WalkOutput::Fn(vec![x]),
                        None => WalkOutput::None,
                    };
                }
                ItemKind::TyAlias(ty, _) => {
                    debug!("type_alias.decl: {:?}", ty);

                    if let Ok(ref decl) = walk_type_item(ctx, &ty) {
                        let lhs_parent_id = ctx.parent_hir_id(hir_id);
                        let lhs_parent_defpath = ctx.def_path(&lhs_parent_id.owner.to_def_id());
                        let lhs_mod = ModuleDecl {
                            path: mod_from_defpath(&lhs_parent_defpath),
                            krate: Some(ctx.crate_name_of(lhs_parent_defpath.krate)),
                        };

                        let lhs_deprecated = ctx.lookup_deprecation(lhs_parent_id.owner.to_def_id());

                        info!("TyAlias: {} <- {}", 
                            format!("{}::{}", lhs_mod, item.ident.as_str()), 
                            decl.category.prefix_with(Some(&decl.module))
                        );

                        return WalkOutput::TypeAlias { 
                            target: decl.category.prefix_with(Some(&decl.module)), 
                            alias_decl: TypeDecl {
                                category: TypeCategory::Symbol(item.ident.as_str().to_string()),
                                module: lhs_mod,
                                deprecation: lhs_deprecated,
                            }
                        }
                    }
                    else {
                        return WalkOutput::None;
                    }
                }
                ItemKind::Use(path, kind) => {
                    debug!("use.decl.kind: {kind:?}");
                    debug!("use.decl.path: {path:?}");
                    if let Some(PathSegment { ident, hir_id, .. }) = path.segments.last() {
                        debug!("use.decl.symbol: {ident:?}");

                        if let rustc_hir::Node::Item(node_item) = ctx.node_of(hir_id.owner.to_def_id()) {
                            debug!("use.decl.node: {:?}", node_item);
                        }
                    }
                    return WalkOutput::None;
                }
                ItemKind::ExternCrate(Some(crate_symbol)) => {
                    debug!("extern_crate.decl: {crate_symbol:?}");
                    return WalkOutput::None;
                }

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
                    trace!("unsupported item: {:?}", item.kind)
                }

                ItemKind::TraitAlias(_, _) |
                ItemKind::Trait(_, _, _, _, _) |
                ItemKind::OpaqueTy(_) => {
                    warn!("todo: {:?}", item.kind);
                }
            }
            WalkOutput::None    
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

            trace!("(*1) Impl owner deprecation (trait): {:?}", decl.deprecation);
            decl.deprecation = decl.deprecation.or_else(|| ctx.lookup_deprecation(hir_ref_id.owner.to_def_id()));
            trace!("(*2) Impl owner deprecation (trait): {:?}", decl.deprecation);
            decl
        }
        None => {
            let mut decl = walk_type_item(ctx, &impl_def.self_ty)?;
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

    
    let fn_module = match owner {
        Some(TypeDecl { category: cat, module: ModuleDecl { path: Some(path), krate }, .. }) => {
            ModuleDecl {
                path: Some(format!("{}::{}", path, cat.to_string())),
                krate: krate.clone(),
            }
        }
        Some(TypeDecl { category: cat, module: ModuleDecl { path: None, krate }, .. }) => {
            ModuleDecl {
                path: Some(cat.to_string()),
                krate: krate.clone(),
            }
        }
        None => ModuleDecl::none(),
    };

    let fn_decl = FnDecl {
        proto: TypeDecl {
            category: TypeCategory::Symbol(proto_symbol.to_string()),
            module: fn_module,
            deprecation,
        },
        ret_decl,
        args,
    };
    info!("(fn.qual): {}", fn_decl);
    debug!("(fndecl): {:?}", fn_decl);

    Some(fn_decl)
}

fn walk_return_type_item(ctx: &ProcessContext, type_item: &FnRetTy) -> WalkResult<Option<TypeDecl>> {
    debug!("ret_decl: {:?}", type_item);
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
                    Ok(TypeDecl {
                        category: TypeCategory::Symbol(fn_decl.to_string()),
                        module: ModuleDecl::none(),
                        deprecation: fn_decl.proto.deprecation,
                    })
                }
                None => Err("BareFn".to_string())
            }
        }
        TyKind::TraitObject(_, _, _) => {
            warn!("TraitObject is complecated... so retired!");
            Ok(TypeDecl {
                category: crate::TypeCategory::Symbol("TraitObject".to_string()),
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
        TyKind::OpaqueDef(_, _, _) => Err("OpaqueDef".to_string()),
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
        0 | 1 => None,
        _ => Some(
            segments.iter()
                .take(segments.len()-1)
                .map(|seg| seg.ident.name.as_str().to_string())
                .collect::<Vec<_>>()
                .join("::")
        )
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
    match res {
        rustc_hir::def::Res::Def(_, def_id) => {
            let deprecation = ctx.lookup_deprecation(*def_id);
            debug!("(qual_type.deprecated) {:?}", deprecation);
        
            let defpath = ctx.def_path(def_id);
            let crate_name = ctx.crate_name_of(defpath.krate);
            debug!("defpath: {:?}", defpath);

            let mod_path = mod_from_defpath(&defpath);
            let symbol = symbol_from_defpath(&defpath, segments);

            TypeDecl {
                category: TypeCategory::Symbol(symbol),
                module: ModuleDecl {
                    path: mod_path,
                    krate: Some(crate_name),
                },
                deprecation,
            }
        }
        rustc_hir::def::Res::SelfTyAlias {alias_to: def_id, .. } => {
            match ctx.node_of(*def_id) {
                Node::Item(rustc_hir::Item { kind: ItemKind::Impl( rustc_hir::Impl { self_ty, .. }), .. }) => {
                    info!("(SelfTyAlias) retry `walk_type_item` for resolving type");
                    if let Ok(type_decl) = walk_type_item(ctx, self_ty) {
                        return TypeDecl { category: TypeCategory::SelfAlias(Box::new(type_decl.category)), ..type_decl };
                    };
                }
                _ => {}
            }
            
            TypeDecl {
                category: TypeCategory::Symbol(symbol_from_segments(segments),),
                module: ModuleDecl::none(),
                deprecation: None,
            }
        }
        rustc_hir::def::Res::PrimTy(ty) => {
            TypeDecl {
                category: TypeCategory::Symbol(ty.name_str().to_string()),
                module: ModuleDecl::none(),
                deprecation: None,
            }
        }
        rustc_hir::def::Res::Err if segments.len() > 0 => {
            TypeDecl {
                category: TypeCategory::Symbol(symbol_from_segments(segments)),
                module: ModuleDecl { path: mod_from_segments(segments), krate: None },
                deprecation: None,
            }
        }
        _ => {
            TypeDecl {
                category: TypeCategory::Symbol(format!("***{:?}***", res)),
                module: ModuleDecl::none(),
                deprecation: None,
            }
        }
    }
}

fn pick_defined_generic_params(generics: &Generics) -> Option<String> {
    let generic_param_names = 
        generics.params.into_iter().filter_map(|p| match (p.name, p.kind) {
            (_, rustc_hir::GenericParamKind::Lifetime { .. }) => None,
            (rustc_hir::ParamName::Plain(param_name), _) => {
                Some(param_name.as_str().to_string())
            }
            _ => None,
        })
        .collect::<Vec<_>>()
    ;

    match generic_param_names.len() == 0 {  
        true => None,
        false => Some(generic_param_names.join(",")),
    } 
}

fn pick_bound_generic_params(ctx: &ProcessContext, generics: Option<&rustc_hir::PathSegment>) -> Option<String> {
    match generics {
        Some(rustc_hir::PathSegment { args: Some(rustc_hir::GenericArgs { args, .. }), .. }) => {
            let bound_types = args.into_iter()
                .filter_map(|arg| match arg {
                    GenericArg::Type(ty) => walk_type_item(ctx, ty).ok(),
                    _ => None,
                })
                .map(|x| x.category.prefix_with(Some(&x.module)))
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
    match pick_defined_generic_params(generics) {  
        Some(generic_param_names) => format!("{}<{}>", prototype_name.to_string(), generic_param_names),
        None => prototype_name.to_string(),
    }
}
