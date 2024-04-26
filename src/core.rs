
use rustc_middle::ty::TyCtxt;

use rustc_span::def_id::DefId;
use rustc_hir::def_id::CrateNum;
use rustc_hir::definitions::DefPath;
use rustc_hir::definitions::DefPathData;
use rustc_hir::hir_id::OwnerId;

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

use tracing::{info, debug, warn, trace};

type FnDecl = super::FnDecl;
type TypeDecl = super::TypeDecl;
type SymbolDecl = super::SymbolDecl;
type TypeCategory = super::TypeCategory;

type WalkResult<T> = std::result::Result<T, String>;

pub struct ProcessContext<'ctx> {
    raw_context: TyCtxt<'ctx>,
    root_crate_symbol: String,
}

impl ProcessContext<'_> {
    pub fn new<'ctx>(raw_context: TyCtxt<'ctx>, root_crate: &str) -> ProcessContext<'ctx> {
        ProcessContext { 
            raw_context, 
            root_crate_symbol: root_crate.to_string(),
        }
    }

    pub fn walk_item<'hir, F>(&self, f: F) -> Vec<FnDecl>
        where F: Fn(DefId, &Item) -> Option<Vec<FnDecl>>
    {
        let mut fns = vec![];

        for id in self.raw_context.hir().items() {
            let item = self.raw_context.hir().item(id);

            if let Some(mut fn_decl) = f(item.owner_id.def_id.to_def_id(), &item) {
                fns.append(&mut fn_decl);
            }
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
        ctx.walk_item(|_def_id, item| {
            match item.kind {
                ItemKind::Impl(impl_def) if impl_def.items.len() > 0 => {
                    debug!("impl_def: {:?}", impl_def);

                    let owner = 
                        walk_impl_owner(ctx, &impl_def)
                        .map_err(|err| warn!(err))
                        .ok()
                    ;
                    info!("Owner type: {:?}", owner);

                    return Some(impl_def.items.iter()
                        .filter_map(|item| walk_impl_item(ctx, &ctx.impl_item(item.id), &owner))
                        .collect::<Vec<_>>()
                    );
                }
                ItemKind::Fn(FnSig { decl, .. }, generics, _) => {
                    let defpath = ctx.def_path(&item.owner_id.to_def_id());
                    let owner_crate = ctx.crate_name_of(defpath.krate);

                    let owner = TypeDecl {
                        symbol: symbol_from_defpath(&defpath, &vec![]),
                        qual_symbol: format!("{}::{}", owner_crate, qual_symbol_from_defpath(&defpath)),
                        crate_symbol: Some(owner_crate),
                        type_category: TypeCategory::Nominal,
                        deprecation: None,
                    };

                    debug!("Global function of {}", owner.qual_symbol);

                    let prototype_name = format_prototype_name(item.ident.as_str(), &generics);
                    let fn_decl = walk_function_item(ctx, item.owner_id, decl, &prototype_name, &Some(owner));
                    
                    return fn_decl.map(|x| vec![x])
                }

                ItemKind::Mod(_) |
                ItemKind::ForeignMod { .. } |
                ItemKind::Use(_, _) |
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
                ItemKind::TyAlias(_, _) |
                ItemKind::OpaqueTy(_) => {
                    warn!("todo: {:?}", item.kind);
                }
            }
            None    
        })
    }
}

fn walk_impl_owner(ctx: &ProcessContext, impl_def: &rustc_hir::Impl) -> WalkResult<TypeDecl> {
    match impl_def.of_trait {
        Some(rustc_hir::TraitRef { path, .. }) => {
            Ok(resolve_qualified_type(ctx, &path.res, path.segments))
        }
        None => {
            walk_type_item(ctx, &impl_def.self_ty)
        }
    }
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
    let qual_symbol = format_fn_qual_symbol(proto_symbol, &args, &ret_decl);

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
            symbol: proto_symbol.to_string(),
            qual_symbol: match owner {
                Some(owner) => format!("{}::{}", owner.qual_symbol, qual_symbol),
                None => qual_symbol,
            },
            crate_symbol: owner.as_ref().and_then(|x| x.crate_symbol.as_ref().map(|owner| owner.clone())),
            type_category: crate::TypeCategory::Nominal,
            deprecation,
        },
        ret_decl,
        args,
    };
    info!("{}", fn_decl.proto.qual_symbol);

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
            info!("(Path) retry `walk_type_item` with internal type item");
            walk_type_item(ctx, mut_ty)
        }
        TyKind::Path(QPath::Resolved(None, rustc_hir::Path {res, segments, ..})) => {
            let resolved_type = resolve_qualified_type(ctx, res, segments);
            trace!("qual_symbol: {}, crate_symbol: {:?}", resolved_type.qual_symbol, resolved_type.crate_symbol);

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
                symbol: "()".to_string(),
                qual_symbol: "()".to_string(),
                crate_symbol: None,
                type_category: TypeCategory::Tuple { members: vec![] },
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
                        symbol: format_fn_symbol("", &fn_decl.args, &fn_decl.ret_decl),
                        qual_symbol: format_fn_qual_symbol("", &fn_decl.args, &fn_decl.ret_decl),
                        crate_symbol: None,
                        type_category: TypeCategory::Function,
                        deprecation: fn_decl.proto.deprecation,
                    })
                }
                None => Err("BareFn".to_string())
            }
        }
        TyKind::TraitObject(_, _, _) => {
            warn!("TraitObject is complecated... so retired!");
            Ok(TypeDecl {
                symbol: "TraitObject".to_string(),
                qual_symbol: "TraitObject".to_string(),
                crate_symbol: None,
                type_category: TypeCategory::Trait,
                deprecation: None,
            })
        }
        TyKind::Never => {
            Ok(TypeDecl {
                symbol: "!".to_string(),
                qual_symbol: "!".to_string(),
                crate_symbol: None,
                type_category: TypeCategory::Nominal,
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
    let symbols_vecs: (Vec<_>, Vec<_>) = tup_items.iter().enumerate()
        .map(|(i, item)| {
            match walk_type_item(ctx, &item) {
                Ok(ty) => (ty.symbol, ty.qual_symbol),
                Err(err) =>  {
                    warn!("[TupleItem][{}] unresolved {}", i, err);
                    ("????".to_string(), "????".to_string())
                }
            }
        })
        .unzip()
    ;

    trace!("Tuple items: {:?}",symbols_vecs);
    
    let symbol = symbols_vecs.0.join(", ");
    let qual_symbol = symbols_vecs.1.join(", ");
    let members = symbols_vecs.0.into_iter().zip(symbols_vecs.1)
        .map(|(s, qs)| SymbolDecl { symbol: s.to_string(), qual_symbol: qs.to_string() })
        .collect::<Vec<_>>()
    ;

    Ok(TypeDecl {
        symbol: format!("({})", symbol),
        qual_symbol: format!("({})", qual_symbol),
        crate_symbol: None,
        type_category: TypeCategory::Tuple { members },
        deprecation: None,
    })
}

fn symbol_from_segments(segments: &[rustc_hir::PathSegment]) -> String {
    segments.last()
        .map(|seg| seg.ident.name.to_ident_string())
        .unwrap_or("".to_string())
}

fn qual_symbol_from_segments(segments: &[rustc_hir::PathSegment]) -> String {
    segments.iter()
        .map(|seg| seg.ident.name.as_str().to_string())
        .collect::<Vec<_>>()
        .join("::")
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

fn qual_symbol_from_defpath(defpath: &DefPath) -> String {
    defpath.data.iter().filter_map(|x| match x.data {
        DefPathData::TypeNs(ns) => Some(ns.as_str().to_string()),
        DefPathData::ValueNs(ns) => Some(ns.as_str().to_string()),
        _ => None,
    })
    .collect::<Vec<String>>()
    .join("::")
}

fn resolve_qualified_type(ctx: &ProcessContext, res: &rustc_hir::def::Res, segments: &[rustc_hir::PathSegment]) -> TypeDecl {
    match res {
        rustc_hir::def::Res::Def(_, def_id) => {
            let deprecation = ctx.lookup_deprecation(*def_id);
            debug!("(qual_type.deprecated) {:?}", deprecation);
        
            let defpath = ctx.def_path(def_id);
            let crate_name = ctx.crate_name_of(defpath.krate);
            debug!("defpath: {:?}", defpath);

            let defpath_name = qual_symbol_from_defpath(&defpath);
            let symbol = symbol_from_defpath(&defpath, segments);

            TypeDecl {
                symbol,
                qual_symbol: format!("{crate_name}::{defpath_name}"), 
                crate_symbol: Some(crate_name),
                type_category: TypeCategory::Nominal,
                deprecation,
            }
        }
        rustc_hir::def::Res::SelfTyAlias {alias_to: def_id, .. } => {
            match ctx.node_of(*def_id) {
                Node::Item(rustc_hir::Item { kind: ItemKind::Impl( rustc_hir::Impl { self_ty, .. }), .. }) => {
                    info!("(SelfTyAlias) retry `walk_type_item` for resolving type");
                    if let Ok(type_decl) = walk_type_item(ctx, self_ty) {
                        return TypeDecl{ type_category: TypeCategory::Alias("Self".to_string()), ..type_decl };
                    };
                }
                _ => {}
            }
            
            TypeDecl {
                symbol: symbol_from_segments(segments),
                qual_symbol: qual_symbol_from_segments(segments), 
                crate_symbol: None,
                type_category: TypeCategory::Nominal,
                deprecation: None,
            }
        }
        rustc_hir::def::Res::PrimTy(ty) => {
            TypeDecl {
                symbol: ty.name_str().to_string(),
                qual_symbol: ty.name_str().to_string(),
                crate_symbol: None,
                type_category: TypeCategory::Nominal,
                deprecation: None,
            }
        }
        rustc_hir::def::Res::Err if segments.len() > 0 => {
            TypeDecl {
                symbol: symbol_from_segments(segments),
                qual_symbol: qual_symbol_from_segments(segments),
                crate_symbol: None,
                type_category: TypeCategory::Nominal,
                deprecation: None,
            }
        }
        _ => {
            let symbol = format!("***{:?}***", res);
            TypeDecl {
                symbol: symbol.to_string(),
                qual_symbol: symbol.to_string(),
                crate_symbol: None,
                type_category: TypeCategory::Nominal,
                deprecation: None,
            }
        }
    }
}

fn format_prototype_name(prototype_name: &str, generics: &Generics) -> String {
    let generic_param_names = 
        generics.params.into_iter().filter_map(|p| match p.name {
            rustc_hir::ParamName::Plain(param_name) => Some(param_name.as_str().to_string()),
            _ => None,
        })
        .collect::<Vec<_>>()
    ;

    match generic_param_names.len() == 0 {  
        true => prototype_name.to_string(),
        false => format!("{}<{}>", 
            prototype_name.to_string(),
            generic_param_names.join(",")
        )
    }
}

fn format_fn_symbol(prototype_name: &str, args: &[TypeDecl], ret_decl: &Option<TypeDecl>) -> String {
    let formatted_args = args.iter().map(|arg| arg.symbol.to_string()).collect::<Vec<_>>();
    let formatted_ret = ret_decl.as_ref().map(|ret| format!("-> {}", &ret.symbol)).unwrap_or("".to_string());

    format!("{}({}) {}", prototype_name, formatted_args.join(", "), formatted_ret)
}

fn format_fn_qual_symbol(prototype_name: &str, args: &[TypeDecl], ret_decl: &Option<TypeDecl>) -> String {
    let formatted_args = args.iter().map(|arg| arg.qual_symbol.to_string()).collect::<Vec<_>>();
    let formatted_ret = ret_decl.as_ref().map(|ret| format!("-> {}", &ret.qual_symbol)).unwrap_or("".to_string());

    format!("{}({}) {}", prototype_name, formatted_args.join(", "), formatted_ret)
}
