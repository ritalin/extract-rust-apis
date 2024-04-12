
use rustc_middle::ty::TyCtxt;

use rustc_span::def_id::DefId;
use rustc_hir::def_id::CrateNum;
use rustc_hir::definitions::DefPath;

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


// use rustc_hir::ImplItemKind;

use tracing::{info, debug, warn, trace};

type FnDecl = super::FnDecl;
type TypeDecl = super::TypeDecl;

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

                ItemKind::Fn(_, _, _) |
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
            let (qual_symbol, crate_symbol) = resolve_qualified_type(ctx, &path.res, path.segments);

            Ok(TypeDecl {
                symbol: pick_type_symbol(path.segments),
                qual_symbol,
                crate_symbol,
            })
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
            
            return walk_function_item(ctx, decl, impl_item.ident.name.as_str(), owner);
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

fn walk_function_item(ctx: &ProcessContext, decl: &rustc_hir::FnDecl, proto_symbol: &str, owner: &Option<TypeDecl>) -> Option<FnDecl> {
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

    let fn_decl = FnDecl {
        proto: TypeDecl {
            symbol: proto_symbol.to_string(),
            qual_symbol: match owner {
                Some(owner) => format!("{}::{}", owner.qual_symbol, qual_symbol),
                None => qual_symbol,
            },
            crate_symbol: owner.as_ref().and_then(|x| x.crate_symbol.as_ref().map(|owner| owner.clone())),
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
            let (qual_symbol, crate_symbol) = resolve_qualified_type(ctx, res, segments);
            trace!("qual_symbol: {}, crate_symbol: {:?}", qual_symbol, crate_symbol);

            Ok(TypeDecl {
                symbol: pick_type_symbol(segments),
                qual_symbol,
                crate_symbol,
            })
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
            match walk_function_item(ctx, fn_decl, "", &None) {
                Some(fn_decl) => {
                    Ok(TypeDecl {
                        symbol: format_fn_symbol("", &fn_decl.args, &fn_decl.ret_decl),
                        qual_symbol: format_fn_qual_symbol("", &fn_decl.args, &fn_decl.ret_decl),
                        crate_symbol: None,
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
            })
        }

        TyKind::InferDelegation(_, _) => Err("InferDelegation".to_string()),
        TyKind::Never => Err("Never".to_string()),
        TyKind::AnonAdt(_) => Err("AnonAdt".to_string()),
        TyKind::OpaqueDef(_, _, _) => Err("OpaqueDef".to_string()),
        TyKind::Typeof(_) => Err("Typeof".to_string()),
        TyKind::Infer => Err("Infer".to_string()),
        TyKind::Err(_) => Err("Err".to_string()),
        TyKind::Path(p) => Err(format!("another Path {:?}", p).to_string()),
    }
}

fn walk_tuple_items(ctx: &ProcessContext, tup_items: &[rustc_hir::Ty]) -> WalkResult<TypeDecl> {
    let symbols: (Vec<_>, Vec<_>) = tup_items.iter().enumerate()
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

    trace!("Tuple items: {:?}",symbols);

    Ok(TypeDecl {
        symbol: format!("({})", symbols.0.join(", ")),
        qual_symbol: format!("({})", symbols.1.join(", ")),
        crate_symbol: None,
    })
}

fn pick_type_symbol(segments: &[rustc_hir::PathSegment]) -> String {
    segments.last()
        .map(|seg| seg.ident.name.to_ident_string())
        .unwrap_or("????".to_string())
}

fn resolve_qualified_type(ctx: &ProcessContext, res: &rustc_hir::def::Res, segments: &[rustc_hir::PathSegment]) -> (String, Option<String>) {
    match res {
        rustc_hir::def::Res::Def(_, def_id) |
        rustc_hir::def::Res::SelfTyAlias {alias_to: def_id, .. } => {
            let defpath = ctx.def_path(def_id);
            let crate_name = ctx.crate_name_of(defpath.krate);
            trace!("defpath: {:?}", defpath);

            let defpath_name = 
                defpath.data.iter().filter_map(|x| match x.data {
                    rustc_hir::definitions::DefPathData::TypeNs(ns) => Some(ns.as_str().to_string()),
                    _ => None,
                })
                .collect::<Vec<String>>()
                .join("::")
            ;

            (format!("{crate_name}::{defpath_name}"), Some(crate_name))
        }
        rustc_hir::def::Res::PrimTy(ty) => {
            (ty.name_str().to_string(), None)
        }
        rustc_hir::def::Res::Err if segments.len() > 0 => {
            (segments[0].ident.as_str().to_string(), None)
        }
        _ => (format!("***{:?}***", res), None)
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
