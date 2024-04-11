



use tracing_subscriber::{filter, prelude::*};



fn main() {
    let stdout_log = tracing_subscriber::fmt::layer().compact().without_time().with_target(false);
    tracing_subscriber::registry()
    .with(stdout_log.with_filter(filter::LevelFilter::INFO))
    .init();

    fndump::compile::run("std", fndump::core::ProcessHandler{});


//                 for item_id in ctx.hir().items() {
//                     let item = ctx.hir().item(item_id);

//                     match item.kind {
//                         ItemKind::Impl(impl_def) if impl_def.items.len() > 0 => {
//                             println!("[DEBUG] impl_def: {:?}", impl_def);
//                             println!("[DEBUG] vis: {:?}", ctx.local_visibility(impl_def.self_ty.hir_id.owner.def_id));
//                             let owner = walk_type(&ctx, root_crate_name, &impl_def.self_ty).ok();
//                             println!("[INFO] Type: {:?}", owner);

//                             impl_fns.append(
//                                 &mut impl_def.items.iter()
//                                 .filter_map(|item| walk_impl(&ctx, root_crate_name, &item, &owner))
//                                 .collect::<Vec<_>>()
//                             );
//                         }
//                         _ => println!("[Todo] skipped: {:?}", item.kind),
//                     }
//                 }

//                 let prototypes = impl_fns.iter().map(|f| Some(f.proto.clone()));
//                 let returns = impl_fns.iter().filter_map(|f| f.ret_decl.as_ref().map(|ret| TypeRelEdge { kind: TypeRelKind::Return, parent: f.proto.clone(), child: ret.clone() }));
//                 let parameters = impl_fns.iter().flat_map(|f| f.args.iter().map(|p| TypeRelEdge { kind: TypeRelKind::Arg, parent: f.proto.clone(), child: p.clone() }));
                
//                 match serde_json::to_string(&prototypes.collect::<Vec<_>>()) {
//                     Ok(json) => export_to(Path::new("./exp/prototype.json"), json),
//                     Err(err) => eprintln!("[Error] Can not export to prototype.json: {}", err),
//                 };
//                 match serde_json::to_string(&returns.chain(parameters).collect::<Vec<_>>()) {
//                     Ok(json) => export_to(Path::new("./exp/type.json"), json),
//                     Err(err) => eprintln!("[Error] Can not export to type.json: {}", err),
//                 }
//             });
//         });
//     })
// }

// fn walk_impl(ctx: &rustc_middle::ty::TyCtxt, root_crate_name: &str, item: &ImplItemRef, owner: &Option<TypeDecl>) -> Option<FunctionDecl>  {
//     let impl_item = ctx.hir().impl_item(item.id);

//     match impl_item.kind {
//         ImplItemKind::Fn(FnSig{ decl, span, .. }, _) => {
//             println!("[INFO] Function: {:?}", item.id);
//             println!("[INFO] Function Decl: {:?}", decl);
            
//             let ret_decl = match walk_return_type(ctx, root_crate_name, &decl.output) {
//                 Ok(ty) => Some(ty),
//                 Err(err) => {
//                     println!("[SKIP] fn.return {}", err);
//                     None
//                 }
//             };
//             let args = decl.inputs.iter().enumerate()
//                 .filter_map(|(i, p)| match walk_type(ctx, root_crate_name, &p) {
//                     Ok(ty) => Some(ty),
//                     Err(err) => {
//                         println!("[SKIP] fn.param[{}] {}", i, err);
//                         None
//                     }
//                 })
//                 .collect::<Vec<TypeDecl>>()
//             ;
//             let qual_symbol = format_fn_qual_symbol(&impl_item.ident.name.to_ident_string(), &args, &ret_decl);

//             return Some(FunctionDecl {
//                 proto: TypeDecl {
//                     id: item.id.owner_id.def_id.to_def_id(),
//                     symbol: impl_item.ident.name.to_ident_string(),
//                     qual_symbol: match owner {
//                         Some(owner) => {
//                             let s = format!("{}::{}", owner.qual_symbol, qual_symbol);
//                             println!("{}", s);
//                             s
//                         }
//                         None => qual_symbol,
//                     },
//                     crate_symbol: owner.as_ref().and_then(|x| x.crate_symbol.as_ref().map(|owner| owner.clone())),
//                 },
//                 owner: owner.as_ref().map(|owner| owner.clone()),
//                 ret_decl,
//                 args,
//             });
//         }
//         ImplItemKind::Const(_, _) => {
//             println!("[SKIP] Impl Const");
//         }
//         ImplItemKind::Type(_) => {
//             println!("[SKIP] Impl Type");
//         }
//     }

//     None
// }

// fn walk_return_type(ctx: &rustc_middle::ty::TyCtxt, root_crate_name: &str, type_item: &FnRetTy) -> Result<TypeDecl, String> {
//     match type_item {
//         FnRetTy::Return(ty) => walk_type(ctx, root_crate_name, &ty),
//         FnRetTy::DefaultReturn(_) => Err("fn.return: (none)".to_string()),
//     }
// }

// fn walk_type(ctx: &rustc_middle::ty::TyCtxt, root_crate_name: &str, type_item: &rustc_hir::Ty) -> Result<TypeDecl, String> {
//     match type_item.kind {
//         TyKind::Path(QPath::Resolved(Some(mut_ty), rustc_hir::Path {..})) => {
//             walk_type(ctx, root_crate_name, mut_ty)
//         }
//         TyKind::Path(QPath::Resolved(None, rustc_hir::Path {res, segments, ..})) => {
//             // if let rustc_hir::def::Res::Def(_, did) = res {
//             //     let defpath = ctx.def_path(*did);
//             //     println!("[DEBUG] DefPath: {:?}, Ctrate: {:?} (index: {})", defpath, ctx.crate_name(defpath.krate), defpath.krate.index());
//             //     println!("[DEBUG] DefPath Str: {}{}", ctx.crate_name(defpath.krate), defpath.to_string_no_crate_verbose());
//             // }
//             let (qual_symbol, crate_symbol) = resolve_qualified_type(ctx, res, segments, root_crate_name);
//             println!("[DEBUG] qual_symbol: {}, crate_symbol: {:?}", qual_symbol, crate_symbol);

//             Ok(TypeDecl {
//                 id: type_item.hir_id.owner.def_id.to_def_id(),
//                 symbol: segments.last().map(|seg| seg.ident.name.to_ident_string()).unwrap_or("????".to_string()),
//                 qual_symbol,
//                 crate_symbol,
//             })
//         }
//         TyKind::Ref(_, mut_ty) => {
//             walk_type(ctx, root_crate_name, mut_ty.ty)
//         }
//         TyKind::Tup(tup_items) if tup_items.len() == 0 => {
//             Ok(TypeDecl {
//                 id: type_item.hir_id.owner.def_id.to_def_id(),
//                 symbol: "()".to_string(),
//                 qual_symbol: "()".to_string(),
//                 crate_symbol: None,
//             })
//         }
//         TyKind::Tup(tup_items) => {
//             Ok(walk_tuple_items(ctx, root_crate_name, type_item.hir_id, &tup_items))
//         }

//         TyKind::InferDelegation(_, _) => Err("InferDelegation".to_string()),
//         TyKind::Slice(_) => Err("Slice".to_string()),
//         TyKind::Array(_, _) => Err("Array".to_string()),
//         TyKind::Ptr(_) => Err("Ptr".to_string()),
//         TyKind::BareFn(_) => Err("BareFn".to_string()),
//         TyKind::Never => Err("Never".to_string()),
//         TyKind::AnonAdt(_) => Err("AnonAdt".to_string()),
//         TyKind::OpaqueDef(_, _, _) => Err("OpaqueDef".to_string()),
//         TyKind::TraitObject(_, _, _) => Err("TraitObject".to_string()),
//         TyKind::Typeof(_) => Err("Typeof".to_string()),
//         TyKind::Infer => Err("Infer".to_string()),
//         TyKind::Err(_) => Err("Err".to_string()),
//         TyKind::Path(p) => Err(format!("another Path {:?}", p).to_string()),
//     }
// }

// fn walk_tuple_items(ctx: &rustc_middle::ty::TyCtxt, root_crate_name: &str, id: rustc_hir::HirId, tup_items: &[rustc_hir::Ty]) -> TypeDecl {
//     let symbols: (Vec<_>, Vec<_>) = tup_items.iter().enumerate().map(|(i, item)| {
//             match walk_type(ctx, root_crate_name, &item) {
//                 Ok(ty) => (ty.symbol, ty.qual_symbol),
//                 Err(err) => {
//                     println!("[TupleItem][{}] unresolved {}", i, err);
//                     ("????".to_string(), "????".to_string())
//                 }
//             }
//         })
//         .unzip();

//     println!("[DEBUG] Tuple items: {:?}",symbols);

//     TypeDecl {
//         id: id.owner.def_id.to_def_id(),
//         symbol: format!("({})", symbols.0.join(", ")),
//         qual_symbol: format!("({})", symbols.1.join(", ")),
//         crate_symbol: None,
//     }
// }

// fn resolve_qualified_type(ctx: &rustc_middle::ty::TyCtxt, res: &rustc_hir::def::Res, segments: &[rustc_hir::PathSegment], root_crate_name: &str) -> (String, Option<String>) {
//     match res {
//         rustc_hir::def::Res::Def(_, def_id) |
//         rustc_hir::def::Res::SelfTyAlias {alias_to: def_id, .. } => {
//             let defpath = ctx.def_path(*def_id);
//             let crate_name = match defpath.krate.index() {
//                 0 => root_crate_name.to_string(),
//                 _ => ctx.crate_name(defpath.krate).to_string(),
//             };
//             println!("[DEBUG] Self defpath: {:?}", defpath);
//             let defpath_name = 
//                 defpath.data.iter().filter_map(|x| match x.data {
//                     rustc_hir::definitions::DefPathData::TypeNs(ns) => Some(ns.as_str().to_string()),
//                     _ => None,
//                 })
//                 .collect::<Vec<String>>()
//                 .join("::")
//             ;

//             (format!("{crate_name}::{defpath_name}"), Some(crate_name))
//         }
//         rustc_hir::def::Res::PrimTy(ty) => {
//             (ty.name_str().to_string(), None)
//         }
//         rustc_hir::def::Res::Err if segments.len() > 0 => {
//             (segments[0].ident.as_str().to_string(), None)
//         }
//         // _ => "????".to_string(),
//         _ => (format!("***{:?}***", res), None)
//     }
// }

// fn export_to(path: &Path, value: String) {
//     if let Some(dir) = path.parent() {
//         match dir.exists() {
//             false => std::fs::DirBuilder::new().recursive(true).create(dir).unwrap(),
//             _ => {}
//         }
//     }

//     use std::io::Write;
//     let f = std::fs::File::create(path).expect(&format!("[Error] Failed to create filr: {}", path.display()));
//     let mut writer = std::io::BufWriter::new(f);
//     let _ = writer.write_all(value.as_bytes());
//     let _ = writer.flush();
// }

// fn format_fn_qual_symbol(prototype_name: &str, args: &[TypeDecl], ret_decl: &Option<TypeDecl>) -> String {
//     let formatted_args = args.iter().map(|arg| arg.symbol.to_string()).collect::<Vec<_>>();
//     let formatted_ret = ret_decl.as_ref().map(|ret| format!("-> {}", &ret.symbol)).unwrap_or("".to_string());

//     format!("{}({}) {}", prototype_name, formatted_args.join(", "), formatted_ret)
}