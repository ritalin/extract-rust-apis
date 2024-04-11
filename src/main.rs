



use tracing_subscriber::{filter, prelude::*};

fn main() {
    let stdout_log = tracing_subscriber::fmt::layer().compact()
        .without_time()
        .with_ansi(false)
        .with_target(false)
    ;
    tracing_subscriber::registry()
    .with(stdout_log.with_filter(filter::LevelFilter::DEBUG))
    .init();

    fndump::compile::run("std", fndump::core::ProcessHandler{});





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
}