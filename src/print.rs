use std::path::{PathBuf, Path};
use serde::{Serialize};
use tracing::{error};


type FnDecl = super::FnDecl;
type TypeDecl = super::TypeDecl;

#[derive(Debug, Serialize)]
pub enum TypeRelKind {
    Return, 
    Arg,
}

#[derive(Debug, Serialize)]
pub struct TypeRel {
    kind: TypeRelKind,
    parent: TypeDecl,
    child: TypeDecl,
}

pub struct PrintHandler {
    output_dir: String,
    prety_print: bool,
}

impl PrintHandler {
    pub fn new(output_dir: &str, prety_print: bool) -> Self {
        PrintHandler {
            output_dir: output_dir.to_string(),
            prety_print,
        }
    }

    pub fn handle_print(&self, functions: Vec<FnDecl>) {
        let prototypes = functions.iter().map(|f| Some(f.proto.clone()));
        let returns = functions.iter().filter_map(|f| f.ret_decl.as_ref().map(|ret| TypeRel { kind: TypeRelKind::Return, parent: f.proto.clone(), child: ret.clone() }));
        let parameters = functions.iter().flat_map(|f| f.args.iter().map(|p| TypeRel { kind: TypeRelKind::Arg, parent: f.proto.clone(), child: p.clone() }));
        
        let prototype_path: PathBuf = [self.output_dir.to_string(), "prototype.json".to_string()].iter().collect();
        let type_path: PathBuf = [self.output_dir.to_string(), "type.json".to_string()].iter().collect();
        
        {
            let ser = if self.prety_print { serde_json::to_string_pretty } else { serde_json::to_string };

            match ser(&prototypes.collect::<Vec<_>>()) {
                Ok(json) => export_to(&prototype_path, json),
                Err(err) => error!("Can not export to prototype.json: {}", err),
            };
        }

        {
            let ser = if self.prety_print { serde_json::to_string_pretty } else { serde_json::to_string };

            match ser(&returns.chain(parameters).collect::<Vec<_>>()) {
                Ok(json) => export_to(&type_path, json),
                Err(err) => error!("Can not export to type.json: {}", err),
            }
        }
    }
}

fn export_to(path: &Path, value: String) {
    if let Some(dir) = path.parent() {
        match dir.exists() {
            false => std::fs::DirBuilder::new().recursive(true).create(dir).unwrap(),
            _ => {}
        }
    }

    use std::io::Write;
    let f = std::fs::File::create(path).expect(&format!("[Error] Failed to create filr: {}", path.display()));
    let mut writer = std::io::BufWriter::new(f);
    let _ = writer.write_all(value.as_bytes());
    let _ = writer.flush();
}
