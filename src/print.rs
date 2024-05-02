use std::path::{PathBuf, Path};
use serde::Serialize;
use tracing::error;


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
    crate_symbol: String,
    output_dir: String,
    prety_print: bool,
}

impl PrintHandler {
    pub fn new(crate_symbol: &str, output_dir: &str, prety_print: bool) -> Self {
        PrintHandler {
            crate_symbol: crate_symbol.to_string(),
            output_dir: output_dir.to_string(),
            prety_print,
        }
    }

    pub fn handle_print(&self, functions: Vec<FnDecl>) {
        let file_path: PathBuf = [self.output_dir.to_string(), format!("fn_decl_{}.json", self.crate_symbol)].iter().collect();
        let ser = if self.prety_print { serde_json::to_string_pretty } else { serde_json::to_string };

        match ser(&functions) {
            Ok(json) => export_to(&file_path, json),
            Err(err) => error!("Can not export to prototype.json: {}", err),
        };
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
