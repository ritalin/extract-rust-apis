use std::{collections::BTreeSet, path::{Path, PathBuf}};
use serde::Serialize;
use tracing::error;


type FnDecl = super::FnDecl;
type TypeDecl = super::TypeDecl;

#[derive(Debug, Serialize)]
pub struct TypeDeclExport {
    lookup_key: String,
    decl: TypeDecl,
}

impl TypeDeclExport {
    pub fn new(decl: &TypeDecl) -> Self {
        TypeDeclExport {
            lookup_key: decl.make_lookup_key(),
            decl: decl.clone(),
        }
    }
}

impl PartialOrd for TypeDeclExport {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.decl.make_lookup_key().partial_cmp(&other.decl.make_lookup_key())
    }
}
impl Ord for TypeDeclExport {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.decl.make_lookup_key().cmp(&other.decl.make_lookup_key())
    }
}

impl PartialEq for TypeDeclExport {
    fn eq(&self, other: &Self) -> bool {
        self.lookup_key == other.lookup_key
    }
}
impl Eq for TypeDeclExport {}

pub struct PrintHandler {
    output_dir: PathBuf,
    prety_print: bool,
}

impl PrintHandler {
    pub fn new(crate_symbol: &str, output_dir: &str, prety_print: bool) -> Self {
        PrintHandler {
            output_dir: [output_dir, crate_symbol].iter().collect(),
            prety_print,
        }
    }

    pub fn handle_print(&self, functions: &[FnDecl], import_to: &[&crate::support::ImportConfig]) {
        handle_print_functions(&functions, self.output_dir.join("fn_decl.json"), self.prety_print);
        handle_print_types(&functions, import_to, self.output_dir.join("type_decl.json"), self.prety_print);
        handle_print_imports(&import_to, self.output_dir.join("import_map.json"), self.prety_print);
    }
}

fn handle_print_functions(functions: &[FnDecl], file_path: PathBuf, pp: bool) {
    let ser = if pp { serde_json::to_string_pretty } else { serde_json::to_string };

    match ser(&functions) {
        Ok(json) => {
            let _ = export_to(&file_path, json);
        }
        Err(err) => error!("Can not export to {}: {err}", file_path.display()),
    };
}

fn handle_print_types(functions: &[FnDecl], import_to: &[&crate::support::ImportConfig], file_path: PathBuf, pp: bool) {
    let type_decls = 
        functions.into_iter()
        .flat_map(|f| f.args.clone())
        .chain(functions.into_iter().filter_map(|f| f.ret_decl.clone()))
        .chain(functions.into_iter().filter_map(|f| f.owner.clone()))
        .chain(import_to.into_iter().map(|config| config.to_type.clone()))
        .map(|ref decl| TypeDeclExport::new(decl))
        .collect::<BTreeSet<TypeDeclExport>>()
    ;

    let ser = if pp { serde_json::to_string_pretty } else { serde_json::to_string };

    match ser(&type_decls) {
        Ok(json) => {
            let _ = export_to(&file_path, json);
        }
        Err(err) => error!("Can not export to {}: {err}", file_path.display()),
    };
}

fn handle_print_imports(import_to: &[&crate::support::ImportConfig], file_path: PathBuf, pp: bool) {
    #[derive(Serialize)]
    struct ImportMap {
        crate_symbol: String,
        from_type: String,
        to_type: String,
    }

    let maps = import_to.into_iter()
        .map(|config| {
            ImportMap {
                crate_symbol: config.crate_symbol.clone(),
                from_type: config.from_type.make_lookup_key(),
                to_type: config.to_type.make_lookup_key(),
            }
        })
        .collect::<Vec<_>>()
    ;

    let ser = if pp { serde_json::to_string_pretty } else { serde_json::to_string };

    match ser(&maps) {
        Ok(json) => {
            let _ = export_to(&file_path, json);
        }
        Err(err) => error!("Can not export to {}: {err}", file_path.display()),
    };
}

fn export_to(path: &Path, value: String) -> std::io::Result<()> {
    if let Some(dir) = path.parent() {
        match dir.exists() {
            false => std::fs::DirBuilder::new().recursive(true).create(dir).unwrap(),
            _ => {}
        }
    }

    use std::io::Write;
    let f = std::fs::File::create(path)?;
    let mut writer = std::io::BufWriter::new(f);
    writer.write_all(value.as_bytes())?;
    writer.flush()?;

    Ok(())
}
