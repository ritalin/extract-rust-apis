

use std::path::PathBuf;

use rustc_interface::interface;
use rustc_session::config;
use rustc_hash::{FxHashMap};

// use rustc_hir::ItemKind;
// use rustc_hir::ImplItemKind;
// use rustc_hir::ImplItemRef;
// use rustc_hir::FnSig;
// use rustc_hir::FnRetTy;
// use rustc_hir::TyKind;
// use rustc_hir::QPath;
// use rustc_span::def_id::DefId;

use tracing::{info};

type FnDecl = super::FnDecl;
type ProcessContext<'ctx> = super::core::ProcessContext<'ctx>;
type ProcessHandler = super::core::ProcessHandler;

pub fn run(root_crate: &str, handler: ProcessHandler) {
    let rustc_out = std::process::Command::new("rustc")
        .arg("--print=sysroot")
        .current_dir(".")
        .output()
        .unwrap()
    ;
    let sysroot = String::from_utf8(rustc_out.stdout).unwrap().trim().to_string();

    // let root_crate_name = "std";
    
    let file_path: PathBuf = [
        &sysroot, 
        "lib/rustlib/src/rust/library",
        root_crate, "src/lib.rs"
    ]
    .iter().collect();

    info!("root_path: {}", file_path.display());

    // let home_dir: String = std::env::var_os("HOME").and_then(|v| v.into_string().ok()).unwrap();
    // let cargo_home: PathBuf = [home_dir, ".cargo/registry/src".to_string()].iter().collect();
    // println!("[INFO] cargo-home: {}", cargo_home.display());

    // let cargo_repo: PathBuf = find_path(&cargo_home, &path_is_accepted("index.crates.io-")).unwrap().unwrap();
    // let mut file_path = find_path(&cargo_repo, &path_is_accepted("rand-")).unwrap().unwrap();

    let using_internal_features = rustc_driver::install_ice_hook(
        "https://github.com/rust-lang/rust/issues/new?labels=C-bug%2C+I-ICE%2C+T-rustdoc&template=ice.md",
        |_| (),
    );

    let errors = rustc_driver::diagnostics_registry();
    // let errors = rustc_errors::registry::Registry::new(vec![]);

    let rustc_config = interface::Config {
        opts: config::Options {
            maybe_sysroot: Some(PathBuf::from(sysroot)),
            ..config::Options::default()
        },
        input: config::Input::File(file_path),
        registry: errors,
        locale_resources: rustc_driver::DEFAULT_LOCALE_RESOURCES, 
        lint_caps: FxHashMap::default(),
        crate_cfg: vec![],
        crate_check_cfg: vec![], 
        expanded_args: vec![], 
        output_dir: None,
        output_file: None,
        file_loader: None,
        register_lints: None,
        override_queries: None,
        hash_untracked_state: None,
        ice_file: None, 
        make_codegen_backend: None,
        psess_created: None,
        using_internal_features,
    };

    interface::run_compiler(rustc_config, |compiler| {
        compiler.enter(|queries| {
            let Ok(mut gcx) = queries.global_ctxt() else { rustc_errors::FatalError.raise() };
            gcx.enter(|ctx| {
                let _ = handler.handle_extract(&ProcessContext::new(ctx, root_crate));
            })
        })
    })
}