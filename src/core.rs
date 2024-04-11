
use rustc_middle::ty::TyCtxt;
use tracing::{info};

type FnDecl = super::FnDecl;

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
}

pub struct ProcessHandler;

impl ProcessHandler {
    pub fn handle_extract(&self, _ctx: &ProcessContext) -> Vec<FnDecl> {
        info!("Begining extract");

        vec![]
    }
}