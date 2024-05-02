use std::collections::HashMap;

use rustc_hir::def_id::LocalModDefId;
use rustc_hir::definitions::DefPath;
use rustc_hir::definitions::DefPathData;
use rustc_middle::ty::Visibility;
use rustc_span::def_id::DefId;
use rustc_hir::def_id::CrateNum;
use rustc_middle::ty::TyCtxt;

use rustc_hir::Node;
use rustc_hir::HirId;
use rustc_hir::Item;
use rustc_hir::ImplItemId;
use rustc_hir::ImplItem;

use crate::FnDecl;
use crate::ModuleDecl;
use crate::TypeDecl;

pub struct ImportConfig { 
    from_type: TypeDecl, 
    to_type: TypeDecl, 
}

impl ImportConfig {
    pub fn new(from_type: TypeDecl, to_type: TypeDecl) -> Self {
        ImportConfig { from_type: from_type.clone(), to_type: to_type.clone() }
    }
}

pub struct ProcessContext<'ctx> {
    pub raw_context: TyCtxt<'ctx>,
    root_crate_symbol: String,
    import_lookup: HashMap<String, ImportConfig>
}

impl ProcessContext<'_> {
    pub fn new<'ctx>(raw_context: TyCtxt<'ctx>, root_crate: &str) -> ProcessContext<'ctx> {
        ProcessContext { 
            raw_context, 
            root_crate_symbol: root_crate.to_string(),
            import_lookup: HashMap::<String, ImportConfig>::new(),
        }
    }

    pub fn walk_item<'hir, F>(&self, f: F) -> Vec<FnDecl>
        where F: Fn(HirId, DefId, &Item) -> Option<Vec<FnDecl>>
    {
        let mut fns = vec![];

        for id in self.raw_context.hir().items() {
            let item = self.raw_context.hir().item(id);

            match f(id.hir_id(), item.owner_id.def_id.to_def_id(), &item) {
                Some(mut decls) => {
                    fns.append(&mut decls);
                }
                // WalkOutput::TypeAlias { target: _target, alias_decl: _alias_decl } => {

                // },
                None => {},
            };
        };

        fns
    }

    pub fn walk_import_item<'hir, F>(&self, f: F) -> HashMap<String, ImportConfig>
        where F: Fn(HirId, DefId, &Item) -> Option<Vec<ImportConfig>>
    {
        let mut map = HashMap::<String, ImportConfig>::new();

        for id in self.raw_context.hir().items() {
            let item = self.raw_context.hir().item(id);

            match f(id.hir_id(), item.owner_id.def_id.to_def_id(), &item) {
                Some(configs) => {
                    for config in configs {
                        let from_ley = config.from_type.category.prefix_with(Some(&config.from_type.module));

                        if let std::collections::hash_map::Entry::Vacant(entry) = map.entry(from_ley) {
                            entry.insert(config);
                        }
                    }
                }
                None => {}
            }
        }

        map
    }

    pub fn set_import_loopup(&mut self, lookup: HashMap<String, ImportConfig>) {
        self.import_lookup = lookup;
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

    pub fn visibility(&self, def_id: &DefId) -> Visibility<DefId> {
        self.raw_context.visibility(*def_id)
    }

    pub fn item_of(&self, id: rustc_hir::ItemId) -> &Item {
        self.raw_context.hir().item(id)
    }

    pub fn impl_item(&self, id: ImplItemId) -> &ImplItem {
        self.raw_context.hir().impl_item(id)
    }

    pub fn mod_items_of(&self, def_id: &DefId) -> &[rustc_middle::metadata::ModChild] {
        self.raw_context.module_children(def_id)
    }

    pub fn attrs_of(&self, hir_id: HirId) -> &[rustc_ast::Attribute] {
        self.raw_context.hir().attrs(hir_id)
    }

    pub fn lookup_deprecation(&self, def_id: DefId) -> Option<rustc_attr::Deprecation> {
        self.raw_context.lookup_deprecation(def_id)
    }

    pub fn parent_module_of(&self, hir_id: HirId) -> LocalModDefId {
        self.raw_context.parent_module(hir_id)
    }

    pub fn reexport_module(&self, decl: &TypeDecl) -> Option<ModuleDecl> {
        match self.import_lookup.get(&decl.category.prefix_with(Some(&decl.module))) {
            Some(config) => Some(config.to_type.module.clone()),
            _ => None,
        }

    }
}

pub fn is_import_from_prelude(attrs: &[rustc_ast::Attribute]) -> bool {
    attrs.into_iter()
    .any(|attr| {
        if let Some(meta_list) = attr.meta_item_list() {
            return meta_list.into_iter()
            .filter_map(|meta| match meta {
                rustc_ast::NestedMetaItem::MetaItem(meta) => Some(meta),
                rustc_ast::NestedMetaItem::Lit(_) => None,
            })
            .any(|meta| match meta.kind {
                rustc_ast::MetaItemKind::NameValue(lit) => lit.symbol.as_str().contains("prelude"),
                _ => false,
            })
        }

        false
    })
}

pub fn is_prelude_mod(defpath: &DefPath) -> bool {
    defpath.data.iter()
    .any(|x| match x.data {
        DefPathData::TypeNs(ns) => ns.as_str().contains("prelude"),
        _ => false,
    })
}
