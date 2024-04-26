#![feature(rustc_private)]

extern crate rustc_interface;
extern crate rustc_session;
extern crate rustc_hir;
extern crate rustc_hash;
extern crate rustc_span;
extern crate rustc_errors;
extern crate rustc_driver; 
extern crate rustc_middle; 
extern crate rustc_attr; 

use rustc_attr::DeprecatedSince;

use serde::{Serialize, ser::SerializeStruct};

#[derive(Debug, Clone)]
pub struct SymbolDecl {
    symbol: String,
    qual_symbol: String,
}

impl Serialize for SymbolDecl {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut decl = serializer.serialize_struct("type_decl", 4)?;
        {
            decl.serialize_field("symbol", &self.symbol)?;
            decl.serialize_field("qual_symbol", &self.qual_symbol)?;
        }
        decl.end()
    }
}

#[derive(Debug, Clone)]
pub struct TypeDecl {
    symbol: String,
    qual_symbol: String,
    crate_symbol: Option<String>,
    type_category: TypeCategory,
    deprecation: Option<rustc_attr::Deprecation>,
}

impl TypeDecl {
    pub fn unknown() -> Self {
        TypeDecl {
            symbol: "????***(unknown)***".to_string(),
            qual_symbol: "????***(unknown)***".to_string(),
            crate_symbol: None,
            type_category: TypeCategory::Nominal,
            deprecation: None,
        }
    }

    pub fn as_slice(self) -> Self {
        TypeDecl {
            symbol: format!("[{}]", self.symbol),
            qual_symbol: format!("[{}]", self.qual_symbol),
            crate_symbol: self.crate_symbol,
            type_category: TypeCategory::Slice(SymbolDecl { symbol: self.symbol.to_string(), qual_symbol: self.qual_symbol.to_string() }),
            deprecation: self.deprecation,
        }
    }

    pub fn as_ptr(self) -> Self {
        TypeDecl {
            symbol: format!("*{}", self.symbol),
            qual_symbol: format!("*{}", self.qual_symbol),
            crate_symbol: self.crate_symbol,
            type_category: self.type_category,
            deprecation: self.deprecation,
        }
    }
}

impl Serialize for TypeDecl {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut decl = serializer.serialize_struct("type_decl", 4)?;
        {
            decl.serialize_field("symbol", &self.symbol)?;
            decl.serialize_field("qual_symbol", &self.qual_symbol)?;
            decl.serialize_field("crate_symbol", &self.crate_symbol)?;

            match &self.type_category {
                TypeCategory::Nominal | 
                TypeCategory::Function | 
                TypeCategory::Trait => {
                    // dismiss
                }
                TypeCategory::Alias(s) => {
                    decl.serialize_field("alias", &s)?;
                }
                TypeCategory::Slice(member) => {
                    decl.serialize_field("slice_member", &vec![member])?;
                }
                TypeCategory::Tuple { members } => {
                    decl.serialize_field("tuple_members", &members)?;
                }
            };

            if let Some(deprecation) = self.deprecation {
                match deprecation.since {
                    DeprecatedSince::RustcVersion(version) => {
                        decl.serialize_field("deprecated", &format!("{version}"))?;
                    }
                    DeprecatedSince::Future => {
                        decl.serialize_field("deprecated", "future...")?;
                    }
                    _ => {
                        decl.serialize_field("deprecated", "unknown...")?;
                    }
                }
            }
        }
        decl.end()
    }
}

#[derive(Debug, Clone)]
pub enum TypeCategory {
    Nominal,
    Trait,
    Function,
    Alias (String), 
    Slice(SymbolDecl),
    Tuple { members: Vec<SymbolDecl> },
}

#[derive(Debug)]
pub struct FnDecl {
    proto: TypeDecl,
    ret_decl: Option<TypeDecl>,
    args: Vec<TypeDecl>,
}

impl Serialize for FnDecl {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut decl = serializer.serialize_struct("fn_decl", 4)?;
        {
            decl.serialize_field("proto", &self.proto)?;
            decl.serialize_field("ret_decl", &self.ret_decl)?;
            decl.serialize_field("args", &self.args)?;
        }
        decl.end()
    }
}

pub mod compile;
pub mod core;
pub mod print;
