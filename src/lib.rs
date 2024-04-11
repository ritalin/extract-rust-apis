#![feature(rustc_private)]

extern crate rustc_interface;
extern crate rustc_session;
extern crate rustc_hir;
extern crate rustc_hash;
extern crate rustc_span;
extern crate rustc_errors;
extern crate rustc_driver; 
extern crate rustc_middle; 

use serde::{Serialize, ser::SerializeStruct};


#[derive(Debug, Clone)]
pub struct TypeDecl {
    symbol: String,
    qual_symbol: String,
    crate_symbol: Option<String>,
}

impl TypeDecl {
    pub fn unknown() -> Self {
        TypeDecl {
            symbol: "????".to_string(),
            qual_symbol: "????".to_string(),
            crate_symbol: None,
        }
    }

    pub fn as_slice(self) -> Self {
        TypeDecl {
            symbol: format!("[{}]", self.symbol),
            qual_symbol: format!("[{}]", self.qual_symbol),
            crate_symbol: self.crate_symbol,
        }
    }

    pub fn as_ptr(self) -> Self {
        TypeDecl {
            symbol: format!("*{}", self.symbol),
            qual_symbol: format!("*{}", self.qual_symbol),
            crate_symbol: self.crate_symbol,
        }
    }
}

impl Serialize for TypeDecl {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut decl = serializer.serialize_struct("type_decl", 3)?;
        decl.serialize_field("symbol", &self.symbol)?;
        decl.serialize_field("qual_symbol", &self.qual_symbol)?;
        decl.serialize_field("crate_symbol", &self.crate_symbol)?;
        decl.end()
    }
}

#[derive(Debug)]
pub struct FnDecl {
    proto: TypeDecl,
    ret_decl: Option<TypeDecl>,
    args: Vec<TypeDecl>,
}

pub mod compile;
pub mod core;
pub mod print;
