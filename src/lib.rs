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

use std::fmt::Display;

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
pub struct ModuleDecl {
    path: Option<String>, 
    krate: Option<String>,
}

impl ModuleDecl {
    pub fn none() -> Self {
        ModuleDecl { path: None, krate: None, }
    }

    pub fn to_path(&self) -> Option<String> {
        match self {
            Self { path: Some(path), krate: Some(krate) } => Some(format!("{krate}::{path}")),
            Self { path: Some(path), krate: None } => Some(format!("{path}")),
            _ => None,
        }
    }
}

impl Display for ModuleDecl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.to_path() {
            Some(path) => write!(f, "{path}"),
            _ => write!(f, ""),
        }
    }
}

#[derive(Debug, Clone)]
pub enum TypeCategory {
    Symbol(String),
        // qual_symbol: String,
        // crate_symbol: Option<String>,
        // type_category: TypeCategory,
        // deprecation: Option<rustc_attr::Deprecation>,
    Slice(Box<TypeCategory>),
    Tuple(Vec<TypeDecl>),
    Ptr(Box<TypeCategory>),
    SelfAlias(Box<TypeCategory>),
}

impl TypeCategory {
    pub fn prefix_with(&self, prefix: Option<&ModuleDecl>) -> String 
    {
        match self {
            TypeCategory::Symbol(path) => {
                match prefix.as_ref().and_then(|p| p.to_path()) {
                    Some(prefix) => format!("{}::{}", prefix, path), 
                    None => path.to_string(),
                }
            }
            TypeCategory::Tuple(xs) => {
                let items = xs.into_iter().map(|x| {
                    let module = match prefix {
                        None => None,
                        _ => Some(&x.module),
                    };
                    return x.category.prefix_with(module);
                }).collect::<Vec<_>>().join(", ");
                format!("({})", items)
            }
            TypeCategory::Slice(x) => {
                let item = x.prefix_with(prefix);
                format!("[{}]", item)
            }
            TypeCategory::Ptr(x) => {
                let item = x.prefix_with(prefix);
                format!("*{}", item)
            }
            TypeCategory::SelfAlias(x) => x.prefix_with(prefix),
        }     
    }

    pub fn append_generic_params(&self, params: String) -> TypeCategory {
        match self {
            TypeCategory::Symbol(path) => {
                TypeCategory::Symbol(format!("{path}<{params}>"))
            }
            TypeCategory::Ptr(x) => {
                TypeCategory::Ptr(Box::new(x.append_generic_params(params)))
            }
            TypeCategory::SelfAlias(x) => {
                TypeCategory::SelfAlias(Box::new(x.append_generic_params(params)))
            }
            TypeCategory::Slice(_) |
            TypeCategory::Tuple(_) => self.clone(),
        }
    }
}

impl Display for TypeCategory {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.prefix_with(None))
    }
}

#[derive(Debug, Clone)]
pub struct TypeDecl {
    category: TypeCategory,
    module: ModuleDecl,
    deprecation: Option<rustc_attr::Deprecation>,
}

impl TypeDecl {
    pub fn unknown() -> Self {
        TypeDecl {
            category: TypeCategory::Symbol(
                "????***(unknown)***".to_string()
            ),
            module: ModuleDecl { path: Some("????***(unknown)***".to_string()), krate: Some("????".to_string()) },
            deprecation: None,
        }
    }

    pub fn as_slice(self) -> Self {
        TypeDecl { category: TypeCategory::Slice(Box::new(self.category)), ..self }
    }

    pub fn as_ptr(self) -> Self {
        TypeDecl { category: TypeCategory::Ptr(Box::new(self.category)), ..self }
    }

    pub fn format_deprecated(&self) -> Option<String> {
        match self.deprecation {
            Some(rustc_attr::Deprecation { since: DeprecatedSince::RustcVersion(version), .. }) => {
                Some(format!("{version}"))
            }
            Some(rustc_attr::Deprecation { since: DeprecatedSince::Future, .. }) => {
                Some("future...".to_string())
            }
            Some(_) => {
                Some("unknown...".to_string())
            }
            _ => None
        }
    }
}

impl Serialize for TypeDecl {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut decl = serializer.serialize_struct("type_decl", 5)?;
        {
            let module = Some(&self.module);

            decl.serialize_field("symbol", &self.category.to_string())?;
            decl.serialize_field("qual_symbol", &self.category.prefix_with(module))?;
            decl.serialize_field("crate_symbol", &self.module.krate)?;

            // optional fields ...
            match &self.category {
                TypeCategory::Symbol(_) |
                TypeCategory::Ptr(_) => {}
                TypeCategory::Slice(x) => {
                    let path = x.prefix_with(None);

                    decl.serialize_field("slice_member", &vec![ SymbolDecl { 
                        symbol: path.to_string(), 
                        qual_symbol: TypeCategory::Symbol(path).prefix_with(module), 
                    } ])?;
                }
                TypeCategory::SelfAlias(_) => {
                    decl.serialize_field("alias", "Self")?;
                }
                TypeCategory::Tuple(xs) => {
                    let members = xs.into_iter().map(|x| {
                        let path = x.category.prefix_with(None);

                        return SymbolDecl {
                            symbol: path.to_string(),
                            qual_symbol: TypeCategory::Symbol(path).prefix_with(Some(&x.module)),
                        };
                    })
                    .collect::<Vec<_>>();

                    decl.serialize_field("tuple_members", &members)?;
                }
            };

            if let Some(deprecation) = self.format_deprecated() {
                decl.serialize_field("deprecated", &deprecation)?;
            }
        }
        decl.end()
    }
}

#[derive(Debug)]
pub struct FnDecl {
    proto: TypeDecl,
    ret_decl: Option<TypeDecl>,
    args: Vec<TypeDecl>,
}

impl FnDecl {
    pub fn qual_symbol(&self) -> String {
        let proto = self.proto.category.prefix_with(Some(&self.proto.module));
        let ret = self.ret_decl.as_ref()
            .map(|ret| format!("-> {}", &ret.category.prefix_with(Some(&ret.module))))
            .unwrap_or("".to_string())
        ;
        let args = self.args.iter()
            .map(|arg| arg.category.prefix_with(Some(&arg.module)))
            .collect::<Vec<_>>()
            .join(", ")
        ;

        format!("{}({}) {}", proto, args, ret)
    }
}

impl Display for FnDecl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.qual_symbol())
    }
}

impl Serialize for FnDecl {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where S: serde::Serializer,
    {
        struct Prototype<'a> {
            decl: &'a FnDecl
        }
        impl <'a> Serialize for Prototype<'a> {
            fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
            where S: serde::Serializer 
            {
                let mut decl = serializer.serialize_struct("type_decl", 5)?;
                {
                    decl.serialize_field("symbol", &self.decl.proto.category.to_string())?;
                    decl.serialize_field("qual_symbol", &self.decl.qual_symbol())?;
                    decl.serialize_field("crate_symbol", &self.decl.proto.module.krate)?;

                    if let Some(deprecation) = self.decl.proto.format_deprecated() {
                        decl.serialize_field("deprecated", &deprecation)?;
                    }
                        }
                decl.end()
            }
        }

        let mut decl = serializer.serialize_struct("fn_decl", 4)?;
        {
            decl.serialize_field("proto", &Prototype { decl: self })?;
            decl.serialize_field("ret_decl", &self.ret_decl)?;
            decl.serialize_field("args", &self.args)?;
        }
        decl.end()
    }
}

pub mod compile;
pub mod core;
pub mod print;
