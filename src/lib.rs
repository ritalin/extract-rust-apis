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
extern crate rustc_ast;

use std::fmt::Display;

use rustc_attr::DeprecatedSince;

use rustc_hir::def_id::DefId;
use serde::{Serialize, ser::SerializeStruct};

#[derive(Debug, Clone)]
pub struct ModuleDecl {
    path: Option<String>, 
    generics: Option<Vec<String>>,
    krate: Option<String>,
}

impl ModuleDecl {
    pub fn none() -> Self {
        ModuleDecl { path: None, generics: None, krate: None, }
    }

    pub fn to_path(&self) -> Option<String> {
        match self {
            Self { path: Some(path), krate: Some(krate), generics: Some(generics) } => {
                Some(format!("{krate}::{path}::<{}>", generics.join(",")))
            }
            Self { path: Some(path), krate: Some(krate), generics: None } => {
                Some(format!("{krate}::{path}"))
            }
            Self { path: Some(path), krate: None, .. } => Some(format!("{path}")),
            Self { path: None, krate: Some(krate), .. } => Some(format!("{krate}")),
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
    Primitive(String),
    Slice(Box<TypeDecl>),
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
            TypeCategory::Primitive(path) => {
                path.to_string()
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
                let item = x.category.prefix_with(prefix);
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
            TypeCategory::Primitive(_) | 
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
pub enum TypeVisibility {
    Public,
    Restricted,
}

use rustc_middle::ty::Visibility;

impl From<Visibility<DefId>> for TypeVisibility 
{
    fn from(value: Visibility<DefId>) -> Self {
        match value {
            Visibility::Public => TypeVisibility::Public,
            Visibility::Restricted(_) => TypeVisibility::Restricted,
        }
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
            module: ModuleDecl { path: Some("????***(unknown)***".to_string()), generics: None, krate: Some("????".to_string()) },
            deprecation: None,
        }
    }

    pub fn as_slice(self) -> Self {
        TypeDecl { category: TypeCategory::Slice(Box::new(self.clone())), ..self }
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

    pub fn make_lookup_key(&self) -> String {
        self.category.prefix_with(Some(&ModuleDecl { generics: None, ..self.module.clone() }))
    }
}

impl Serialize for TypeDecl {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        pub struct TypeItemExport<'a> {
            decl: &'a TypeDecl,
        }
        
        impl <'a> Serialize for TypeItemExport<'a> {
            fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
            where
                S: serde::Serializer,
            {
                let mut decl = serializer.serialize_struct("type_decl", 4)?;
                {
                    decl.serialize_field("lookup_key", &self.decl.make_lookup_key())?;
                    decl.serialize_field("symbol", &self.decl.category.prefix_with(None))?;
                    decl.serialize_field("module_symbol", &self.decl.module.path)?;
                    decl.serialize_field("crate_symbol", &self.decl.module.krate)?;
                }
                decl.end()
            }
        }

        let mut decl = serializer.serialize_struct("type_decl", 5)?;
        {
            decl.serialize_field("symbol", &self.category.to_string())?;
            decl.serialize_field("module_symbol", &self.module.path)?;
            decl.serialize_field("crate_symbol", &self.module.krate)?;

            // optional fields ...
            match &self.category {
                TypeCategory::Symbol(_) |
                TypeCategory::Primitive(_) |
                TypeCategory::Ptr(_) => {}
                TypeCategory::Slice(x) => {
                    decl.serialize_field("slice_member", &vec![ TypeItemExport { decl: x } ])?;
                }
                TypeCategory::SelfAlias(_) => {
                    decl.serialize_field("alias", "Self")?;
                }
                TypeCategory::Tuple(xs) => {
                    let members = xs.into_iter()
                        .map(|x| TypeItemExport { decl: x })
                        .collect::<Vec<_>>()
                    ;

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
    owner: Option<TypeDecl>,
}

impl FnDecl {
    pub fn qual_symbol(&self) -> String {
        let module = self.owner.as_ref().map(|TypeDecl { category, module, ..  }| {
            ModuleDecl {
                path: Some(category.prefix_with(Some(&module))),
                generics: None,
                krate: None,
            }
        });
        let proto = self.proto.category.prefix_with(module.as_ref());

        let ret = self.ret_decl.as_ref()
            .map(|ret| format!(" -> {}", &ret.category.prefix_with(Some(&ret.module))))
            .unwrap_or("".to_string())
        ;
        let args = self.args.iter()
            .map(|arg| arg.category.prefix_with(Some(&arg.module)))
            .collect::<Vec<_>>()
            .join(", ")
        ;

        format!("{}({}){}", proto, args, ret)
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
        struct FnArg<'a> {
            index: usize,
            decl: &'a TypeDecl,
        }
        impl <'a> Serialize for FnArg<'a> {
            fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
            where S: serde::Serializer 
            {
                let mut arg_decl = serializer.serialize_struct("fn_decl", 3)?;
                {
                    arg_decl.serialize_field("sort_order", &self.index)?;
                    arg_decl.serialize_field("lookup_key", &self.decl.make_lookup_key())?;
                    arg_decl.serialize_field("generic_args", &self.decl.module.generics)?;
                }
                arg_decl.end()
            }
        }

        struct FnReturn<'a> {
            decl: &'a TypeDecl,
        }
        impl <'a> Serialize for FnReturn<'a> {
            fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
            where S: serde::Serializer 
            {
                let mut decl = serializer.serialize_struct("ret_decl", 1)?;
                {
                    decl.serialize_field("lookup_key", &self.decl.make_lookup_key())?;
                    decl.serialize_field("generic_args", &self.decl.module.generics)?;
                }
                decl.end()
            }
        }

        struct FnOwner<'a> {
            decl: &'a TypeDecl,
        }
        impl <'a> Serialize for FnOwner<'a> {
            fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
            where S: serde::Serializer 
            {
                let mut decl = serializer.serialize_struct("owner", 2)?;
                {
                    decl.serialize_field("lookup_key", &self.decl.make_lookup_key())?;
                    decl.serialize_field("generic_args", &self.decl.module.generics)?;
                }
                decl.end()
            }
        }
        let ret = self.ret_decl.as_ref()
            .map(|decl| FnReturn { decl: &decl })
        ;
        let args = self.args.iter()
            .enumerate()
            .map(|(i, decl)| FnArg { index: i+1, decl: decl })
            .collect::<Vec<_>>()
        ;

        let mut decl = serializer.serialize_struct("fn_decl", 7)?;
        {
            decl.serialize_field("proto", &self.proto.category.to_string())?; 
            decl.serialize_field("proto_qual", &self.qual_symbol())?;
            decl.serialize_field("ret_decl", &ret)?;
            decl.serialize_field("args", &args)?;
            decl.serialize_field("owner", &self.owner.as_ref().map(|owner| FnOwner { decl: &owner }))?;

            if let Some(deprecation) = self.proto.format_deprecated() {
                decl.serialize_field("deprecated", &deprecation)?;
            }
        }
        decl.end()
    }
}

pub mod compile;
pub mod core;
pub mod print;
pub mod support;