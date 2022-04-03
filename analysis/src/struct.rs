use std::collections::{HashMap, HashSet};

use kay_syntax::ast;

use crate::ty::Ty;

#[derive(Debug, Clone, Copy)]
pub struct StructId(usize);

#[derive(Debug)]
pub struct StructIndex {
    structs: Vec<Struct>,
}

impl StructIndex {
    pub fn new() -> Self {
        Self {
            structs: Vec::new(),
        }
    }

    pub fn define_struct(&mut self, r#struct: ast::Struct) {
        let ast::Struct {
            ident,
            fields,
            generics,
        } = r#struct;

        let ident = *ident;
        let generics = generics
            .map(|generics| generics.params.iter().map(|p| **p).collect())
            .unwrap_or_default();
        let fields = fields
            .map(|fields| {
                fields
                    .iter()
                    .map(|field| {
                        let ast::StructFieldDef { ident, ty } = &**field;
                        let ident = **ident;
                        let ty = Ty::from(&**ty);

                        (ident, ty)
                    })
                    .collect()
            })
            .unwrap_or_default();

        self.structs.push(Struct {
            ident,
            fields,
            generics,
        })
    }
}

#[derive(Debug)]
pub struct Struct {
    pub ident: ast::Ident,
    pub fields: HashMap<ast::Ident, Ty>,
    pub generics: HashSet<ast::Ident>,
}

pub fn define_structs(structs: Vec<ast::Struct>) -> StructIndex {
    let mut index = StructIndex {
        structs: Vec::new(),
    };

    for r#struct in structs {}

    index
}
