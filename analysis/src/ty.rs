use std::collections::{BTreeMap, HashMap};

use kay_syntax::{ast, node::SrcNode, span::Span};

use crate::error::Error;

#[derive(Debug)]
pub enum Prim {
    Int,
    Str,
}

#[derive(Debug)]
pub enum Ty {
    Prim(Prim),
    Struct(BTreeMap<ast::Ident, TyId>),
    Gen(usize, GenScopeId),
}

#[derive(Debug, Clone, Copy)]
pub struct TyId(usize);

#[derive(Debug, Default)]
pub struct Types {
    tys: Vec<(Span, Ty)>,
    scopes: Vec<GenScope>,
}

impl Types {
    pub fn insert_gen_scope(&mut self, gen_scope: GenScope) -> GenScopeId {
        let id = GenScopeId(self.scopes.len());
        self.scopes.push(gen_scope);
        id
    }
}

#[derive(Debug)]
pub struct GenTy {
    pub name: SrcNode<ast::Ident>,
}

#[derive(Debug, Clone, Copy)]
pub struct GenScopeId(usize);

#[derive(Debug)]
pub struct GenScope {
    pub span: Span,
    types: Vec<GenTy>,
}

impl GenScope {
    pub fn from_ast(generics: &SrcNode<ast::Generics>) -> (Self, Vec<Error>) {
        let mut existing = HashMap::new();
        let mut errors = Vec::new();

        for gen_ty in generics.params.iter() {
            if let Some(old_span) = existing.insert(gen_ty.inner, gen_ty.span()) {
                errors.push(Error::DuplicateGenName {
                    name: gen_ty.inner().clone(),
                    old_span,
                    span: gen_ty.span(),
                });
            }
        }

        let scope = Self {
            span: generics.span(),
            types: generics
                .params
                .iter()
                .map(|ident| GenTy {
                    name: ident.clone(),
                })
                .collect(),
        };

        (scope, errors)
    }
}
