use std::collections::HashMap;

use kay_syntax::{ast, node::SrcNode, span::Span};

use crate::{error::Error, ty::GenScopeId};

#[derive(Debug)]
pub struct Struct {
    pub name: SrcNode<ast::Ident>,
    gen_scope: Option<GenScopeId>,
}

#[derive(Debug, Clone, Copy)]
pub struct StructId(usize);

#[derive(Debug, Default)]
pub struct Structs {
    name_lut: HashMap<ast::Ident, (Span, StructId, Option<GenScopeId>)>,
    structs: Vec<(Span, Option<Struct>)>,
}

impl Structs {
    pub fn declare_struct(
        &mut self,
        name: SrcNode<ast::Ident>,
        gen_scope: Option<GenScopeId>,
    ) -> Result<StructId, Error> {
        let id = StructId(self.structs.len());
        if let Err(old) = self
            .name_lut
            .try_insert(*name, (name.span(), id, gen_scope))
        {
            Err(Error::DuplicateTypeName {
                name: *name,
                old_span: old.entry.get().0,
                span: name.span(),
            })
        } else {
            self.structs.push((name.span(), None));
            Ok(id)
        }
    }
}
