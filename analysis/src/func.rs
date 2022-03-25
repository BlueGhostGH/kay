use std::collections::HashMap;

use kay_syntax::{ast, node::SrcNode, span::Span};

use crate::{error::Error, ty::GenScopeId};

#[derive(Debug)]
pub struct Func {
    pub name: SrcNode<ast::Ident>,
    gen_scope: Option<GenScopeId>,
}

#[derive(Debug, Clone, Copy)]
pub struct FuncId(usize);

#[derive(Debug, Default)]
pub struct Funcs {
    name_lut: HashMap<ast::Ident, (Span, FuncId)>,
    funcs: Vec<Func>,
}

impl Funcs {
    pub fn declare_func(
        &mut self,
        name: SrcNode<ast::Ident>,
        gen_scope: Option<GenScopeId>,
    ) -> Result<FuncId, Error> {
        let id = FuncId(self.funcs.len());
        if let Err(old) = self.name_lut.try_insert(*name, (name.span(), id)) {
            Err(Error::DuplicateTypeName {
                name: *name,
                old_span: old.entry.get().0,
                span: name.span(),
            })
        } else {
            self.funcs.push(Func { name, gen_scope });
            Ok(id)
        }
    }
}
