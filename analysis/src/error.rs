use kay_syntax::{ast, span::Span};

#[derive(Debug)]
pub enum Error {
    DuplicateGenName {
        name: ast::Ident,
        old_span: Span,
        span: Span,
    },
    DuplicateTypeName {
        name: ast::Ident,
        old_span: Span,
        span: Span,
    },
}
