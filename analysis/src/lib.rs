mod r#struct;
mod ty;

use kay_syntax::ast;

#[derive(Debug)]
pub struct Hir;

pub fn lower(ast: ast::Module) -> Hir {
    let (structs, ast) = ast
        .items
        .into_iter()
        .partition::<Vec<_>, _>(|item| matches!(**item, ast::Item::Struct(_)));
    Hir
}
