use kay_syntax::ast;

pub struct Context<'a> {
    pub structs: Vec<&'a ast::Struct>,
}

impl<'a> Context<'a> {
    pub fn from_module(ast: &'a ast::Module) -> Self {
        Self {
            structs: ast.structs().collect(),
        }
    }
}
