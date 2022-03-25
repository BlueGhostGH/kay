use kay_syntax::ast;

use crate::{
    func::Funcs,
    r#struct::Structs,
    ty::{GenScope, Types},
};

pub struct Context {
    pub structs: Structs,
    pub funcs: Funcs,
    pub tys: Types,
}

impl Context {
    pub fn from_module(ast: &ast::Module) -> Self {
        let mut this = Self {
            structs: Structs::default(),
            funcs: Funcs::default(),
            tys: Types::default(),
        };

        let mut errors = Vec::new();

        let mut structs = Vec::new();
        let mut funcs = Vec::new();

        for (r#struct, name) in ast.structs() {
            let gen_scope = r#struct.generics.as_ref().map(|generics| {
                let (gen_scope, mut errs) = GenScope::from_ast(generics);
                errors.append(&mut errs);

                this.tys.insert_gen_scope(gen_scope)
            });

            if let Err(err) = this.structs.declare_struct(name.clone(), gen_scope) {
                errors.push(err);
                continue;
            } else {
                structs.push(r#struct)
            }
        }

        for (func, name) in ast.funcs() {
            let gen_scope = func.generics.as_ref().map(|generics| {
                let (gen_scope, mut errs) = GenScope::from_ast(generics);
                errors.append(&mut errs);

                this.tys.insert_gen_scope(gen_scope)
            });

            if let Err(err) = this.funcs.declare_func(name.clone(), gen_scope) {
                errors.push(err);
                continue;
            } else {
                funcs.push(func)
            }
        }

        this
    }
}
