use std::fmt;

use kay_syntax::ast;

use crate::r#struct::StructId;

#[derive(Clone, Copy)]
pub struct Ty {
    pub ptr_depth: usize,
    pub kind: TypeKind,
}

impl fmt::Debug for Ty {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}{:?}", "&".repeat(self.ptr_depth), self.kind)
    }
}

impl From<&ast::Ty> for Ty {
    fn from(mut ty: &ast::Ty) -> Self {
        let mut this = Self {
            ptr_depth: 0,
            kind: TypeKind::Any,
        };

        loop {
            match ty {
                ast::Ty::Ptr(inner) => {
                    this.ptr_depth += 1;
                    ty = &*inner;
                }
                ast::Ty::Path(path) => {
                    // We know there is at least a segment part
                    let segment = &path.segments[0];

                    this.kind = match segment.as_ref() {
                        "I32" => TypeKind::Primitive(Primitive::I32),
                        "Str" => TypeKind::Primitive(Primitive::Str),
                        "Any" => TypeKind::Any,
                        _ => todo!("Implement struct types"),
                    };

                    break;
                }
            }
        }

        this
    }
}

#[derive(Debug, Clone, Copy)]
pub enum TypeKind {
    Primitive(Primitive),
    Struct(StructId),
    Any,
}

#[derive(Debug, Clone, Copy)]
pub enum Primitive {
    Str,

    I32,
}
