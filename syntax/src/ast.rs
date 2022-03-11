use std::{fmt, ops};

use internment::Intern;

use crate::node::SrcNode;

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Ident {
    inner: Intern<String>,
}

impl fmt::Debug for Ident {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.inner)
    }
}

impl fmt::Display for Ident {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.inner)
    }
}

impl Ident {
    pub fn new<S>(s: S) -> Self
    where
        S: ToString,
    {
        let inner = Intern::new(s.to_string());

        Self { inner }
    }

    pub fn as_ref(&self) -> &'static str {
        self.inner.as_ref()
    }
}

impl ops::Deref for Ident {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

#[derive(Debug)]
pub enum Lit {
    Int(u128),
    Str(String),
}

#[derive(Debug, Clone)]
pub enum BinOp {
    Add,
    Sub,

    Mul,
    Div,
    Rem,
}

#[derive(Debug)]
pub struct Path {
    pub segments: Vec<Ident>,
}

#[derive(Debug)]
pub enum Expr {
    Binary(SrcNode<BinOp>, SrcNode<Self>, SrcNode<Self>),
    Lit(Lit),
    Call(SrcNode<Self>, Vec<SrcNode<Self>>),
    Path(Path),
}

#[derive(Debug)]
pub enum ItemKind {
    Struct {
        generics: Option<Vec<Ident>>,
        fields: Option<Vec<(Ident, Ident)>>,
    },
    Func {
        inputs: Vec<(Ident, Ident)>,
        output: Option<Ident>,
        block: SrcNode<Block>,
    },
}

#[derive(Debug)]
pub struct Block {
    pub stmts: Vec<Stmt>,
}

#[derive(Debug)]
pub struct Item {
    pub ident: Ident,
    pub kind: SrcNode<ItemKind>,
}

#[derive(Debug)]
pub enum Stmt {
    Item(SrcNode<Item>),
    Expr(SrcNode<Expr>),
}
