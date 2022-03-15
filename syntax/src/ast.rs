use std::{fmt, ops};

use internment::Intern;

use crate::node::SrcNode;

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Ident {
    pub inner: Intern<String>,
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

#[derive(Debug, Clone)]
pub enum UnOp {
    Deref,
    Neg,
}

#[derive(Debug, Clone)]
pub struct Path {
    pub segments: Vec<SrcNode<Ident>>,
}

#[derive(Debug)]
pub enum Expr {
    Lit(SrcNode<Lit>),
    Path(SrcNode<Path>),
    Call(SrcNode<Self>, Vec<SrcNode<Self>>),
    Addr(SrcNode<Self>),
    Unary(SrcNode<UnOp>, SrcNode<Self>),
    Binary(SrcNode<BinOp>, SrcNode<Self>, SrcNode<Self>),
}

#[derive(Debug, Clone)]
pub struct FieldDef {
    pub ident: SrcNode<Ident>,
    pub ty: SrcNode<Ty>,
}

#[derive(Debug)]
pub struct Param {
    pub ident: SrcNode<Ident>,
    pub ty: SrcNode<Ty>,
}

#[derive(Debug)]
pub enum FnRetTy {
    Default(SrcNode<()>),
    Ty(SrcNode<Ty>),
}

#[derive(Debug)]
pub struct FnSig {
    pub inputs: SrcNode<Vec<SrcNode<Param>>>,
    pub output: SrcNode<FnRetTy>,
}

#[derive(Debug)]
pub struct Generics {
    pub params: Vec<SrcNode<Ident>>,
}

#[derive(Debug)]
pub struct Block {
    pub stmts: Vec<Stmt>,
}

#[derive(Debug, Clone)]
pub enum Ty {
    Path(SrcNode<Path>),
    Ptr(SrcNode<Self>),
}

#[derive(Debug)]
pub enum ItemKind {
    Struct {
        generics: Option<SrcNode<Generics>>,
        fields: Option<SrcNode<Vec<SrcNode<FieldDef>>>>,
    },
    Func {
        generics: Option<SrcNode<Generics>>,
        sig: FnSig,
        block: SrcNode<Block>,
    },
}

#[derive(Debug)]
pub struct Item {
    pub ident: SrcNode<Ident>,
    pub kind: SrcNode<ItemKind>,
}

#[derive(Debug)]
pub struct Local {
    pub ident: SrcNode<Ident>,
    pub kind: SrcNode<LocalKind>,
}

#[derive(Debug)]
pub enum LocalKind {
    Init(SrcNode<Expr>, Option<SrcNode<Ty>>),
    Decl(SrcNode<Ty>),
}

#[derive(Debug)]
pub enum Stmt {
    Item(SrcNode<Item>),
    Expr(SrcNode<Expr>),
    Local(SrcNode<Local>),
}

#[derive(Debug)]
pub struct Module {
    pub items: Vec<SrcNode<Item>>,
}
