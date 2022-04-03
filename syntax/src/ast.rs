use std::{fmt, ops};

use internment::Intern;

use crate::{node::SrcNode, span::Span};

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

#[derive(Debug, PartialEq)]
pub enum Lit {
    Int(u128),
    Str(Intern<String>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinOp {
    Add,
    Sub,

    Mul,
    Div,
    Rem,
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnOp {
    Deref,
    Neg,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Path {
    pub segments: Vec<SrcNode<Ident>>,
}

#[derive(Debug, PartialEq)]
pub enum Expr {
    Lit(SrcNode<Lit>),
    Path(SrcNode<Path>),
    Addr(SrcNode<Self>),
    Call {
        callee: SrcNode<Self>,
        args: Vec<SrcNode<Self>>,
    },
    Unary {
        op: SrcNode<UnOp>,
        operand: SrcNode<Self>,
    },
    Binary {
        op: SrcNode<BinOp>,
        lhs: SrcNode<Self>,
        rhs: SrcNode<Self>,
    },
    Error,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Ty {
    Path(SrcNode<Path>),
    Ptr(SrcNode<Self>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructFieldDef {
    pub ident: SrcNode<Ident>,
    pub ty: SrcNode<Ty>,
}

#[derive(Debug, PartialEq)]
pub struct FnParam {
    pub ident: SrcNode<Ident>,
    pub ty: SrcNode<Ty>,
}

#[derive(Debug, PartialEq)]
pub enum FnRetTy {
    Default(Span),
    Ty(SrcNode<Ty>),
}

#[derive(Debug, PartialEq)]
pub struct FnSig {
    pub inputs: SrcNode<Vec<SrcNode<FnParam>>>,
    pub output: SrcNode<FnRetTy>,
}

#[derive(Debug, PartialEq)]
pub struct Generics {
    pub params: Vec<SrcNode<Ident>>,
}

#[derive(Debug, PartialEq)]
pub struct Block {
    pub stmts: Vec<Stmt>,
}

#[derive(Debug, PartialEq)]
pub struct Struct {
    pub ident: SrcNode<Ident>,
    pub generics: Option<SrcNode<Generics>>,
    pub fields: Option<SrcNode<Vec<SrcNode<StructFieldDef>>>>,
}

#[derive(Debug, PartialEq)]
pub struct Func {
    pub ident: SrcNode<Ident>,
    pub generics: Option<SrcNode<Generics>>,
    pub sig: FnSig,
    pub block: SrcNode<Block>,
}

#[derive(Debug, PartialEq)]
pub enum Item {
    Struct(SrcNode<Struct>),
    Func(SrcNode<Func>),
}

#[derive(Debug, PartialEq)]
pub enum LocalKind {
    Init(SrcNode<Expr>, Option<SrcNode<Ty>>),
    Decl(SrcNode<Ty>),
}

#[derive(Debug, PartialEq)]
pub struct Local {
    pub ident: SrcNode<Ident>,
    pub kind: SrcNode<LocalKind>,
}

#[derive(Debug, PartialEq)]
pub enum Stmt {
    Item(SrcNode<Item>),
    Expr(SrcNode<Expr>),
    Local(SrcNode<Local>),
}

#[derive(Debug, PartialEq)]
pub struct Module {
    pub items: Vec<SrcNode<Item>>,
}

impl Module {
    pub fn structs(&self) -> impl Iterator<Item = &Struct> + '_ {
        self.items.iter().filter_map(|item| match &**item {
            Item::Struct(r#struct) => Some(&**r#struct),
            _ => None,
        })
    }

    pub fn funcs(&self) -> impl Iterator<Item = &Func> + '_ {
        self.items.iter().filter_map(|item| match &**item {
            Item::Func(func) => Some(&**func),
            _ => None,
        })
    }
}
