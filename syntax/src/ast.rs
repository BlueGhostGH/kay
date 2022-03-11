use crate::node::SrcNode;

#[derive(Debug)]
pub enum Lit {
    Int(u128),
    Str(String),
}

#[derive(Debug, Clone)]
pub enum BinOp {
    Add,
    Sub,
}

#[derive(Debug)]
pub struct Path {
    pub segments: Vec<String>,
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
        generics: Option<Vec<String>>,
        fields: Option<Vec<(String, String)>>,
    },
    Func {
        inputs: Vec<(String, String)>,
        output: Option<String>,
        block: SrcNode<Block>,
    },
}

#[derive(Debug)]
pub struct Block {
    pub stmts: Vec<Stmt>,
}

#[derive(Debug)]
pub struct Item {
    pub ident: String,
    pub kind: SrcNode<ItemKind>,
}

#[derive(Debug)]
pub enum Stmt {
    Item(SrcNode<Item>),
    Expr(SrcNode<Expr>),
}
