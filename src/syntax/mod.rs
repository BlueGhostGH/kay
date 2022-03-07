use chumsky::{
    error::Simple,
    primitive::{choice, end, filter_map, just},
    recursive::recursive,
    Error, Parser,
};

mod span;
mod token;

pub use span::Span;

use token::Delimiter;
pub use token::{lexer, Token};

#[derive(Debug)]
pub enum Lit {
    Int(i32),
    Str(String),
}

#[derive(Debug, Clone)]
pub enum BinOp {
    Add,
    Sub,
}

#[derive(Debug)]
pub struct Path {
    segments: Vec<PathSegment>,
}

#[derive(Debug)]
pub struct PathSegment {
    ident: String,
}

#[derive(Debug)]
pub struct Expr {
    kind: ExprKind,
}

#[derive(Debug)]
pub enum ExprKind {
    Binary(BinOp, Box<Expr>, Box<Expr>),
    Lit(Lit),
    Call(Box<Expr>, Vec<Box<Expr>>),
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
        block: Box<Block>,
    },
}

#[derive(Debug)]
pub struct Block {
    stmts: Vec<Stmt>,
}

#[derive(Debug)]
pub struct Item {
    ident: String,
    kind: ItemKind,
}

#[derive(Debug)]
pub struct Stmt {
    kind: StmtKind,
}

#[derive(Debug)]
pub enum StmtKind {
    Item(Item),
    Expr(Expr),
}

pub fn parser() -> impl chumsky::Parser<Token, Vec<Item>, Error = Simple<Token, Span>> {
    let expr = recursive(|expr| {
        let expr_list = expr
            .clone()
            .separated_by(just(Token::Comma))
            .delimited_by(
                just(Token::Open(Delimiter::Paren)),
                just(Token::Close(Delimiter::Paren)),
            )
            .or_not();

        let int = filter_map(|span, tok| {
            if let Token::Int(mut int) = tok {
                int.remove_matches('_');
                if let Ok(int) = int.parse() {
                    Ok(Lit::Int(int))
                } else {
                    Err(Simple::custom(span, "invalid integer literal"))
                }
            } else {
                Err(Simple::expected_input_found(span, Vec::new(), Some(tok)))
            }
        });
        let r#str = filter_map(|span, tok| {
            if let Token::Str(r#str) = tok {
                Ok(Lit::Str(r#str))
            } else {
                Err(Simple::expected_input_found(span, Vec::new(), Some(tok)))
            }
        });
        let lit = int.or(r#str).map(ExprKind::Lit).map(|kind| Expr { kind });

        let ident = filter_map(|span, tok| {
            if let Token::Ident(id) = tok {
                Ok(id)
            } else {
                Err(Simple::expected_input_found(span, Vec::new(), Some(tok)))
            }
        });
        let path = ident
            .map(|seg| PathSegment { ident: seg })
            .separated_by(just([Token::Colon, Token::Colon]))
            .at_least(1)
            .allow_leading()
            .map(|segments| Path { segments })
            .map(|path| Expr {
                kind: ExprKind::Path(path),
            });

        let atom = lit.or(path);

        let call = atom.then(expr_list).map(|(f, args)| match args {
            Some(args) => {
                let args = args.into_iter().map(Box::new).collect();

                Expr {
                    kind: ExprKind::Call(Box::new(f), args),
                }
            }
            None => f,
        });

        let op = just(Token::Binary(token::BinOp::Add))
            .to(BinOp::Add)
            .or(just(Token::Binary(token::BinOp::Sub)).to(BinOp::Sub));
        let sum = call
            .clone()
            .then(op.then(call).repeated())
            .foldl(|a, (op, b)| Expr {
                kind: ExprKind::Binary(op, Box::new(a), Box::new(b)),
            });

        sum
    });

    let item = recursive(|item| {
        let ident = filter_map(|span, tok| {
            if let Token::Ident(id) = tok {
                Ok(id)
            } else {
                Err(Simple::expected_input_found(span, Vec::new(), Some(tok)))
            }
        });

        let field = ident.then_ignore(just(Token::Colon)).then(ident);
        let fields = field
            .separated_by(just(Token::Comma))
            .allow_trailing()
            .delimited_by(
                just(Token::Open(Delimiter::Brace)),
                just(Token::Close(Delimiter::Brace)),
            );

        let generics = ident
            .separated_by(just(Token::Comma))
            .delimited_by(just(Token::Lt), just(Token::Gt))
            .or_not();

        let r#struct = just(Token::Struct)
            .ignore_then(ident)
            .then(generics)
            .then(fields.map(Some).or(just(Token::Semicolon).to(None)))
            .map(|((name, generics), fields)| Item {
                ident: name,
                kind: ItemKind::Struct { generics, fields },
            });

        let arg = ident.then_ignore(just(Token::Colon)).then(ident);
        let args = arg.separated_by(just(Token::Comma)).delimited_by(
            just(Token::Open(Delimiter::Paren)),
            just(Token::Close(Delimiter::Paren)),
        );

        let ret_ty = just(Token::RArrow).ignore_then(ident);

        let block = just(Token::Open(Delimiter::Brace))
            .ignore_then(
                choice((item.map(StmtKind::Item), expr.map(StmtKind::Expr)))
                    .map(|kind| Stmt { kind })
                    .repeated(),
            )
            .then_ignore(just(Token::Close(Delimiter::Brace)))
            .map(|stmts| Block { stmts });

        let func = just(Token::Func)
            .ignore_then(ident)
            .then(args)
            .then(ret_ty.or_not())
            .then(block)
            .map(|(((name, args), ret_ty), block)| Item {
                ident: name,
                kind: ItemKind::Func {
                    inputs: args,
                    output: ret_ty,
                    block: Box::new(block),
                },
            });

        r#struct.or(func)
    });

    item.repeated().then_ignore(end())
}
