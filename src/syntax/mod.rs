use chumsky::{
    error::Simple,
    primitive::{end, filter_map, just},
    Error, Parser,
};

mod token;

use token::Delimiter;
pub use token::{lexer, Token};

#[derive(Debug)]
pub enum Literal {
    Int(i32),
    Str(String),
}

#[derive(Debug)]
pub enum Expr {
    Literal(Literal),
}

#[derive(Debug)]
pub enum Stmt {
    Struct {
        name: String,
        generics: Option<Vec<String>>,
        fields: Option<Vec<(String, String)>>,
    },
    Func {
        name: String,
        args: Vec<(String, String)>,
    },
}

#[derive(Debug)]
pub enum Item {
    Expr(Expr),
    Stmt(Stmt),
}

pub fn parser() -> impl chumsky::Parser<Token, Vec<Item>, Error = Simple<Token>> {
    let ident = filter_map(|span, tok| {
        if let Token::Ident(id) = tok {
            Ok(id)
        } else {
            Err(Simple::expected_input_found(span, Vec::new(), Some(tok)))
        }
    });

    let int = filter_map(|span, tok| {
        if let Token::Int(mut int) = tok {
            int.remove_matches('_');
            if let Ok(int) = int.parse() {
                Ok(Literal::Int(int))
            } else {
                Err(Simple::custom(span, "invalid integer literal"))
            }
        } else {
            Err(Simple::expected_input_found(span, Vec::new(), Some(tok)))
        }
    });
    let r#str = filter_map(|span, tok| {
        if let Token::Str(r#str) = tok {
            Ok(Literal::Str(r#str))
        } else {
            Err(Simple::expected_input_found(span, Vec::new(), Some(tok)))
        }
    });
    let lit = int.or(r#str);

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
        .map(|((name, generics), fields)| Stmt::Struct {
            name,
            generics,
            fields,
        });

    let arg = ident.then_ignore(just(Token::Colon)).then(ident);
    let args = arg.separated_by(just(Token::Comma)).delimited_by(
        just(Token::Open(Delimiter::Paren)),
        just(Token::Close(Delimiter::Paren)),
    );

    let block = just(Token::Open(Delimiter::Brace))
        .ignore_then(just(Token::Close(Delimiter::Brace)))
        .ignored();

    let func = just(Token::Func)
        .ignore_then(ident)
        .then(args)
        .then_ignore(block)
        .map(|(name, args)| Stmt::Func { name, args });

    (r#struct.or(func).map(Item::Stmt))
        .or(lit.map(Expr::Literal).map(Item::Expr))
        .repeated()
        .then_ignore(end())
}
