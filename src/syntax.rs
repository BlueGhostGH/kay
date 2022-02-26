use std::fmt;

use chumsky::{
    error::Simple,
    primitive::{choice, end, filter, filter_map, just},
    text::{ident, keyword, TextParser},
    Error, Parser,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Delimiter {
    Paren,
    Brace,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Token {
    Struct,
    Func,
    Ident(String),

    Int(String),
    Str(String),

    Comma,
    Colon,
    Semicolon,
    Lt,
    Gt,

    Open(Delimiter),
    Close(Delimiter),
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::Struct => write!(f, "struct"),
            Token::Func => write!(f, "func"),
            Token::Ident(id) => write!(f, "ident({})", id),

            Token::Int(int) => write!(f, "int({})", int),
            Token::Str(str) => write!(f, "str({})", str),

            Token::Comma => write!(f, ","),
            Token::Colon => write!(f, ":"),
            Token::Semicolon => write!(f, ";"),
            Token::Lt => write!(f, "<"),
            Token::Gt => write!(f, ">"),

            Token::Open(Delimiter::Paren) => write!(f, "("),
            Token::Open(Delimiter::Brace) => write!(f, "{{"),
            Token::Close(Delimiter::Paren) => write!(f, ")"),
            Token::Close(Delimiter::Brace) => write!(f, "}}"),
        }
    }
}

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

pub fn lexer() -> impl chumsky::Parser<char, Vec<Token>, Error = Simple<char>> {
    let kw = choice((
        keyword("struct").to(Token::Struct),
        keyword("func").to(Token::Func),
    ));

    let ident = ident().map(|id| Token::Ident(id));

    let dec = filter(char::is_ascii_digit);
    let dec_ = just('_').or_not().ignore_then(dec);
    let dec_int = dec.chain(dec_.repeated());
    let int = dec_int.collect().map(|int| Token::Int(int));

    let r#str = just('"')
        .ignore_then(filter(|ch| *ch != '"').repeated())
        .then_ignore(just('"'))
        .collect()
        .map(|str| Token::Str(str));

    let ctrl = choice((
        just(',').to(Token::Comma),
        just(':').to(Token::Colon),
        just(';').to(Token::Semicolon),
        just('<').to(Token::Lt),
        just('>').to(Token::Gt),
    ));

    let delim = choice((
        just('(').to(Token::Open(Delimiter::Paren)),
        just('{').to(Token::Open(Delimiter::Brace)),
        just(')').to(Token::Close(Delimiter::Paren)),
        just('}').to(Token::Close(Delimiter::Brace)),
    ));

    kw.or(ident)
        .or(int)
        .or(r#str)
        .or(ctrl)
        .or(delim)
        .padded()
        .repeated()
        .then_ignore(end())
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
