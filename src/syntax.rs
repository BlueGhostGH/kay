use std::fmt;

use chumsky::{
    error::Simple,
    primitive::{choice, end, filter_map, just},
    text::{ident, keyword, TextParser},
    Error, Parser,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Delimiter {
    Brace,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Token {
    Struct,
    Ident(String),

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
            Token::Ident(id) => write!(f, "ident({})", id),
            Token::Comma => write!(f, ","),
            Token::Colon => write!(f, ":"),
            Token::Semicolon => write!(f, ";"),
            Token::Lt => write!(f, "<"),
            Token::Gt => write!(f, ">"),
            Token::Open(Delimiter::Brace) => write!(f, "{{"),
            Token::Close(Delimiter::Brace) => write!(f, "}}"),
        }
    }
}

#[derive(Debug)]
pub enum Stmt {
    Struct {
        name: String,
        generics: Option<Vec<String>>,
        fields: Option<Vec<(String, String)>>,
    },
}

pub fn lexer() -> impl chumsky::Parser<char, Vec<Token>, Error = Simple<char>> {
    let ctrl = choice((
        just(',').to(Token::Comma),
        just(':').to(Token::Colon),
        just(';').to(Token::Semicolon),
        just('<').to(Token::Lt),
        just('>').to(Token::Gt),
    ));

    let delim = choice((
        just('{').to(Token::Open(Delimiter::Brace)),
        just('}').to(Token::Close(Delimiter::Brace)),
    ));

    let kw = keyword("struct").to(Token::Struct);

    let ident = ident().map(|id| Token::Ident(id));

    kw.or(ident)
        .or(ctrl)
        .or(delim)
        .padded()
        .repeated()
        .then_ignore(end())
}

pub fn parser() -> impl chumsky::Parser<Token, Vec<Stmt>, Error = Simple<Token>> {
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
        .map(|((name, generics), fields)| Stmt::Struct {
            name,
            generics,
            fields,
        });

    r#struct.repeated().then_ignore(end())
}
