use std::fmt;

use chumsky::{
    self,
    error::Simple,
    primitive::{choice, end, filter, just},
    text::{ident, TextParser},
    Parser,
};

use super::span::Span;

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

pub fn lexer() -> impl chumsky::Parser<char, Vec<(Token, Span)>, Error = Simple<char, Span>> {
    let dec = filter(char::is_ascii_digit);
    let dec_ = just('_').or_not().ignore_then(dec);
    let dec_int = dec.chain(dec_.repeated());
    let int = dec_int.collect().map(Token::Int);

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

    let escape = just('\\').ignore_then(
        just('\\')
            .or(just('/'))
            .or(just('"'))
            .or(just('n').to('\n')),
    );

    let r#str = just('"')
        .ignore_then(filter(|ch| *ch != '\\' && *ch != '"').or(escape).repeated())
        .then_ignore(just('"'))
        .collect()
        .map(Token::Str);

    let word = ident().map(|s: String| match s.as_str() {
        "struct" => Token::Struct,
        "func" => Token::Func,
        _ => Token::Ident(s),
    });

    let token = choice((ctrl, delim, word, int, r#str))
        .map_with_span(|token, span| (token, span))
        .padded();

    token.repeated().padded().then_ignore(end())
}
