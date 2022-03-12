use std::fmt::{self, Write};

use chumsky::{
    self,
    error::Simple,
    primitive::{choice, end, filter, just},
    text::{ident, TextParser},
    Parser,
};

use crate::{ast, span::Span};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Delimiter {
    Paren,
    Brace,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum BinOp {
    Add,
    Sub,

    Mul,
    Div,
    Rem,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Token {
    Struct,
    Func,
    Ident(ast::Ident),

    Int(u128),
    Str(String),

    Comma,
    Colon,
    Semicolon,
    Lt,
    Gt,

    RArrow,

    Binary(BinOp),

    Open(Delimiter),
    Close(Delimiter),
}

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::Struct => f.write_str("struct"),
            Token::Func => f.write_str("func"),
            Token::Ident(id) => write!(f, "ident({})", id),

            Token::Int(int) => write!(f, "int({})", int),
            Token::Str(str) => write!(f, "str({})", str),

            Token::Comma => f.write_char(','),
            Token::Colon => f.write_char(':'),
            Token::Semicolon => f.write_char(';'),
            Token::Lt => f.write_char('<'),
            Token::Gt => f.write_char('>'),

            Token::RArrow => f.write_str("->"),

            Token::Binary(BinOp::Add) => f.write_char('+'),
            Token::Binary(BinOp::Sub) => f.write_char('-'),
            Token::Binary(BinOp::Mul) => f.write_char('*'),
            Token::Binary(BinOp::Div) => f.write_char('/'),
            Token::Binary(BinOp::Rem) => f.write_char('%'),

            Token::Open(Delimiter::Paren) => f.write_char('('),
            Token::Open(Delimiter::Brace) => f.write_char('{'),
            Token::Close(Delimiter::Paren) => f.write_char(')'),
            Token::Close(Delimiter::Brace) => f.write_char('}'),
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

pub fn lexer() -> impl chumsky::Parser<char, Vec<(Token, Span)>, Error = Simple<char, Span>> {
    let dec = filter(char::is_ascii_digit);
    let dec_ = just('_').or_not().ignore_then(dec);
    let dec_int = dec
        .chain(dec_.repeated())
        .collect::<String>()
        .try_map(|mut int, span| {
            int.remove_matches('_');

            int.parse::<u128>()
                .map_err(|err| Simple::custom(span, format!("{}", err)))
        });
    let int = dec_int.map(Token::Int);

    let ctrl = choice((
        just(',').to(Token::Comma),
        just(':').to(Token::Colon),
        just(';').to(Token::Semicolon),
        just('<').to(Token::Lt),
        just('>').to(Token::Gt),
        just("->").to(Token::RArrow),
    ));

    let op = choice((
        just('+').to(Token::Binary(BinOp::Add)),
        just('-').to(Token::Binary(BinOp::Sub)),
        just('*').to(Token::Binary(BinOp::Mul)),
        just('/').to(Token::Binary(BinOp::Div)),
        just('%').to(Token::Binary(BinOp::Rem)),
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
        _ => Token::Ident(ast::Ident::new(s)),
    });

    let token = choice((ctrl, op, delim, word, int, r#str))
        .map_with_span(|token, span| (token, span))
        .padded();

    token.repeated().padded().then_ignore(end())
}
