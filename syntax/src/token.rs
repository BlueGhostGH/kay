use std::fmt::{self, Write};

use internment::Intern;

use chumsky::{
    self,
    error::Error,
    primitive::{any, choice, end, filter, just},
    recovery::skip_then_retry_until,
    text::{ident, TextParser},
    Parser,
};

use crate::{ast, error, span::Span};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Delimiter {
    Paren,
    Brace,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum Token {
    Struct,
    Func,
    Ident(ast::Ident),

    Int(u128),
    Str(Intern<String>),

    Comma,
    Colon,
    Semicolon,
    Lt,
    Gt,
    Eq,
    RArrow,

    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    And,

    Open(Delimiter),
    Close(Delimiter),

    Error(char),
}

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::Struct => f.write_str("struct"),
            Token::Func => f.write_str("func"),
            Token::Ident(id) => write!(f, "ident({})", id),

            Token::Int(int) => write!(f, "int({})", int),
            Token::Str(str) => write!(f, "str(\"{}\")", str),

            Token::Comma => f.write_char(','),
            Token::Colon => f.write_char(':'),
            Token::Semicolon => f.write_char(';'),
            Token::Lt => f.write_char('<'),
            Token::Gt => f.write_char('>'),
            Token::Eq => f.write_char('='),
            Token::RArrow => f.write_str("->"),

            Token::Plus => f.write_char('+'),
            Token::Minus => f.write_char('-'),
            Token::Star => f.write_char('*'),
            Token::Slash => f.write_char('/'),
            Token::Percent => f.write_char('%'),
            Token::And => f.write_char('&'),

            Token::Open(Delimiter::Paren) => f.write_char('('),
            Token::Open(Delimiter::Brace) => f.write_char('{'),
            Token::Close(Delimiter::Paren) => f.write_char(')'),
            Token::Close(Delimiter::Brace) => f.write_char('}'),

            Token::Error(ch) => write!(f, "err('{}')", ch),
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

pub fn lexer() -> impl chumsky::Parser<char, Vec<(Token, Span)>, Error = error::Error> {
    let dec = filter(char::is_ascii_digit);
    let dec_ = just('_').or(dec);
    let dec_int = dec
        .chain(dec_.repeated())
        .collect::<String>()
        .try_map(|mut int, _span| {
            int.remove_matches('_');

            int.parse::<u128>().map_err(|_err| {
                // Simple::custom(span, format!("{}", err))
                todo!()
            })
        });
    let int = dec_int.map(Token::Int);

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
        .map(|str| Token::Str(Intern::new(str)));

    let lit = choice((int, r#str));

    let word = ident().map(|s: String| match s.as_str() {
        "struct" => Token::Struct,
        "func" => Token::Func,
        _ => Token::Ident(ast::Ident::new(s)),
    });

    let ctrl = choice((
        just(',').to(Token::Comma),
        just(':').to(Token::Colon),
        just(';').to(Token::Semicolon),
        just('<').to(Token::Lt),
        just('>').to(Token::Gt),
        just('=').to(Token::Eq),
        just("->").to(Token::RArrow),
    ));

    let op = choice((
        just('+').to(Token::Plus),
        just('-').to(Token::Minus),
        just('*').to(Token::Star),
        just('/').to(Token::Slash),
        just('%').to(Token::Percent),
        just('&').to(Token::And),
    ));

    let delim = choice((
        just('(').to(Token::Open(Delimiter::Paren)),
        just('{').to(Token::Open(Delimiter::Brace)),
        just(')').to(Token::Close(Delimiter::Paren)),
        just('}').to(Token::Close(Delimiter::Brace)),
    ));

    let token = choice((lit, word, ctrl, op, delim))
        .or(any().map(Token::Error).validate(|t, span, emit| {
            emit(error::Error::expected_input_found(span, None, Some(t)));
            t
        }))
        .map_with_span(|token, span| (token, span))
        .padded()
        .recover_with(skip_then_retry_until([]));

    token.repeated().padded().then_ignore(end())
}

#[cfg(test)]
mod tests {
    use chumsky::{Parser, Span};

    macro_rules! T {
        [struct] => {
            $crate::token::Token::Struct
        };
        [func] => {
            $crate::token::Token::Func
        };
        [ident($id:ident)] => {{
            let id = $crate::ast::Ident::new(stringify!($id));
            $crate::token::Token::Ident(id)
        }};

        [int($int:literal)] => {
            $crate::token::Token::Int($int)
        };
        [str($str:literal)] => {
            $crate::token::Token::Str(internment::Intern::new($str.into()))
        };

        [,] => {
            $crate::token::Token::Comma
        };
        [:] => {
            $crate::token::Token::Colon
        };
        [;] => {
            $crate::token::Token::Semicolon
        };
        [<] => {
            $crate::token::Token::Lt
        };
        [>] => {
            $crate::token::Token::Gt
        };
        [=] => {
            $crate::token::Token::Eq
        };
        [->] => {
            $crate::token::Token::RArrow
        };

        [+] => {
            $crate::token::Token::Plus
        };
        [-] => {
            $crate::token::Token::Minus
        };
        [*] => {
            $crate::token::Token::Star
        };
        [/] => {
            $crate::token::Token::Slash
        };
        [%] => {
            $crate::token::Token::Percent
        };
        [&] => {
            $crate::token::Token::And
        };

        [Open::Brace] => {
            $crate::token::Token::Open($crate::token::Delimiter::Brace)
        };
        [Close::Brace] => {
            $crate::token::Token::Close($crate::token::Delimiter::Brace)
        };
        [Open::Paren] => {
            $crate::token::Token::Open($crate::token::Delimiter::Paren)
        };
        [Close::Paren] => {
            $crate::token::Token::Close($crate::token::Delimiter::Paren)
        };

        [err($ch:literal)] => {
            $crate::token::Token::Error($ch)
        };
    }

    macro_rules! S {
        [$start:literal, $end:literal] => {
            $crate::span::Span::new($crate::src::SrcId::empty(), $start..$end)
        };
    }

    macro_rules! E {
        (unexpected($t:expr), $span:expr) => {{
            let kind = $crate::error::ErrorKind::Unexpected($crate::error::Pattern::Token($t));
            $crate::error::Error::new(kind, $span)
        }};
    }

    macro_rules! expect_lex {
        ($code:literal, [$($t:expr),*], $lexer_errs: ident) => {
            let code = $code;
            let len = code.chars().count();

            let span = |i| $crate::span::Span::new($crate::src::SrcId::empty(), i..i + 1);
            let (tokens, $lexer_errs) = $crate::token::lexer()
                .parse_recovery(chumsky::Stream::from_iter(
                    span(len),
                    code.chars().enumerate().map(|(i, c)| (c, span(i))),
                ));

            assert!(tokens.is_some());
            let tokens = tokens.unwrap();
            let tokens = tokens.into_iter().map(|(tok, _)| tok).collect::<Vec<_>>();

            let expected_tokens = [$($t),*];
            assert!(tokens.len() == expected_tokens.len());
            assert!(tokens.into_iter().zip(expected_tokens.into_iter()).all(|(t, et)| t == et));
        };
        ($code:literal, [$($t:expr),*]) => {
            expect_lex!($code, [$($t), *], _lexer_errs);
        };
        ($code:literal, [$($t:expr),*], [$($e:expr),*]) => {
            expect_lex!($code, [$($t),*], lexer_errs);
            dbg!(&lexer_errs);

            let expected_errs = [$($e),*];
            assert!(lexer_errs.len() == expected_errs.len());
            assert!(lexer_errs.into_iter().zip(expected_errs.into_iter()).all(|(e, ee)| e == ee));
        };
    }

    #[test]
    fn lits() {
        expect_lex!(
            "1 11 1_1 1_ 11_ \"hello\" \"\\\\ \\\"hello\\\" \\n\"",
            [
                T![int(1)],
                T![int(11)],
                T![int(11)],
                T![int(1)],
                T![int(11)],
                T![str("hello")],
                T![str("\\ \"hello\" \n")]
            ]
        );
    }

    #[test]
    fn words() {
        expect_lex!("struct func main", [T![struct], T![func], T![ident(main)]]);
    }

    #[test]
    fn ctrls() {
        expect_lex!(
            ", : ; < > = ->",
            [T![,], T![:], T![;], T![<], T![>], T![=], T![->]]
        );
    }

    #[test]
    fn ops() {
        expect_lex!("+ - * / % &", [T![+], T![-], T![*], T![/], T![%], T![&]]);
    }

    #[test]
    fn delims() {
        expect_lex!(
            "{ } ( )",
            [
                T![Open::Brace],
                T![Close::Brace],
                T![Open::Paren],
                T![Close::Paren]
            ]
        );
    }

    #[test]
    fn errs() {
        expect_lex!(
            "@ + $ -",
            [T![err('@')], T![+], T![err('$')], T![-]],
            [
                E![unexpected(T![err('@')]), S![0, 1]],
                E![unexpected(T![err('$')]), S![4, 5]]
            ]
        );
    }
}
