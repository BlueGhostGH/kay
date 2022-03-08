#![feature(string_remove_matches)]

mod syntax;

pub use syntax::{
    parse::parser,
    span::Span,
    token::{lexer, Token},
};
