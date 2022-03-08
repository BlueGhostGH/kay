#![feature(string_remove_matches, trait_alias)]

mod syntax;

pub use syntax::{
    parse::parser,
    span::Span,
    token::{lexer, Token},
};
