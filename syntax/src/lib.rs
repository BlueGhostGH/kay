#![feature(string_remove_matches, trait_alias)]

mod node;
mod parse;
mod span;
mod token;

pub use crate::{
    parse::parser,
    span::Span,
    token::{lexer, Token},
};
