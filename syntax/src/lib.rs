#![feature(string_remove_matches, trait_alias)]

mod ast;
mod error;
mod node;
mod parse;
mod span;
mod src;
mod token;

pub use crate::{
    parse::module_parser,
    span::Span,
    src::SrcId,
    token::{lexer, Token},
};
