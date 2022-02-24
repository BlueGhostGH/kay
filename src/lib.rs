#![feature(string_remove_matches)]

mod syntax;

pub use syntax::{lexer, parser, Token};
