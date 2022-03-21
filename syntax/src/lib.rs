#![feature(string_remove_matches, trait_alias)]

pub mod ast;
pub mod error;
pub mod node;
pub mod parse;
pub mod span;
pub mod src;
pub mod token;

use chumsky::{Parser, Span, Stream};

use {error::Error, node::SrcNode, src::SrcId};

fn parse<T, P>(parser: P, code: &str, src: SrcId) -> (Option<T>, Vec<Error>)
where
    P: parse::helper::Parser<T>,
{
    let mut errs = Vec::new();

    let len = code.chars().count();
    let eoi = span::Span::new(src, len..len);

    let (tokens, mut lexer_errs) = token::lexer().parse_recovery(Stream::from_iter(
        eoi,
        code.chars()
            .enumerate()
            .map(|(i, c)| (c, span::Span::new(src, i..i + 1))),
    ));
    errs.append(&mut lexer_errs);

    let tokens = if let Some(tokens) = tokens {
        tokens
    } else {
        return (None, errs);
    };

    let (output, mut parse_errs) =
        parser.parse_recovery(Stream::from_iter(eoi, tokens.into_iter()));
    errs.append(&mut parse_errs);

    (output, errs)
}

pub fn parse_module(code: &str, src: SrcId) -> (Option<SrcNode<ast::Module>>, Vec<Error>) {
    parse(
        parse::module_parser().map_with_span(SrcNode::new),
        code,
        src,
    )
}
