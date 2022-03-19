use std::collections::HashSet;

use crate::{span::Span, token::Token};

#[derive(Debug, PartialEq)]
pub enum ErrorKind {
    UnexpectedEnd,
    Unexpected(Pattern),
}

#[derive(Debug)]
pub struct Error {
    #[allow(dead_code)]
    kind: ErrorKind,
    #[allow(dead_code)]
    span: Span,
    expected: HashSet<Pattern>,
    label: Option<&'static str>,
}

impl Error {
    pub fn new(kind: ErrorKind, span: Span) -> Self {
        Self {
            kind,
            span,
            expected: HashSet::new(),
            label: None,
        }
    }

    pub fn expected(mut self, pat: Pattern) -> Self {
        self.expected.insert(pat);
        self
    }

    fn merge(mut self, other: Self) -> Self {
        for expected in other.expected {
            self.expected.insert(expected);
        }
        self
    }
}

impl PartialEq for Error {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind && self.span == other.span && self.label == other.label
    }
}

impl<T: Into<Pattern>> chumsky::Error<T> for Error {
    type Span = Span;
    type Label = &'static str;

    fn expected_input_found<Iter: IntoIterator<Item = Option<T>>>(
        span: Self::Span,
        expected: Iter,
        found: Option<T>,
    ) -> Self {
        let kind = found
            .map(Into::into)
            .map(ErrorKind::Unexpected)
            .unwrap_or(ErrorKind::UnexpectedEnd);
        let expected = expected
            .into_iter()
            .map(|ex| ex.map(Into::into).unwrap_or(Pattern::End))
            .collect();

        Self {
            kind,
            span,
            expected,
            label: None,
        }
    }

    fn with_label(mut self, label: Self::Label) -> Self {
        self.label.get_or_insert(label);
        self
    }

    fn merge(self, other: Self) -> Self {
        self.merge(other)
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum Pattern {
    Char(char),
    Token(Token),
    Literal,
    Ident,
    End,
}

impl From<char> for Pattern {
    fn from(ch: char) -> Self {
        Self::Char(ch)
    }
}

impl From<Token> for Pattern {
    fn from(tok: Token) -> Self {
        Self::Token(tok)
    }
}
