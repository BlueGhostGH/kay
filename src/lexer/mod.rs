mod thin;

mod lexer;

use std::result;

use lexer::Lexer;

#[derive(Debug, PartialEq, Eq)]
pub struct Token {
    kind: TokenKind,
    span: Span,
}

#[derive(Debug, PartialEq, Eq)]
struct Span {
    start: usize,
    end: usize,
}

impl Span {
    fn mk_sp(start: usize, end: usize) -> Self {
        Self { start, end }
    }
}

#[derive(Debug, PartialEq, Eq)]
enum TokenKind {
    Eq,
    EqEq,
    Lt,
    Le,
    Gt,
    Ge,
    Not,
    Ne,
    AndAnd,
    OrOr,

    Semi,
    Colon,
    Comma,
    Dot,
    ModSep,
    RArrow,
    LArrow,
    FatArrow,

    Ident,

    Eof,
}

use TokenKind::*;

enum LiteralKind {
    Char,
    Byte,
    Str,
    ByteStr,
}

use LiteralKind::*;

#[derive(Debug)]
pub struct Error {
    span: Span,
    kind: ErrorKind,
}

#[derive(Debug)]
enum ErrorKind {
    UnterminatedBlockComment,
    UnterminatedCharacterLiteral,
    UnterminatedByteConstant,
    UnterminatedDoubleQuoteString,
    UnterminatedDoubleQuoteByteString,
}

use ErrorKind::*;

type Result<T> = result::Result<T, Error>;

#[derive(Debug)]
pub struct Tokens<'src> {
    lexer: Lexer<'src>,
    eof: bool,
}

impl Iterator for Tokens<'_> {
    type Item = Result<Token>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.eof {
            None
        } else {
            let token = self.lexer.next_token();

            if matches!(token, Ok(Token { kind: Eof, .. })) {
                self.eof = true;
            }

            Some(token)
        }
    }
}

pub fn tokenize(input: &str) -> Tokens<'_> {
    Tokens {
        lexer: Lexer::new(input),
        eof: false,
    }
}

impl Lexer<'_> {
    fn next_token(&mut self) -> Result<Token> {
        loop {
            if self.src.is_empty() {
                let span = Span::mk_sp(self.pos, self.pos);

                return Ok(Token { kind: Eof, span });
            }

            let token = thin::first_token(self.src);

            let start = self.pos;
            self.pos += token.len;

            if let Some(kind) = self.cook_lexer_token(token.kind, start)? {
                let span = Span::mk_sp(start, self.pos);

                return Ok(Token { kind, span });
            }
        }
    }

    fn cook_lexer_token(
        &mut self,
        token: thin::TokenKind,
        start: usize,
    ) -> Result<Option<TokenKind>> {
        let kind = match token {
            thin::TokenKind::LineComment => Ok(None),
            thin::TokenKind::BlockComment { terminated } => {
                if terminated {
                    Ok(None)
                } else {
                    let span = Span::mk_sp(start, self.pos);
                    Err(Error {
                        kind: UnterminatedBlockComment,
                        span,
                    })
                }
            }
            thin::TokenKind::Ident => Ok(Some(Ident)),
            thin::TokenKind::Whitespace => Ok(None),
            thin::TokenKind::Literal { kind, suffix_start } => {
                let suffix_start = start + suffix_start;
                let kind = self.cook_lexer_literal(start, suffix_start, kind)?;

                unimplemented!()
            }
            _ => unimplemented!(),
        }?;

        Ok(kind)
    }

    fn cook_lexer_literal(
        &self,
        start: usize,
        suffix_start: usize,
        kind: thin::LiteralKind,
    ) -> Result<LiteralKind> {
        match kind {
            thin::LiteralKind::Char { terminated } => {
                if terminated {
                    Ok(Char)
                } else {
                    let span = Span::mk_sp(start, suffix_start);
                    Err(Error {
                        kind: UnterminatedCharacterLiteral,
                        span,
                    })
                }
            }
            thin::LiteralKind::Byte { terminated } => {
                if terminated {
                    Ok(Byte)
                } else {
                    let span = Span::mk_sp(start + 1, suffix_start);
                    Err(Error {
                        kind: UnterminatedByteConstant,
                        span,
                    })
                }
            }
            thin::LiteralKind::Str { terminated } => {
                if terminated {
                    Ok(Str)
                } else {
                    let span = Span::mk_sp(start, suffix_start);
                    Err(Error {
                        kind: UnterminatedDoubleQuoteString,
                        span,
                    })
                }
            }
            thin::LiteralKind::ByteStr { terminated } => {
                if terminated {
                    Ok(ByteStr)
                } else {
                    let span = Span::mk_sp(start + 1, suffix_start);
                    Err(Error {
                        kind: UnterminatedDoubleQuoteByteString,
                        span,
                    })
                }
            }
            _ => unimplemented!(),
        }
    }
}
