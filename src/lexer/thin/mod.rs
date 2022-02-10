mod cursor;

use cursor::{Cursor, EOF_CHAR};

#[derive(Debug, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub len: usize,
}

#[derive(Debug, PartialEq, Eq)]
pub enum TokenKind {
    LineComment,
    BlockComment {
        terminated: bool,
    },
    Whitespace,
    Ident,
    Literal {
        kind: LiteralKind,
        suffix_start: usize,
    },

    Semi,
    Colon,
    Comma,
    Dot,
    OpenParen,
    CloseParen,
    OpenBracket,
    CloseBracket,
    OpenBrace,
    CloseBrace,
    Eq,
    Lt,
    Gt,
    Bang,
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    And,
    Or,
    Caret,

    Unknown,
}

use TokenKind::*;

#[derive(Debug, PartialEq, Eq)]
pub enum LiteralKind {
    Int {
        base: Base,
        empty_int: bool,
    },
    Float {
        base: Base,
        empty_exponent: bool,
    },
    Char {
        terminated: bool,
    },
    Byte {
        terminated: bool,
    },
    Str {
        terminated: bool,
    },
    ByteStr {
        terminated: bool,
    },
    RawStr {
        n_hashes: u16,
        err: Option<RawStrError>,
    },
    RawByteStr {
        n_hashes: u16,
        err: Option<RawStrError>,
    },
}

use LiteralKind::*;

#[derive(Debug, PartialEq, Eq)]
pub enum RawStrError {
    InvalidStarter {
        bad_char: char,
    },
    NoTerminator {
        expected: usize,
        found: usize,
        possible_terminator_offset: Option<usize>,
    },
    TooManyDelimiters {
        found: usize,
    },
}

#[derive(Debug, PartialEq, Eq)]
pub enum Base {
    Binary,
    Octal,
    Hexadecimal,
    Decimal,
}

pub fn first_token(input: &str) -> Token {
    Cursor::new(input).advance_token()
}

fn is_whitespace(ch: char) -> bool {
    matches!(
        ch,
        '\u{0009}'   // \t
        | '\u{000A}' // \n
        | '\u{000B}' // vertical tab
        | '\u{000C}' // form feed
        | '\u{000D}' // \r
        | '\u{0020}' // space
    )
}

fn is_id_start(ch: char) -> bool {
    matches!(ch, '_' | 'A' ..= 'Z' | 'a' ..= 'z')
}

fn is_id_continue(ch: char) -> bool {
    is_id_start(ch) || matches!(ch, '0'..='9')
}

impl Cursor<'_> {
    fn advance_token(&mut self) -> Token {
        let first_ch = self.bump().unwrap();

        let kind = match first_ch {
            '/' => match self.first() {
                '/' => self.line_comment(),
                '*' => self.block_comment(),
                _ => Slash,
            },

            ch if is_whitespace(ch) => self.whitespace(),

            'r' => match self.first() {
                '#' | '"' => {
                    let (n_hashes, err) = self.raw_double_quoted_string(1);
                    let suffix_start = self.len_consumed();

                    if err.is_none() {
                        self.eat_literal_suffix();
                    }

                    let kind = RawStr { n_hashes, err };

                    Literal { kind, suffix_start }
                }
                _ => self.ident(),
            },

            'b' => match (self.first(), self.second()) {
                ('\'', _) => {
                    self.bump();
                    let terminated = self.single_quoted_string();
                    let kind = Byte { terminated };
                    let suffix_start = self.len_consumed();

                    if terminated {
                        self.eat_literal_suffix();
                    }

                    Literal { kind, suffix_start }
                }
                ('"', _) => {
                    self.bump();
                    let terminated = self.double_quoted_string();
                    let kind = ByteStr { terminated };
                    let suffix_start = self.len_consumed();

                    if terminated {
                        self.eat_literal_suffix();
                    }

                    Literal { kind, suffix_start }
                }
                ('r', '"') | ('r', '#') => {
                    self.bump();
                    let (n_hashes, err) = self.raw_double_quoted_string(2);
                    let suffix_start = self.len_consumed();

                    if err.is_none() {
                        self.eat_literal_suffix();
                    }

                    let kind = RawByteStr { n_hashes, err };

                    Literal { kind, suffix_start }
                }
                _ => self.ident(),
            },

            ch if is_id_start(ch) => self.ident(),

            ch @ '0'..='9' => {
                let kind = self.number(ch);
                let suffix_start = self.len_consumed();

                self.eat_literal_suffix();

                TokenKind::Literal { kind, suffix_start }
            }

            ';' => Semi,
            ':' => Colon,
            ',' => Comma,
            '.' => Dot,
            '(' => OpenParen,
            ')' => CloseParen,
            '[' => OpenBracket,
            ']' => CloseBracket,
            '{' => OpenBrace,
            '}' => CloseBrace,
            '=' => Eq,
            '<' => Lt,
            '>' => Gt,
            '!' => Bang,
            '+' => Plus,
            '-' => Minus,
            '*' => Star,
            '%' => Percent,
            '&' => And,
            '|' => Or,
            '^' => Caret,

            '\'' => {
                let terminated = self.single_quoted_string();
                let kind = Char { terminated };
                let suffix_start = self.len_consumed();

                if terminated {
                    self.eat_literal_suffix();
                }

                Literal { kind, suffix_start }
            }

            '"' => {
                let terminated = self.double_quoted_string();
                let kind = Str { terminated };
                let suffix_start = self.len_consumed();

                if terminated {
                    self.eat_literal_suffix();
                }

                Literal { kind, suffix_start }
            }

            _ => Unknown,
        };

        let len = self.len_consumed();

        Token { kind, len }
    }

    fn line_comment(&mut self) -> TokenKind {
        self.bump();

        self.eat_while(|ch| ch != '\n');

        LineComment
    }

    fn block_comment(&mut self) -> TokenKind {
        self.bump();

        let mut depth = 1usize;
        while let Some(ch) = self.bump() {
            match ch {
                '/' if self.first() == '*' => {
                    self.bump();
                    depth += 1;
                }
                '*' if self.first() == '/' => {
                    self.bump();
                    depth -= 1;

                    if depth == 0 {
                        break;
                    }
                }
                _ => {}
            }
        }

        BlockComment {
            terminated: depth == 0,
        }
    }

    fn whitespace(&mut self) -> TokenKind {
        self.eat_while(is_whitespace);

        Whitespace
    }

    fn ident(&mut self) -> TokenKind {
        self.eat_while(is_id_continue);

        Ident
    }

    fn number(&mut self, first_digit: char) -> LiteralKind {
        let mut base = Base::Decimal;
        if first_digit == '0' {
            let has_digits = match self.first() {
                'b' => {
                    self.bump();
                    base = Base::Binary;
                    self.eat_decimal_digits()
                }
                'o' => {
                    self.bump();
                    base = Base::Octal;
                    self.eat_decimal_digits()
                }
                'x' => {
                    self.bump();
                    base = Base::Hexadecimal;
                    self.eat_hexadecimal_digits()
                }
                '0'..='9' | '_' | '.' | 'e' | 'E' => {
                    self.eat_decimal_digits();
                    true
                }
                _ => {
                    return Int {
                        base,
                        empty_int: false,
                    }
                }
            };

            if !has_digits {
                return Int {
                    base,
                    empty_int: true,
                };
            }
        } else {
            self.eat_decimal_digits();
        }

        match self.first() {
            '.' if self.second() != '.' && !is_id_start(self.second()) => {
                self.bump();

                let mut empty_exponent = false;

                if self.first().is_digit(10) {
                    self.eat_decimal_digits();
                    match self.first() {
                        'e' | 'E' => {
                            self.bump();
                            empty_exponent = !self.eat_float_exponent();
                        }
                        _ => {}
                    }
                }

                Float {
                    base,
                    empty_exponent,
                }
            }
            'e' | 'E' => {
                self.bump();
                let empty_exponent = !self.eat_float_exponent();

                Float {
                    base,
                    empty_exponent,
                }
            }
            _ => Int {
                base,
                empty_int: false,
            },
        }
    }

    fn single_quoted_string(&mut self) -> bool {
        if self.second() == '\'' && self.first() != '\\' {
            self.bump();
            self.bump();
            return true;
        }

        loop {
            match self.first() {
                '\'' => {
                    self.bump();
                    return true;
                }
                '/' => break,
                '\n' if self.second() != '\'' => break,
                EOF_CHAR if self.is_eof() => break,
                '\\' => {
                    self.bump();
                    self.bump();
                }
                _ => {
                    self.bump();
                }
            }
        }

        false
    }

    fn double_quoted_string(&mut self) -> bool {
        while let Some(c) = self.bump() {
            match c {
                '"' => {
                    return true;
                }
                '\\' if self.first() == '\\' || self.first() == '"' => {
                    self.bump();
                }
                _ => {}
            }
        }

        false
    }

    fn raw_double_quoted_string(&mut self, prefix_len: usize) -> (u16, Option<RawStrError>) {
        let (n_hashes, err) = self.raw_string_unvalidated(prefix_len);

        if let Ok(num) = u16::try_from(n_hashes) {
            (num, err)
        } else {
            (0, Some(RawStrError::TooManyDelimiters { found: n_hashes }))
        }
    }

    fn raw_string_unvalidated(&mut self, prefix_len: usize) -> (usize, Option<RawStrError>) {
        let start_pos = self.len_consumed();
        let mut possible_terminator_offset = None;
        let mut max_hashes = 0;

        let mut eaten = 0;
        while self.first() == '#' {
            eaten += 1;
            self.bump();
        }
        let n_start_hashes = eaten;

        match self.bump().unwrap_or(EOF_CHAR) {
            '"' => {}
            ch => {
                return (
                    n_start_hashes,
                    Some(RawStrError::InvalidStarter { bad_char: ch }),
                )
            }
        }

        loop {
            self.eat_while(|ch| ch != '"');

            if self.is_eof() {
                return (
                    n_start_hashes,
                    Some(RawStrError::NoTerminator {
                        expected: n_start_hashes,
                        found: max_hashes,
                        possible_terminator_offset,
                    }),
                );
            }

            self.bump();

            let mut n_end_hashes = 0;
            while self.first() == '#' && n_end_hashes < n_start_hashes {
                n_end_hashes += 1;
                self.bump();
            }

            if n_end_hashes == n_start_hashes {
                return (n_start_hashes, None);
            } else if n_end_hashes > max_hashes {
                possible_terminator_offset =
                    Some(self.len_consumed() - start_pos - n_end_hashes + prefix_len);
                max_hashes = n_end_hashes;
            }
        }
    }

    fn eat_decimal_digits(&mut self) -> bool {
        let mut has_digits = false;

        loop {
            match self.first() {
                '_' => {
                    self.bump();
                }
                '0'..='9' => {
                    has_digits = true;
                    self.bump();
                }
                _ => break,
            }
        }

        has_digits
    }

    fn eat_hexadecimal_digits(&mut self) -> bool {
        let mut has_digits = false;

        loop {
            match self.first() {
                '_' => {
                    self.bump();
                }
                '0'..='9' | 'a'..='f' | 'A'..='F' => {
                    has_digits = true;
                    self.bump();
                }
                _ => break,
            }
        }

        has_digits
    }

    fn eat_float_exponent(&mut self) -> bool {
        if self.first() == '-' || self.first() == '+' {
            self.bump();
        }

        self.eat_decimal_digits()
    }

    fn eat_literal_suffix(&mut self) {
        self.eat_identifier();
    }

    fn eat_identifier(&mut self) {
        if !is_id_start(self.first()) {
            return;
        }
        self.bump();

        self.eat_while(is_id_continue);
    }
}
