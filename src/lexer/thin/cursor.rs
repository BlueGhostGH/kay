use std::str;

#[derive(Debug)]
pub struct Cursor<'src> {
    initial_len: usize,
    chars: str::Chars<'src>,
}

pub const EOF_CHAR: char = '\0';

impl<'src> Cursor<'src> {
    pub fn new(input: &'src str) -> Self {
        Self {
            initial_len: input.len(),
            chars: input.chars(),
        }
    }

    pub fn first(&self) -> char {
        self.chars.clone().next().unwrap_or(EOF_CHAR)
    }

    pub fn second(&self) -> char {
        let mut it = self.chars.clone();
        it.next();
        it.next().unwrap_or(EOF_CHAR)
    }

    pub fn is_eof(&self) -> bool {
        self.chars.as_str().is_empty()
    }

    pub fn len_consumed(&self) -> usize {
        self.initial_len - self.chars.as_str().len()
    }

    pub fn reset_len_consumed(&mut self) {
        self.initial_len = self.chars.as_str().len();
    }

    pub fn bump(&mut self) -> Option<char> {
        self.chars.next()
    }

    pub fn eat_while<P>(&mut self, predicate: P)
    where
        P: Fn(char) -> bool,
    {
        while predicate(self.first()) && !self.is_eof() {
            self.bump();
        }
    }
}
