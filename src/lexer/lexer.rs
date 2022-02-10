#[derive(Debug)]
pub struct Lexer<'src> {
    pub pos: usize,
    pub src: &'src str,
}

impl<'src> Lexer<'src> {
    pub fn new(input: &'src str) -> Self {
        Self { pos: 0, src: input }
    }
}
