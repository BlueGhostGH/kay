use std::ops::Range;

#[derive(Debug, Clone)]
pub struct Span {
    context: String,
    range: (usize, usize),
}

impl chumsky::Span for Span {
    type Context = String;
    type Offset = usize;

    fn new(context: String, range: Range<usize>) -> Self {
        Self {
            context,
            range: (range.start, range.end),
        }
    }

    fn context(&self) -> Self::Context {
        self.context.clone()
    }
    fn start(&self) -> Self::Offset {
        self.range.0
    }
    fn end(&self) -> Self::Offset {
        self.range.1
    }
}
