use std::{cmp, fmt, ops::Range};

use chumsky::Span as ChumskySpan;

#[derive(Clone)]
pub struct Span {
    context: String,
    range: (usize, usize),
}

impl Span {
    pub fn union(self, other: Self) -> Self {
        Self {
            range: (
                cmp::min(self.start(), other.start()),
                cmp::max(self.end(), other.end()),
            ),
            ..self
        }
    }
}

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({}..{})", self.range.0, self.range.1)
    }
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
