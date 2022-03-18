use std::{cmp, fmt, ops::Range};

use crate::src::SrcId;

#[derive(Clone, Copy)]
pub struct Span {
    context: SrcId,
    range: (usize, usize),
}

impl Span {
    pub fn union(self, other: Self) -> Self {
        use chumsky::Span;

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
    type Context = SrcId;
    type Offset = usize;

    fn new(context: SrcId, range: Range<usize>) -> Self {
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
