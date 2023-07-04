/// Represent a span of text
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum Span {
    Span(usize, usize),
    #[default]
    Empty,
}

impl Span {
    #[must_use]
    #[inline]
    pub(super) const fn new(start: usize, end: usize) -> Self {
        debug_assert!(start <= end);
        Self::Span(start, end)
    }

    #[must_use]
    #[inline]
    pub const fn join(self, other: Span) -> Self {
        match (self, other) {
            (Span::Span(a, b), Span::Span(c, d)) => Self::Span(Ord::min(a, c), Ord::max(b, d)),
            (s @ Span::Span(..), Span::Empty) | (Span::Empty, s @ Span::Span(..)) => s,
            (Span::Empty, Span::Empty) => Span::Empty,
        }
    }

    #[must_use]
    #[inline]
    pub fn len(&self) -> usize {
        match self {
            Span::Span(a, b) => b - a,
            Span::Empty => 0,
        }
    }
}

// Something that has a span
pub trait Spanned {
    #[must_use]
    fn span(&self) -> Span;
}

impl<T> Spanned for [T]
where
    T: Spanned,
{
    fn span(&self) -> Span {
        self.iter()
            .map(Spanned::span)
            .reduce(Span::join)
            .unwrap_or_default()
    }
}
