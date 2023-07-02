/// Represent a span of text
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span(usize, usize);

impl Span {
    #[must_use]
    #[inline]
    pub(super) const fn new(start: usize, end: usize) -> Self {
        debug_assert!(start <= end);
        Self(start, end)
    }

    #[must_use]
    #[inline]
    pub const fn join(self, other: Span) -> Self {
        Self::new(usize::min(self.0, other.0), usize::max(self.1, other.1))
    }

    pub fn len(&self) -> usize {
        self.1 - self.0
    }
}

// Something that has a span
pub trait Spanned {
    #[must_use]
    fn span(&self) -> Span;
}
