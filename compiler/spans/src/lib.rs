use nonmax::NonMaxU32;
use string_interner::symbol::DefaultSymbol;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Pos {
    /// Source file, if any
    pub source: Option<DefaultSymbol>,
    /// Position in the source
    pub position: Option<NonMaxU32>,
}
impl Pos {
    pub fn missing() -> Self {
        Self {
            source: None,
            position: None,
        }
    }
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Span {
    /// Source file, if any
    pub source: Option<DefaultSymbol>,
    /// Start position in the source
    pub start: Option<NonMaxU32>,
    /// End position in the source
    pub end: Option<NonMaxU32>,
}

pub trait Spanned {
    fn span(&self) -> Span;
}
