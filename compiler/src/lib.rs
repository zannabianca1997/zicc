#![feature(box_patterns)]
#![feature(box_into_inner)]
#![feature(iterator_try_collect)]
#![feature(const_option)]
#![feature(never_type)]
#![feature(unwrap_infallible)]
#![feature(map_try_insert)]
#![feature(iter_next_chunk)]

pub mod types;

pub mod ast;

pub mod span {
    use nonmax::NonMaxU32;
    use string_interner::symbol::DefaultSymbol;

    #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct Pos {
        /// Source file, if any
        source: Option<DefaultSymbol>,
        /// Position in the source
        position: Option<NonMaxU32>,
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
        source: Option<DefaultSymbol>,
        /// Start position in the source
        start: Option<NonMaxU32>,
        /// End position in the source
        end: Option<NonMaxU32>,
    }

    pub trait Spanned {
        fn span(&self) -> Span;
    }
}
