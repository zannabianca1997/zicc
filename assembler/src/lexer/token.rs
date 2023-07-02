use smartstring::{LazyCompact, SmartString};

#[derive(Debug, Clone, PartialEq, Eq)]
enum Token<'i> {
    Number(u64),
    Identifier(&'i str),
    Punctuator(Punctuator),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Punctuator {
    Comma,
    Colon,
    Plus,
    Minus,
    Star,
    Slash,
    Hashtag,
    At,
    ParenthesesOpen,
    ParenthesesClose,
    BracketOpen,
    BracketClose,
    BraceOpen,
    BraceClose,
}
