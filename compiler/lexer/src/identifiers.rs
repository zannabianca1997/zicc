use std::hash::BuildHasher;

use lazy_regex::{Lazy, Regex, regex};
use string_interner::{DefaultSymbol, StringInterner};

use crate::Token;

type Symbol = DefaultSymbol;

/// Regular expression to match identifiers
pub static RE: &Lazy<Regex> = regex!(r#"^(?:[\w&&[^\d_]]\w*|_+[\w&&[^_]]\w*)$"#);

/// An identifier
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Identifier(Symbol);

impl Identifier {
    /// Create a new identifier
    ///
    /// This will validate the given string against the regex [`RE`], and return
    /// [`Some`] only if a match is found
    pub fn new<B, H>(value: &str, interner: &mut StringInterner<B, H>) -> Option<Self>
    where
        B: string_interner::backend::Backend<Symbol = Symbol>,
        H: BuildHasher,
    {
        if !RE.is_match(value) {
            return None;
        }
        Some(Self(interner.get_or_intern(value)))
    }
}

impl TryFrom<Token> for Identifier {
    type Error = Token;
    fn try_from(value: Token) -> Result<Self, Token> {
        if let Token::Identifier(value) = value {
            Ok(value)
        } else {
            Err(value)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::RE;

    /// Returns true if the string is matched by RE
    fn is_valid(s: &str) -> bool {
        RE.is_match(s)
    }

    /// Simple letter-only identifiers should match
    #[test]
    fn letter_only_identifier_should_match() {
        assert!(is_valid("foo"));
        assert!(is_valid("Bar"));
        assert!(is_valid("x"));
    }

    /// Identifiers with letters and digits should match
    #[test]
    fn alphanumeric_identifier_should_match() {
        assert!(is_valid("foo1"));
        assert!(is_valid("myVar2"));
        assert!(is_valid("a0b1c2"));
    }

    /// Identifiers with internal underscores should match
    #[test]
    fn identifier_with_internal_underscore_should_match() {
        assert!(is_valid("foo_bar"));
        assert!(is_valid("my_var_1"));
    }

    /// Identifiers with a leading underscore followed by a non-underscore should match
    #[test]
    fn underscore_prefixed_identifier_should_match() {
        assert!(is_valid("_foo"));
        assert!(is_valid("_bar1"));
        assert!(is_valid("__baz"));
        assert!(is_valid("_1"));
    }

    /// Bare underscore(s) with no following non-underscore char should not match
    #[test]
    fn bare_underscores_should_not_match() {
        assert!(!is_valid("_"));
        assert!(!is_valid("__"));
        assert!(!is_valid("___"));
    }

    /// Identifiers starting with a digit should not match
    #[test]
    fn digit_leading_identifier_should_not_match() {
        assert!(!is_valid("1foo"));
        assert!(!is_valid("0bar"));
        assert!(!is_valid("9"));
    }

    /// The empty string should not match
    #[test]
    fn empty_string_should_not_match() {
        assert!(!is_valid(""));
    }

    /// A valid identifier embedded in a longer string should not be a full match
    #[test]
    fn identifier_with_surrounding_chars_should_not_match() {
        assert!(!is_valid(" foo"));
        assert!(!is_valid("foo!"));
        assert!(!is_valid("foo bar"));
    }
}
