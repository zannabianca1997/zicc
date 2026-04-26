use std::{fmt::Display, ops::Neg, str::FromStr};

use lazy_regex::{Lazy, Regex, regex};
use zicc_limits::{ParseValueError, Value};

use crate::Token;

/// Regular expression to match integer literals
pub static RE: &Lazy<Regex> = regex!(r#"^(?:-|\+)?\d+$"#);

/// An integer literal
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct IntLiteral(Value);

impl IntLiteral {
    /// Parse an integer literal
    ///
    /// This will validate the given string against the regex [`RE`], and return
    /// [`Ok`] only if a match is found
    pub fn parse(value: &str) -> Result<Self, ParseValueError> {
        value.parse().map(Self)
    }

    pub fn is_negative(&self) -> bool {
        self.0 < Value::ZERO
    }
}

impl Display for IntLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl FromStr for IntLiteral {
    type Err = ParseValueError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Self::parse(s)
    }
}

impl TryFrom<Token> for IntLiteral {
    type Error = Token;
    fn try_from(value: Token) -> Result<Self, Token> {
        if let Token::IntLiteral(value) = value {
            Ok(value)
        } else {
            Err(value)
        }
    }
}

impl Neg for IntLiteral {
    type Output = IntLiteral;

    fn neg(self) -> Self::Output {
        Self(-self.0)
    }
}

#[cfg(test)]
mod tests {
    use super::{IntLiteral, RE};

    /// Returns true if the string is matched by RE
    fn is_valid(s: &str) -> bool {
        RE.is_match(s)
    }

    /// Simple positive integers should match
    #[test]
    fn positive_integer_should_match() {
        assert!(is_valid("0"));
        assert!(is_valid("1"));
        assert!(is_valid("42"));
        assert!(is_valid("1000000"));
    }

    /// Negative integers should match
    #[test]
    fn negative_integer_should_match() {
        assert!(is_valid("-1"));
        assert!(is_valid("-42"));
        assert!(is_valid("-1000000"));
    }

    /// Large integers (beyond i64) should match
    #[test]
    fn large_integer_should_match() {
        assert!(is_valid("99999999999999999999999999999999"));
        assert!(is_valid("-99999999999999999999999999999999"));
    }

    /// The empty string should not match
    #[test]
    fn empty_string_should_not_match() {
        assert!(!is_valid(""));
    }

    /// A bare minus sign should not match
    #[test]
    fn bare_minus_should_not_match() {
        assert!(!is_valid("-"));
    }

    /// A plus sign prefix should match
    #[test]
    fn plus_prefix_should_match() {
        assert!(is_valid("+1"));
        assert!(is_valid("+0"));
        assert!(is_valid("+42"));
    }

    /// Floats should not match
    #[test]
    fn float_should_not_match() {
        assert!(!is_valid("1.0"));
        assert!(!is_valid("3.14"));
        assert!(!is_valid("-2.5"));
    }

    /// Non-numeric strings should not match
    #[test]
    fn non_numeric_should_not_match() {
        assert!(!is_valid("abc"));
        assert!(!is_valid("foo"));
    }

    /// Integers with surrounding whitespace or extra chars should not match
    #[test]
    fn integer_with_surrounding_chars_should_not_match() {
        assert!(!is_valid(" 42"));
        assert!(!is_valid("42 "));
        assert!(!is_valid("42abc"));
        assert!(!is_valid("1,000"));
    }

    /// Valid integer strings parse successfully
    #[test]
    fn valid_integer_should_parse() {
        assert!("0".parse::<IntLiteral>().is_ok());
        assert!("42".parse::<IntLiteral>().is_ok());
        assert!("-7".parse::<IntLiteral>().is_ok());
        assert!("+7".parse::<IntLiteral>().is_ok());
    }

    /// Large integers parse successfully
    #[test]
    fn large_integer_should_parse() {
        assert!(
            "99999999999999999999999999999999"
                .parse::<IntLiteral>()
                .is_ok()
        );
    }

    /// Invalid strings fail to parse
    #[test]
    fn invalid_string_should_fail_to_parse() {
        assert!("".parse::<IntLiteral>().is_err());
        assert!("abc".parse::<IntLiteral>().is_err());
        assert!("1.5".parse::<IntLiteral>().is_err());
    }
}
