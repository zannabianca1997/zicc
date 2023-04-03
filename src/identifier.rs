//! Identifiers storage and checking

use std::{
    borrow::{Borrow, Cow},
    fmt::Display,
    ops::Deref,
    str::FromStr,
};

use lazy_regex::regex_is_match;
use serde::{Deserialize, Serialize};
use smartstring::{Compact, SmartString};
use thiserror::Error;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct Identifier(SmartString<Compact>);

impl Identifier {
    pub fn new(s: Cow<str>) -> Result<Self, InvalidIdentifierError> {
        if regex_is_match!("^[_a-zA-Z][_a-zA-Z0-9]*$", &s) {
            Ok(Self(SmartString::from(s)))
        } else {
            Err(InvalidIdentifierError)
        }
    }
}

impl Deref for Identifier {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl AsRef<str> for Identifier {
    fn as_ref(&self) -> &str {
        self.deref()
    }
}
impl Borrow<str> for Identifier {
    fn borrow(&self) -> &str {
        self.deref()
    }
}
impl Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

#[derive(Debug, Error, PartialEq, Eq)]
#[error("Not a valid identifier")]
pub struct InvalidIdentifierError;

impl FromStr for Identifier {
    type Err = InvalidIdentifierError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Self::new(Cow::Borrowed(s))
    }
}

#[cfg(test)]
mod tests {
    use super::Identifier;
    use super::InvalidIdentifierError;

    #[test]
    fn from_str() {
        assert_eq!("ab_3".parse(), Ok(Identifier("ab_3".into())))
    }
    #[test]
    fn from_invalid_str() {
        assert_eq!("3ab".parse::<Identifier>(), Err(InvalidIdentifierError))
    }
}
