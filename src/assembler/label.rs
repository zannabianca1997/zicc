//! Labels

use std::{
    collections::HashSet,
    fmt::Display,
    hash::Hash,
    iter::once,
    num::ParseIntError,
    ops::{Deref, DerefMut},
    str::FromStr,
};

use smartstring::{Compact, SmartString};
use thiserror::Error;

use crate::identifier::{Identifier, InvalidIdentifierError};

#[derive(Debug, Hash, PartialEq, Eq, Clone, PartialOrd, Ord)]
pub enum Label {
    /// Symbol visible outside the compilation unit
    Global(Identifier),
    /// Symbol visible only inside the compilation unit
    Local(Identifier),
    /// Unnamed label
    Unnamed(usize),
}

impl Display for Label {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Label::Global(name) => write!(f, "{name}"),
            Label::Local(name) => write!(f, ".{name}"),
            Label::Unnamed(id) => write!(f, "{id}"),
        }
    }
}

#[derive(Debug, Error, PartialEq, Eq)]
pub enum InvalidLabelError {
    #[error(transparent)]
    InvalidIdentifier(#[from] InvalidIdentifierError),
    #[error("Invalid numeral for unnamed label")]
    InvalidId(
        #[from]
        #[source]
        ParseIntError,
    ),
    #[error("Invalid empty string")]
    EmptyString,
}

impl FromStr for Label {
    type Err = InvalidLabelError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(
            match s.chars().next().ok_or(InvalidLabelError::EmptyString)? {
                ch if ch.is_ascii_digit() => Self::Unnamed(s.parse()?),
                '.' => Self::Local(s[1..].parse()?),
                _ => Self::Global(s.parse()?),
            },
        )
    }
}

/// An object with a set of labels attached
#[derive(Debug, Clone, Eq, PartialEq, Default)]
pub struct Labelled<T> {
    pub inner: T,
    pub lbls: HashSet<Label>,
}
impl<T> Labelled<T> {
    pub fn new_unlabelled(inner: T) -> Self {
        Self {
            inner,
            lbls: Default::default(),
        }
    }
    pub fn new(inner: T, lbl: Label) -> Self {
        Self {
            inner,
            lbls: once(lbl).collect(),
        }
    }
    pub fn new_multiple_labels(inner: T, lbls: impl IntoIterator<Item = Label>) -> Self {
        Self {
            inner,
            lbls: lbls.into_iter().collect(),
        }
    }

    pub fn label(&mut self, value: Label) -> bool {
        self.lbls.insert(value)
    }
}
impl<T> Labelled<Labelled<T>> {
    /// Flatten double labelled elements
    pub fn flatten(self) -> Labelled<T> {
        let Labelled {
            inner: Labelled {
                inner,
                lbls: inner_lbls,
            },
            mut lbls,
        } = self;
        lbls.extend(inner_lbls);
        Labelled { inner, lbls }
    }
}

impl<T: PartialOrd> PartialOrd for Labelled<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.inner.partial_cmp(&other.inner)
    }
}
impl<T: Ord> Ord for Labelled<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.inner.cmp(&other.inner)
    }
}
impl<T: Display> Display for Labelled<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for lbl in self.lbls.iter() {
            write!(f, "{lbl}: ")?
        }
        self.inner.fmt(f)
    }
}
impl<T> From<T> for Labelled<T> {
    fn from(other: T) -> Self {
        Self {
            inner: other,
            lbls: Default::default(),
        }
    }
}
impl<T> Deref for Labelled<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}
impl<T> DerefMut for Labelled<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}
impl<T> AsRef<T> for Labelled<T> {
    fn as_ref(&self) -> &T {
        &self.inner
    }
}
impl<T> AsMut<T> for Labelled<T> {
    fn as_mut(&mut self) -> &mut T {
        &mut self.inner
    }
}

trait Labellable: Sized {
    fn labelled(self, lbl: Label) -> Labelled<Self> {
        Labelled::new(self, lbl)
    }
    fn labelled_multiple(self, lbls: impl IntoIterator<Item = Label>) -> Labelled<Self> {
        Labelled::new_multiple_labels(self, lbls)
    }
}
impl<T> Labellable for T {}

#[cfg(test)]
mod tests {
    use super::Label;

    #[test]
    fn local_from_str() {
        assert_eq!(".ab3".parse(), Ok(Label::Local("ab3".parse().unwrap())))
    }
    #[test]
    fn global_from_str() {
        assert_eq!("ab3".parse(), Ok(Label::Global("ab3".parse().unwrap())))
    }
    #[test]
    fn unnamed_from_str() {
        assert_eq!("3".parse(), Ok(Label::Unnamed(3)))
    }
}
