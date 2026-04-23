//! Labelled items

use std::collections::BTreeSet;

use crate::identifier::Identifier;

/// Labelled item
///
/// AST item preceded by any set of labels followed by a colon
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Labelled<T> {
    pub labels: BTreeSet<Identifier>,
    pub item: T,
}

impl<T> Labelled<T> {
    pub fn unlabelled(item: T) -> Self {
        Self {
            labels: BTreeSet::new(),
            item,
        }
    }

    /// Map the labelled item
    pub fn map<U>(self, fun: impl FnOnce(T) -> U) -> Labelled<U> {
        let Self { labels, item } = self;
        Labelled {
            labels,
            item: fun(item),
        }
    }

    /// Merge more labels in this one
    pub fn merge(&mut self, other: Labelled<()>) {
        self.labels.extend(other.labels);
    }
}

impl<T> Labelled<Option<T>> {
    /// Try unwrap the labelled option
    pub fn try_unwrap(self) -> Result<Labelled<T>, Labelled<()>> {
        match self {
            Self {
                labels,
                item: Some(item),
            } => Ok(Labelled { labels, item }),
            Self { labels, item: None } => Err(Labelled { labels, item: () }),
        }
    }
}
