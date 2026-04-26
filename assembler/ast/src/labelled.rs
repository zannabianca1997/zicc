//! Labelled items

use std::collections::BTreeSet;

use zicc_display::DisplayWith;

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

    /// Format and print the labels
    pub(crate) fn fmt_labels_with(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        interner: &string_interner::DefaultStringInterner,
    ) -> std::fmt::Result {
        for label in &self.labels {
            label.fmt_with(f, interner)?;
            write!(f, ": ")?;
        }
        Ok(())
    }

    /// Are there any label?
    pub fn is_labelled(&self) -> bool {
        !self.labels.is_empty()
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

impl<T: DisplayWith> DisplayWith for Labelled<T> {
    fn fmt_with(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        interner: &string_interner::DefaultStringInterner,
    ) -> std::fmt::Result {
        self.fmt_labels_with(f, interner)?;
        self.item.fmt_with(f, interner)
    }
}
