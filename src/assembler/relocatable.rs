//! Relocatable code

use std::{
    collections::{HashMap, HashSet},
    fmt::Debug,
};

use thiserror::Error;

use crate::ICValue;

use super::label::{Label, Labelled};

/// An intcode program fragment that can be posizioned, and contains references to other fragments

/*
Invariants:
 - `usize`s are indices on the slice inside `content`. They must never exceed her lenght.
 - `references` are external references. No value of that map is a key to `labels`
 - `relatives` elements and `references` keys are disjointed
*/
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct ICProgramFragment {
    /// Content of the fragment
    content: Vec<ICValue>,
    /// Labels defined in this fragment
    labels: HashMap<Label, usize>,
    /// Labels referenced in this fragment
    /// The value in content is intended as an offset
    /// The value at pos `p` is then `labels[references[p]] + content[p]` (this is contracted as soon as possible)
    references: HashMap<usize, Label>,
    /// Content relative to the start of the fragment
    /// The value at pos `p` where `p` is inside `relatives` is then `(start_of_segment) + content[p]`
    relatives: HashSet<usize>,
}

#[derive(Debug, Error)]
pub enum AppendError {
    #[error("Duplicate label definition {0}")]
    DuplicateLabelDefError(Label),
}

impl ICProgramFragment {
    /// Concatenate two program fragments
    pub fn join(self, other: Self) -> Result<Self, AppendError> {
        // early return if collision happens
        if let Some(lbl) = other
            .labels
            .keys()
            .filter(|lbl| self.labels.contains_key(lbl))
            .next()
        {
            return Err(AppendError::DuplicateLabelDefError(lbl.clone()));
        }

        let join_point = self.content.len();

        // Merging contents
        let mut content = {
            let mut content = self.content;
            let mut other_content = other.content;
            content.append(&mut other_content);
            content
        };

        // Merging labels
        let labels = {
            let mut labels = self.labels;
            labels.extend(
                other
                    .labels
                    .into_iter()
                    .map(|(lbl, pos)| (lbl, pos + join_point)), // keep track of the definition position
            );
            labels
        };

        // Merging references, and solving internal references
        let (new_relatives, references) = {
            let references = self.references.into_iter().chain(
                other
                    .references
                    .into_iter()
                    .map(|(pos, lbl)| (pos + join_point, lbl)), // keep track of the relative position
            );
            let mut new_relatives = HashSet::new();
            let mut extern_references = HashMap::new();
            for (pos, lbl) in references {
                if let Some(labelled) = labels.get(&lbl) {
                    // internal reference
                    // it points to labels[lbl] + content[pos]
                    content[pos] = ICValue(content[pos].0 + (*labelled as i64));
                    new_relatives.insert(pos);
                } else {
                    // external reference
                    extern_references.insert(pos, lbl);
                }
            }
            (new_relatives, extern_references)
        };

        // Merging relatives
        let relatives = {
            let mut relatives = self.relatives;
            relatives.extend(other.relatives.into_iter().map(|pos| pos + join_point));
            relatives.extend(new_relatives);
            relatives
        };

        // Setting up new values
        let res = Self {
            content,
            labels,
            references,
            relatives,
        };

        res.debug_assert_invariants();

        Ok(res)
    }

    /// Check that all invariants are upholded.
    ///
    /// This is a no-op in release.
    fn debug_assert_invariants(&self) {
        #[cfg(debug_assertions)]
        {
            // Check that all indices are inside the content
            for (lbl, pos) in self.labels.iter() {
                assert!(
                    *pos < self.content.len(),
                    "Label {lbl} defined outside content"
                )
            }
            for (pos, lbl) in self.references.iter() {
                assert!(
                    *pos < self.content.len(),
                    "Reference to label {lbl} defined outside content"
                )
            }
            for pos in self.relatives.iter() {
                assert!(
                    *pos < self.content.len(),
                    "Relative defined outside content"
                )
            }
            // Check that all references are external
            for (_, lbl) in self.references.iter() {
                assert!(
                    !self.labels.contains_key(lbl),
                    "{lbl} is kept as an internal reference"
                )
            }
            // Check that no value is both a reference and a relative
            for (pos, _) in self.references.iter() {
                assert!(
                    !self.relatives.contains(pos),
                    "{pos} is both a relative value and a reference"
                )
            }
        }
    }

    /// Empty program fragment
    pub fn empty() -> ICProgramFragment {
        Default::default()
    }

    /// Add a memory location to the fragment
    pub fn push(&mut self, value: Labelled<RlValue>) -> Result<(), AppendError> {
        let Labelled { inner: value, lbls } = value;
        // Check for collisions
        if let Some(lbl) = lbls
            .iter()
            .filter(|lbl| self.labels.contains_key(lbl))
            .next()
        {
            return Err(AppendError::DuplicateLabelDefError(lbl.clone()));
        }
        // Add labels
        for lbl in lbls {
            self.labels.insert(lbl, self.content.len());
        }
        // Add to the end
        self.content.push(match value {
            RlValue::Absolute(v) => v,
            RlValue::Reference { lbl, offset } => match self.labels.get(&lbl) {
                Some(v) => {
                    // the label is already resolved
                    self.relatives.insert(self.content.len());
                    ICValue(*v as _) + offset
                }
                None => {
                    self.references.insert(self.content.len(), lbl);
                    offset
                }
            },
        });
        self.debug_assert_invariants();
        Ok(())
    }

    /// Shrink the memory usage as much as possible
    pub fn shrink_to_fit(&mut self) {
        self.content.shrink_to_fit();
        self.labels.shrink_to_fit();
        self.references.shrink_to_fit();
        self.relatives.shrink_to_fit();
    }

    /// Iter through the label defined in this fragment
    pub fn defined_labels(&self) -> impl Iterator<Item = &Label> {
        self.labels.keys()
    }
    /// Iter through the label referenced in this fragment
    ///
    /// They are in a unspecified order, and repetition are allowed
    pub fn referenced_labels(&self) -> impl Iterator<Item = &Label> {
        self.references.values()
    }

    /// Remove a label definition, making it local to this fragment
    pub fn remove_label(&mut self, lbl: &Label) -> Option<Label> {
        self.labels.remove_entry(lbl).map(|(lbl, _)| lbl)
    }
    /// Remove all labels matching a predicate
    pub fn remove_labels<P>(&mut self, mut predicate: P)
    where
        P: FnMut(&Label) -> bool,
    {
        self.labels.retain(|lbl, _| !predicate(lbl))
    }
}

impl FromIterator<ICProgramFragment> for Result<ICProgramFragment, AppendError> {
    fn from_iter<T: IntoIterator<Item = ICProgramFragment>>(iter: T) -> Self {
        iter.into_iter()
            .try_reduce(ICProgramFragment::join)
            .map(Option::unwrap_or_default)
    }
}

/// A single, relocatable memory location
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum RlValue {
    /// An exact value
    Absolute(ICValue),
    /// A label index, plus an offset
    Reference { lbl: Label, offset: ICValue },
}
impl From<ICValue> for RlValue {
    fn from(value: ICValue) -> Self {
        Self::Absolute(value)
    }
}
impl From<Label> for RlValue {
    fn from(value: Label) -> Self {
        Self::Reference {
            lbl: value,
            offset: ICValue(0),
        }
    }
}

/*
/// A reference to a single, relocatable memory location
#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum RlValueRef<'a> {
    /// An exact value
    Absolute(&'a ICValue),
    /// A label index, plus an offset
    Reference { lbl: &'a Label, offset: &'a ICValue },
    /// An index in the fragment
    Relative(&'a ICValue),
}
impl<'a> From<RlValueMut<'a>> for RlValueRef<'a> {
    fn from(value: RlValueMut<'a>) -> Self {
        match value {
            RlValueMut::Absolute(v) => Self::Absolute(v),
            RlValueMut::Reference { lbl, offset } => Self::Reference { lbl, offset },
            RlValueMut::Relative(v) => Self::Relative(v),
        }
    }
}
impl RlValueRef<'_> {
    pub fn cloned(self) -> RlValue {
        match self {
            Self::Absolute(v) => RlValue::Absolute(*v),
            Self::Reference { lbl, offset } => RlValue::Reference {
                lbl: lbl.clone(),
                offset: *offset,
            },
            Self::Relative(v) => RlValue::Relative(*v),
        }
    }
}
/// A mutable reference to a single, relocatable memory location
#[derive(Debug, Eq, PartialEq)]
pub enum RlValueMut<'a> {
    /// An exact value
    Absolute(&'a mut ICValue),
    /// A label index, plus an offset
    Reference {
        lbl: &'a mut Label,
        offset: &'a mut ICValue,
    },
    /// An index in the fragment
    Relative(&'a mut ICValue),
}
impl RlValueMut<'_> {
    pub fn cloned(self) -> RlValue {
        match self {
            Self::Absolute(v) => RlValue::Absolute(*v),
            Self::Reference { lbl, offset } => RlValue::Reference {
                lbl: lbl.clone(),
                offset: *offset,
            },
            Self::Relative(v) => RlValue::Relative(*v),
        }
    }
}

*/
