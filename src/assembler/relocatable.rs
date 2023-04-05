//! Relocatable code

use std::{
    collections::{
        hash_map::{Entry, OccupiedEntry},
        HashMap, HashSet,
    },
    fmt::{Debug, Display},
};

use serde::{Deserialize, Serialize};
use thiserror::Error;

use super::label::{Label, Labelled};
use crate::intcode::{ICProgram, ICValue};

/// An intcode program fragment that can be posizioned, and contains references to other fragments

/*
Invariants:
 - `usize`s are indices on the slice inside `content`. They must never exceed her lenght.
 - `references` are external references. No value of that map is a key to `labels`
 - `relatives` elements and `references` keys are disjointed
*/
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
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

#[derive(Debug, Error, PartialEq, Eq)]
pub enum AppendError {
    #[error("Duplicate label definition {0}")]
    DuplicateLabelDefError(Label),
}

impl ICProgramFragment {
    /// Concatenate two program fragments
    pub fn join(self, other: Self) -> Result<Self, AppendError> {
        let join_point = self.content.len();

        // early return if collision happens
        if let Some((lbl, _)) = other
            .labels
            .iter()
            .filter(|(lbl, pos1)| {
                // no two labels point to the same position
                self.labels
                    .get(lbl)
                    .is_some_and(|pos2| join_point + **pos1 != *pos2)
            })
            .next()
        {
            return Err(AppendError::DuplicateLabelDefError(lbl.clone()));
        }

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
            for relative in other.relatives {
                // find new position
                let relative = relative + join_point;
                // adjust offset
                content[relative] += ICValue(join_point as _);
                // add relative to the list
                relatives.insert(relative);
            }
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
                    *pos <= self.content.len(),
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
    pub fn empty() -> Self {
        Default::default()
    }

    /// Program fragment made only of zeros
    pub fn zeros(len: usize) -> Self {
        Self {
            content: vec![ICValue(0); len],
            labels: HashMap::new(),
            references: HashMap::new(),
            relatives: HashSet::new(),
        }
    }

    /// Add a memory location to the fragment
    pub fn push(&mut self, value: Labelled<RlValue>) -> Result<(), AppendError> {
        let (value, lbls) = value.split();
        // Check for collisions
        if let Some(lbl) = lbls
            .lbls
            .iter()
            .filter(|lbl| {
                // either the label is undefined, or point to the end of the content
                self.labels
                    .get(lbl)
                    .is_some_and(|pos| *pos != self.content.len())
            })
            .next()
        {
            return Err(AppendError::DuplicateLabelDefError(lbl.clone()));
        }
        // Add labels
        self.push_labels(lbls)?;
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

    /// Add labels at the end of the fragment
    pub fn push_labels(&mut self, lbls: Labelled<()>) -> Result<(), AppendError> {
        let Labelled { lbls, .. } = lbls;
        // Check for collisions
        if let Some(lbl) = lbls
            .iter()
            .filter(|lbl| {
                // either the label is undefined, or point to the end of the content
                self.labels
                    .get(lbl)
                    .is_some_and(|pos| *pos != self.content.len())
            })
            .next()
        {
            return Err(AppendError::DuplicateLabelDefError(lbl.clone()));
        }
        // Add labels
        let lbls_pos = self.content.len();
        for lbl in lbls {
            // We need to collect all the reference, to then modify the hashmap
            let references: Vec<_> = self
                .references
                .iter()
                .filter(|(_, lbl2)| &lbl == *lbl2)
                .map(|(pos, _)| *pos)
                .collect();
            for reference in references {
                self.references.remove(&reference);
                self.content[reference] += ICValue(lbls_pos as _);
                self.relatives.insert(reference);
            }
            self.labels.insert(lbl, lbls_pos);
        }
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

    /// Check if the program is complete (no more free reference)
    pub fn is_free(&self) -> bool {
        self.referenced_labels().next().is_none()
    }

    /// Emit a complete program as a program
    pub fn emit(self) -> Option<ICProgram> {
        if self.is_free() {
            // `relatives` are ok, cause now the start is finalized
            Some(self.content.into())
        } else {
            None
        }
    }

    /// Check if two program fragment are equivalent
    ///
    /// Equivalent mean that they have the same code and references, all the non-anonimous labels are the same and on the same locations, and all the anonimous labels can be mapped on one another
    pub fn equivalent(&self, other: &Self) -> bool {
        // Content and relatives are indipendent from the labels
        if !(self.content == other.content && self.relatives == other.relatives) {
            return false;
        }

        let (mut self_labels_anon, self_labels_named): (HashMap<_, _>, HashMap<_, _>) = self
            .labels
            .iter()
            .map(|(lbl, pos)| (lbl, *pos))
            .partition(|(lbl, _)| lbl.is_anonimous());
        let (mut other_labels_anon, other_labels_named): (HashMap<_, _>, HashMap<_, _>) = other
            .labels
            .iter()
            .map(|(lbl, pos)| (lbl, *pos))
            .partition(|(lbl, _)| lbl.is_anonimous());

        // check the named part is equal
        if self_labels_named != other_labels_named {
            return false;
        }

        let (self_refs_anon, self_refs_named): (HashMap<_, _>, HashMap<_, _>) = self
            .references
            .iter()
            .map(|(pos, lbl)| (*pos, lbl))
            .partition(|(_, lbl)| lbl.is_anonimous());
        let (mut other_refs_anon, other_refs_named): (HashMap<_, _>, HashMap<_, _>) = other
            .references
            .iter()
            .map(|(pos, lbl)| (*pos, lbl))
            .partition(|(_, lbl)| lbl.is_anonimous());

        // check the named part is equal
        if self_refs_named != other_refs_named {
            return false;
        }

        // Let's build a mapping between the anonimous labels
        // Drain all the reference. there is a single label for reference, so match is forced.
        let mut anon_mapping: HashMap<&Label, &Label> = HashMap::new(); // map self -> other
        for (pos, lbl1) in self_refs_anon {
            if let Some(lbl2) = other_refs_anon.remove(&pos) {
                // they need to match
                match anon_mapping.entry(lbl1) {
                    Entry::Occupied(oe) => {
                        if *oe.get() != lbl2 {
                            return false; // label was already matched with a different one
                        }
                    }
                    Entry::Vacant(ve) => {
                        ve.insert(lbl2); // force the match
                    }
                }
            } else {
                return false; // reference was not matches
            }
        }
        if !other_refs_anon.is_empty() {
            return false; // Not all references were matched
        }

        // I can throw away the mapping now, given that no defined label can be in the references
        // the only thing that remains now are labels that are defined, but not referenced.
        // They only need to match in number defined on the same cell, and mapping can then be arbitrary
        let mut anon_number: HashMap<usize, usize> = HashMap::new();
        for pos in self_labels_anon.into_values() {
            *anon_number.entry(pos).or_insert(0) += 1;
        }
        for pos in other_labels_anon.into_values() {
            match anon_number.entry(pos) {
                Entry::Occupied(oe) if *oe.get() == 1 => {
                    oe.remove(); // no labels left, pop this entry
                }
                Entry::Occupied(mut oe) => {
                    *oe.get_mut() -= 1; // count a label matched
                }
                Entry::Vacant(_) => return false, // no labels were left to match
            }
        }
        if !anon_number.is_empty() {
            return false; // some labels were unmatched...
        }

        true // all test passed. Good!
    }
}

impl FromIterator<ICProgramFragment> for Result<ICProgramFragment, AppendError> {
    fn from_iter<T: IntoIterator<Item = ICProgramFragment>>(iter: T) -> Self {
        iter.into_iter()
            .try_reduce(ICProgramFragment::join)
            .map(Option::unwrap_or_default)
    }
}
impl From<Labelled<RlValue>> for ICProgramFragment {
    fn from(value: Labelled<RlValue>) -> Self {
        match value {
            Labelled {
                inner: RlValue::Absolute(v),
                lbls,
            } => Self {
                content: vec![v],
                labels: lbls.into_iter().map(|lbl| (lbl, 0)).collect(),
                references: HashMap::new(),
                relatives: HashSet::new(),
            },
            Labelled {
                inner: RlValue::Reference { lbl, offset },
                lbls,
            } => {
                if !lbls.contains(&lbl) {
                    Self {
                        content: vec![offset],
                        labels: lbls.into_iter().map(|lbl| (lbl, 0)).collect(),
                        references: HashMap::from([(0, lbl)]),
                        relatives: HashSet::new(),
                    }
                } else {
                    Self {
                        content: vec![offset],
                        labels: lbls.into_iter().map(|lbl| (lbl, 0)).collect(),
                        references: HashMap::new(),
                        relatives: HashSet::from([0]),
                    }
                }
            }
        }
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

impl RlValue {
    pub fn try_into_absolute(self) -> Result<ICValue, Self> {
        if let Self::Absolute(v) = self {
            Ok(v)
        } else {
            Err(self)
        }
    }

    pub(crate) fn offset(self, offset: ICValue) -> Self {
        match self {
            RlValue::Absolute(v) => RlValue::Absolute(v + offset),
            RlValue::Reference {
                lbl,
                offset: offset_before,
            } => RlValue::Reference {
                lbl,
                offset: offset_before + offset,
            },
        }
    }
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
impl Default for RlValue {
    fn default() -> Self {
        Self::Absolute(ICValue(0))
    }
}
impl Display for RlValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RlValue::Absolute(v) => write!(f, "{v}"),
            RlValue::Reference { lbl, offset } => {
                if offset.0 != 0 {
                    write!(f, "{lbl}{offset:+}")
                } else {
                    write!(f, "{lbl}")
                }
            }
        }
    }
}
