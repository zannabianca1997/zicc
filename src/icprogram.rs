//!Types used to contain and manipulate intcode programs

use std::ops::{Deref, DerefMut, Index, IndexMut};
use std::slice::SliceIndex;

use crate::ICValue;
/// An intcode program
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ICProgram(Box<[ICValue]>);

impl ICProgram {
    /// Returns an iterator over the program.
    ///
    /// The iterator yields all items from start to end.
    #[inline]
    pub fn iter(&self) -> impl Iterator<Item = &ICValue> {
        self.0.iter()
    }
    /// Returns an iterator that allows modifying each value.
    ///
    /// The iterator yields all items from start to end.
    #[inline]
    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut ICValue> {
        self.0.iter_mut()
    }
    /// Returns a reference to an element or subslice depending on the type of
    /// index.
    ///
    /// - If given a position, returns a reference to the element at that
    ///   position or `None` if out of bounds.
    /// - If given a range, returns the subslice corresponding to that range,
    ///   or `None` if out of bounds.
    #[must_use]
    #[inline]
    pub const fn get<I>(&self, index: I) -> Option<&I::Output>
    where
        I: ~const SliceIndex<[ICValue]>,
    {
        self.0.get(index)
    }
    /// Returns a mutable reference to an element or subslice depending on the
    /// type of index (see [`get`]) or `None` if the index is out of bounds.
    ///
    /// [`get`]: slice::get
    #[must_use]
    #[inline]
    pub const fn get_mut<I>(&mut self, index: I) -> Option<&mut I::Output>
    where
        I: ~const SliceIndex<[ICValue]>,
    {
        self.0.get_mut(index)
    }
}

impl Default for ICProgram {
    fn default() -> Self {
        Self(Box::new([]))
    }
}
impl IntoIterator for ICProgram {
    type Item = ICValue;

    type IntoIter = <Vec<ICValue> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_vec().into_iter()
    }
}
impl FromIterator<ICValue> for ICProgram {
    fn from_iter<T: IntoIterator<Item = ICValue>>(iter: T) -> Self {
        Self(iter.into_iter().collect())
    }
}
impl Deref for ICProgram {
    type Target = [ICValue];
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl DerefMut for ICProgram {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}
impl AsRef<[ICValue]> for ICProgram {
    fn as_ref(&self) -> &[ICValue] {
        &self.0
    }
}
impl AsMut<[ICValue]> for ICProgram {
    fn as_mut(&mut self) -> &mut [ICValue] {
        &mut self.0
    }
}
impl Index<usize> for ICProgram {
    type Output = ICValue;

    fn index(&self, index: usize) -> &Self::Output {
        &self.0[index]
    }
}
impl IndexMut<usize> for ICProgram {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.0[index]
    }
}
