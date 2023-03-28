//! A intcode program

use std::ops::{Deref, DerefMut, Index, IndexMut};

use crate::ICValue;
/// A incode program
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ICProgram(Box<[ICValue]>);

impl ICProgram {
    pub fn iter(&self) -> impl Iterator<Item = &ICValue> {
        self.0.iter()
    }
    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut ICValue> {
        self.0.iter_mut()
    }
    pub fn get(&self, index: usize) -> Option<&ICValue> {
        self.0.get(index)
    }
    pub fn get_mut(&mut self, index: usize) -> Option<&mut ICValue> {
        self.0.get_mut(index)
    }
}

impl Default for ICProgram {
    fn default() -> Self {
        Self(Default::default())
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
