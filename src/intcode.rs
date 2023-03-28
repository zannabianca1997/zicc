//!Types used to contain and manipulate intcode programs

use std::{
    fmt::{self, Debug},
    ops::{
        Add, AddAssign, Deref, DerefMut, Div, DivAssign, Index, IndexMut, Mul, MulAssign, Neg, Rem,
        RemAssign, Sub, SubAssign,
    },
    slice::SliceIndex,
};

/// The type used to contain a intcode value
type BaseT = i64;

/// A single value in a intcode program
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ICValue(BaseT);

impl Debug for ICValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}
impl Default for ICValue {
    fn default() -> Self {
        Self(BaseT::default())
    }
}
impl From<BaseT> for ICValue {
    fn from(other: BaseT) -> Self {
        Self(other)
    }
}
impl From<ICValue> for BaseT {
    fn from(value: ICValue) -> Self {
        value.0
    }
}
impl Deref for ICValue {
    type Target = BaseT;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl DerefMut for ICValue {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}
impl AsRef<BaseT> for ICValue {
    fn as_ref(&self) -> &BaseT {
        &self.0
    }
}
impl AsMut<BaseT> for ICValue {
    fn as_mut(&mut self) -> &mut BaseT {
        &mut self.0
    }
}
impl Add for ICValue {
    type Output = Self;
    fn add(self, other: Self) -> Self {
        Self(self.0 + other.0)
    }
}
impl AddAssign for ICValue {
    fn add_assign(&mut self, other: Self) {
        self.0 += other.0;
    }
}
impl Div for ICValue {
    type Output = Self;
    fn div(self, rhs: Self) -> Self::Output {
        Self(self.0 / rhs.0)
    }
}
impl DivAssign for ICValue {
    fn div_assign(&mut self, rhs: Self) {
        self.0 /= rhs.0
    }
}
impl Mul for ICValue {
    type Output = Self;
    fn mul(self, rhs: Self) -> Self {
        Self(self.0 * rhs.0)
    }
}
impl MulAssign for ICValue {
    fn mul_assign(&mut self, rhs: Self) {
        self.0 *= rhs.0
    }
}
impl Rem for ICValue {
    type Output = Self;
    fn rem(self, modulus: Self) -> Self::Output {
        Self(self.0 % modulus.0)
    }
}
impl RemAssign for ICValue {
    fn rem_assign(&mut self, modulus: Self) {
        self.0 %= modulus.0;
    }
}
impl Sub for ICValue {
    type Output = Self;
    fn sub(self, other: Self) -> Self {
        Self(self.0 - other.0)
    }
}
impl SubAssign for ICValue {
    fn sub_assign(&mut self, other: Self) {
        self.0 -= other.0
    }
}
impl Neg for ICValue {
    type Output = Self;
    fn neg(self) -> Self::Output {
        Self(-self.0)
    }
}

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
