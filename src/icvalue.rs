//! Type containing an intcode value

use std::{
    fmt::{self, Debug},
    ops::{
        Add, AddAssign, Deref, DerefMut, Div, DivAssign, Mul, MulAssign, Neg, Rem, RemAssign, Sub,
        SubAssign,
    },
};

/// The typer used to contain a intcode value
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
