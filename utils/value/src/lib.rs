use std::{
    ops::{Add, AddAssign, Mul, Neg},
    str::FromStr,
    sync::Arc,
};

use derive_more::Display;
use num::{ToPrimitive, bigint::ParseBigIntError};
use serde::{Deserialize, Serialize};
use snafu::Snafu;

#[derive(Debug, Clone, Display, Hash, Serialize, Deserialize)]
pub struct Value(Inner);

#[derive(Debug, Clone, Display, Serialize, Deserialize, Hash)]
#[serde(untagged)]
enum Inner {
    Small(i64),
    Big(Arc<num::BigInt>),
}

impl Value {
    pub const ZERO: Value = Value(Inner::Small(0));
}

#[derive(Debug, Snafu)]
#[snafu(transparent)]
pub struct ParseValueError {
    source: ParseBigIntError,
}

impl FromStr for Value {
    type Err = ParseValueError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if let Ok(n) = i64::from_str(s) {
            Ok(Value(Inner::Small(n)))
        } else {
            let big = num::BigInt::from_str(s)?;
            Ok(Value(Inner::Big(Arc::new(big))))
        }
    }
}

#[derive(Debug, Snafu, Clone)]
#[snafu(display("cannot cast value {value} to integer type"))]
pub struct CastValueToIntError {
    value: Value,
}

impl CastValueToIntError {
    pub fn into_original(self) -> Value {
        self.value
    }
}

macro_rules! impl_int_conversions {
    ($($ty:ty => $method:ident),* $(,)?) => {
        $(
            impl From<$ty> for Value {
                fn from(n: $ty) -> Self {
                    Value(Inner::Small(n as i64))
                }
            }

            impl TryFrom<Value> for $ty {
                type Error = CastValueToIntError;

                fn try_from(value: Value) -> Result<Self, Self::Error> {
                    match value.0 {
                        Inner::Small(n) => <$ty>::try_from(n).map_err(|_| CastValueToIntSnafu { value }.build()),
                        Inner::Big(ref n) => n.$method().ok_or_else(|| CastValueToIntSnafu { value }.build()),
                    }
                }
            }

            impl TryFrom<&Value> for $ty {
                type Error = CastValueToIntError;

                fn try_from(value: &Value) -> Result<Self, Self::Error> {
                    match &value.0 {
                        Inner::Small(n) => <$ty>::try_from(*n).map_err(|_| CastValueToIntSnafu { value: value.clone() }.build()),
                        Inner::Big(n) => n.$method().ok_or_else(|| CastValueToIntSnafu { value: value.clone() }.build()),
                    }
                }
            }
        )*
    };
}

// Types that always fit in i64
impl_int_conversions! {
    i8 => to_i8,
    i16 => to_i16,
    i32 => to_i32,
    i64 => to_i64,
    u8 => to_u8,
    u16 => to_u16,
    u32 => to_u32,
}

macro_rules! impl_try_from_value {
    ($($ty:ty => $method:ident),* $(,)?) => {
        $(
            impl TryFrom<Value> for $ty {
                type Error = CastValueToIntError;

                fn try_from(value: Value) -> Result<Self, Self::Error> {
                    match value.0 {
                        Inner::Small(n) => <$ty>::try_from(n).map_err(|_| CastValueToIntSnafu { value }.build()),
                        Inner::Big(ref n) => n.$method().ok_or_else(|| CastValueToIntSnafu { value }.build()),
                    }
                }
            }

            impl TryFrom<&Value> for $ty {
                type Error = CastValueToIntError;

                fn try_from(value: &Value) -> Result<Self, Self::Error> {
                    match &value.0 {
                        Inner::Small(n) => <$ty>::try_from(*n).map_err(|_| CastValueToIntSnafu { value: value.clone() }.build()),
                        Inner::Big(n) => n.$method().ok_or_else(|| CastValueToIntSnafu { value: value.clone() }.build()),
                    }
                }
            }
        )*
    };
}

impl_try_from_value! {
    isize => to_isize,
    u64 => to_u64,
    usize => to_usize,
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (&self.0, &other.0) {
            (Inner::Small(a), Inner::Small(b)) => a == b,
            (Inner::Big(a), Inner::Big(b)) => a == b,
            (Inner::Small(a), Inner::Big(b)) => &num::BigInt::from(*a) == b.as_ref(),
            (Inner::Big(a), Inner::Small(b)) => a.as_ref() == &num::BigInt::from(*b),
        }
    }
}

impl Eq for Value {}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Value {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match (&self.0, &other.0) {
            (Inner::Small(a), Inner::Small(b)) => a.cmp(b),
            (Inner::Big(a), Inner::Big(b)) => a.cmp(b),
            (Inner::Small(a), Inner::Big(b)) => num::BigInt::from(*a).cmp(b.as_ref()),
            (Inner::Big(a), Inner::Small(b)) => a.as_ref().cmp(&num::BigInt::from(*b)),
        }
    }
}

impl Neg for Value {
    type Output = Value;

    fn neg(self) -> Value {
        match self.0 {
            Inner::Small(i64::MIN) => Value(Inner::Big(Arc::new(-num::BigInt::from(i64::MIN)))),
            Inner::Small(n) => Value(Inner::Small(-n)),
            Inner::Big(n) => Value(Inner::Big(Arc::new(-n.as_ref()))),
        }
    }
}

impl Neg for &Value {
    type Output = Value;

    fn neg(self) -> Value {
        match &self.0 {
            Inner::Small(i64::MIN) => Value(Inner::Big(Arc::new(-num::BigInt::from(i64::MIN)))),
            Inner::Small(n) => Value(Inner::Small(-n)),
            Inner::Big(n) => Value(Inner::Big(Arc::new(-n.as_ref()))),
        }
    }
}

impl Add<&Value> for &Value {
    type Output = Value;

    fn add(self, other: &Value) -> Value {
        match (&self.0, &other.0) {
            (Inner::Small(a), Inner::Small(b)) => match a.checked_add(*b) {
                Some(sum) => Value(Inner::Small(sum)),
                None => {
                    let big = num::BigInt::from(*a) + num::BigInt::from(*b);
                    Value(Inner::Big(Arc::new(big)))
                }
            },
            (Inner::Small(a), Inner::Big(b)) => {
                Value(Inner::Big(Arc::new(num::BigInt::from(*a) + b.as_ref())))
            }
            (Inner::Big(a), Inner::Small(b)) => {
                Value(Inner::Big(Arc::new(a.as_ref() + num::BigInt::from(*b))))
            }
            (Inner::Big(a), Inner::Big(b)) => Value(Inner::Big(Arc::new(a.as_ref() + b.as_ref()))),
        }
    }
}

impl Mul<&Value> for &Value {
    type Output = Value;

    fn mul(self, other: &Value) -> Value {
        match (&self.0, &other.0) {
            (Inner::Small(a), Inner::Small(b)) => match a.checked_mul(*b) {
                Some(prod) => Value(Inner::Small(prod)),
                None => {
                    let big = num::BigInt::from(*a) * num::BigInt::from(*b);
                    Value(Inner::Big(Arc::new(big)))
                }
            },
            (Inner::Small(a), Inner::Big(b)) => {
                Value(Inner::Big(Arc::new(num::BigInt::from(*a) * b.as_ref())))
            }
            (Inner::Big(a), Inner::Small(b)) => {
                Value(Inner::Big(Arc::new(a.as_ref() * num::BigInt::from(*b))))
            }
            (Inner::Big(a), Inner::Big(b)) => Value(Inner::Big(Arc::new(a.as_ref() * b.as_ref()))),
        }
    }
}

impl AddAssign<&Value> for Value {
    fn add_assign(&mut self, other: &Value) {
        let result = &*self + other;
        *self = result;
    }
}
