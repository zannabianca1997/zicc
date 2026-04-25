#![doc = include_str!("../README.md")]

/// Size of a datatype
pub type Size = u64;

/// Pointer to a memory location
pub type Pointer = u64;

/// Difference between two pointers
pub type PointerOffset = i64;

pub use zicc_value::{CastValueToIntError, ParseValueError, Value};
