#![doc = include_str!("../README.md")]

/// Size of a datatype
pub type Size = u64;

/// Pointer to a memory location
pub type Pointer = u64;

/// Difference between two pointers
pub type PointerOffset = i64;

/// The value stored in a single IntCode memory cell
pub type Value = num::BigInt;

/// Error returned when parsing a [`Value`] from a string
pub type ParseValueError = num::bigint::ParseBigIntError;
