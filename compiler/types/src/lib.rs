#![doc = include_str!("../README.md")]

use std::num::NonZeroUsize;

use derive_more::{From, Into};
pub use table::TypeTable;
use zicc_limits::Size;

pub mod table;

/// An id of a type
///
/// Types are equal if and only if their type id is equal.
///
/// A type id can be obtaining by consulting a [`TypeTable`]. It is essential
/// that type ids are not mixed between tables: they are specific to the single
/// table.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeId(NonZeroUsize);

/// An id of a sized type
///
/// Like [`TypeId`], but the type is guaranteed to be sized.
///
/// Conversion to a generic [`TypeId`] is made with [`Into`], while for the
/// opposite conversion the [`TypeTable`] must be consulted.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Into)]
pub struct SizedTypeId(TypeId);

/// A type in `zicc`
///
/// This represent both sized and unsized type. It is not a recursive data
/// structure as [`TypeId`]s are used inside composite types
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, From)]
pub enum Type {
    /// Sized types
    Sized(SizedType),

    /// Unknown type
    Unknown(Unknown),
}

/// Unknown type `_`
///
/// Type of unknown size or content.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Unknown;

/// A sized type in `zicc`
///
/// See [`Type`]. Guarantee to have a definite size, although a [`TypeTable`]
/// must be consulted to know it.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, From)]
pub enum SizedType {
    /// Int type
    Int(Int),

    /// Pointer type
    Pointer(Pointer),

    /// Array type
    Array(Array),
}

/// Int type `int`
///
/// A single intcode cell, interpreted as a integer.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Int;

/// Pointer type `&...`
///
/// A single cell pointing to a memory location
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Pointer {
    /// Kind of the pointer
    kind: PointerKind,
    /// Pointed type
    pointed: TypeId,
}

/// Pointer kind
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum PointerKind {
    /// Absolute pointers
    ///
    /// Point to an absolute pointer in memory
    Absolute,
    /// Relative pointers
    ///
    /// Point to a position relative to the stack top
    Relative,
}

/// Array type `[...; N]`
///
/// A contiguous slice of known length
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Array {
    /// Element of the array
    element: SizedTypeId,
    /// Length of the array
    length: Size,
}
