#![doc = include_str!("../README.md")]

use std::num::NonZeroUsize;

use derive_more::{Constructor, From, Into, TryUnwrap};
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
/// opposite conversion [`TypeTable::try_unwrap_sized_id`] must be used.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Into)]
pub struct SizedTypeId(TypeId);

/// A type in `zicc`
///
/// This represent both sized and unsized type. It is not a recursive data
/// structure as [`TypeId`]s are used inside composite types
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, From, TryUnwrap)]
#[try_unwrap(ref)]
pub enum Type {
    /// Sized types
    Sized(SizedType),

    /// Unknown type
    Unknown(Unknown),
}
impl Type {
    /// Unknown type
    pub fn unknown() -> Self {
        Unknown::new().into()
    }

    /// Int type
    pub fn int() -> Self {
        SizedType::int().into()
    }

    /// Pointer type
    pub fn pointer(kind: PointerKind, pointed: TypeId) -> Self {
        SizedType::pointer(kind, pointed).into()
    }

    /// Array type
    pub fn array(element: SizedTypeId, length: Size) -> Self {
        SizedType::array(element, length).into()
    }
}

impl Default for Type {
    fn default() -> Self {
        Self::unknown()
    }
}

/// Unknown type `_`
///
/// Type of unknown size or content.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Constructor, Default)]
pub struct Unknown;

impl Unknown {
    /// As a scalar type, [`Unknown`] has a known type id
    pub const fn type_id() -> TypeId {
        TypeId(NonZeroUsize::new(2).unwrap())
    }
}

/// A sized type in `zicc`
///
/// See [`Type`]. Guarantee to have a definite size, although a [`TypeTable`]
/// must be consulted to know it.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, From, TryUnwrap)]
#[try_unwrap(ref)]
pub enum SizedType {
    /// Int type
    Int(Int),

    /// Pointer type
    Pointer(Pointer),

    /// Array type
    Array(Array),
}
impl SizedType {
    /// Int type
    pub fn int() -> Self {
        Int::new().into()
    }

    /// Pointer type
    pub fn pointer(kind: PointerKind, pointed: TypeId) -> Self {
        Pointer::new(kind, pointed).into()
    }

    /// Array type
    pub fn array(element: SizedTypeId, length: Size) -> Self {
        Array::new(element, length).into()
    }
}

/// Int type `int`
///
/// A single intcode cell, interpreted as a integer.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Constructor, Default)]
pub struct Int;

impl Int {
    /// As a scalar type, [`Int`] has a known type id
    pub const fn type_id() -> SizedTypeId {
        SizedTypeId(TypeId(NonZeroUsize::new(1).unwrap()))
    }

    /// Size of a int
    pub const fn size() -> Size {
        1
    }
}

/// Pointer type `&...`
///
/// A single cell pointing to a memory location
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Constructor)]
pub struct Pointer {
    /// Kind of the pointer
    kind: PointerKind,
    /// Pointed type
    pointed: TypeId,
}
impl Pointer {
    /// Size of a pointer
    pub const fn size() -> Size {
        1
    }
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
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Constructor)]
pub struct Array {
    /// Element of the array
    element: SizedTypeId,
    /// Length of the array
    length: Size,
}
