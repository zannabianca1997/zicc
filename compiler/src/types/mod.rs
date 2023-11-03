use std::{iter::zip, rc::Rc};

use not_empty::NonEmptySlice;

/// Any type ICC can manage
pub enum Type {
    /// A type with a lenght
    Data(TypeData),
    /// A function
    Function(TypeFunction),
    /// Unknow type
    Unknow(TypeUnknow),
}

pub struct TypeFunction {
    pub inputs: Rc<[TypeData]>,
    pub output: TypeData,
}

pub struct TypeUnknow;

/// A type representing a string of data in memory
pub enum TypeData {
    /// A single intcode cell
    Int(TypeInt),
    /// An omogeneus, fixed-lenght array
    Array(TypeArray),
    /// A disomogeneus, named or unnamed, tuple
    Struct(TypeStruct),
    /// Any one of the subtypes
    Union(TypeUnion),
    /// A single cell, pointing to a type
    Pointer(TypePointer),
}

pub struct TypeInt;

pub struct TypeArray {
    pub element: Rc<TypeData>,
    pub lenght: usize,
}

pub struct FieldUnnamed {
    types: Rc<[TypeData]>,
}
impl FieldUnnamed {
    pub fn types(&self) -> &[TypeData] {
        &self.types
    }
}

pub struct FieldNamed {
    names: Rc<[Rc<str>]>,
    types: Rc<[TypeData]>,
}
impl FieldNamed {
    pub fn names(&self) -> &[Rc<str>] {
        &self.names
    }
    pub fn types(&self) -> &[TypeData] {
        &self.types
    }
}

pub enum TypeStruct {
    Unnamed(FieldUnnamed),
    Named(FieldNamed),
}

impl TypeStruct {
    pub fn names(&self) -> Option<&[Rc<str>]> {
        match self {
            TypeStruct::Unnamed(_) => None,
            TypeStruct::Named(fields) => Some(fields.names()),
        }
    }
    pub fn types(&self) -> &[TypeData] {
        match self {
            TypeStruct::Unnamed(fields) => fields.types(),
            TypeStruct::Named(fields) => fields.types(),
        }
    }
}
pub struct TypeUnion {
    names: Rc<NonEmptySlice<Rc<str>>>,
    types: Rc<NonEmptySlice<TypeData>>,
}
impl TypeUnion {
    pub fn variants(&self) -> impl Iterator<Item = (&str, &TypeData)> {
        zip(self.names.iter().map(|s| &**s), self.types.iter())
    }
    pub fn names(&self) -> &NonEmptySlice<Rc<str>> {
        &self.names
    }
    pub fn types(&self) -> &NonEmptySlice<TypeData> {
        &self.types
    }
}

pub enum TypePointer {
    Absolute { pointed: Box<Type> },
    Relative { pointed: Box<Type> },
}

impl TypePointer {
    pub fn pointed(&self) -> &Type {
        let (Self::Absolute { pointed, .. } | Self::Relative { pointed, .. }) = self;
        pointed
    }
    pub fn pointed_mut(&mut self) -> &mut Type {
        let (Self::Absolute { pointed, .. } | Self::Relative { pointed, .. }) = self;
        pointed
    }

    #[must_use]
    pub fn as_absolute(&self) -> Option<&Type> {
        if let Self::Absolute { pointed: pointee } = self {
            Some(pointee)
        } else {
            None
        }
    }

    #[must_use]
    pub fn as_relative(&self) -> Option<&Type> {
        if let Self::Relative { pointed: pointee } = self {
            Some(pointee)
        } else {
            None
        }
    }

    /// Returns `true` if the type pointer is [`Absolute`].
    ///
    /// [`Absolute`]: TypePointer::Absolute
    #[must_use]
    pub fn is_absolute(&self) -> bool {
        matches!(self, Self::Absolute { .. })
    }

    /// Returns `true` if the type pointer is [`Relative`].
    ///
    /// [`Relative`]: TypePointer::Relative
    #[must_use]
    pub fn is_relative(&self) -> bool {
        matches!(self, Self::Relative { .. })
    }
}

pub trait TypeSized {
    fn size(&self) -> usize;
}
impl TypeSized for TypeData {
    fn size(&self) -> usize {
        match self {
            TypeData::Int(this) => this.size(),
            TypeData::Array(this) => this.size(),
            TypeData::Struct(this) => this.size(),
            TypeData::Union(this) => this.size(),
            TypeData::Pointer(this) => this.size(),
        }
    }
}
impl TypeSized for TypeInt {
    fn size(&self) -> usize {
        1
    }
}
impl TypeSized for TypeArray {
    fn size(&self) -> usize {
        self.lenght * self.element.size()
    }
}
impl TypeSized for TypeStruct {
    fn size(&self) -> usize {
        self.types().into_iter().map(TypeSized::size).sum()
    }
}
impl TypeSized for TypeUnion {
    fn size(&self) -> usize {
        self.types().into_iter().map(TypeSized::size).max().unwrap()
    }
}
impl TypeSized for TypePointer {
    fn size(&self) -> usize {
        1
    }
}

pub use transmute::Trasmutable;
mod transmute;
