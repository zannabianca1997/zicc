use std::{cell::RefCell, collections::BTreeMap, error::Error};

use either::Either::{self, Left};
use elsa::FrozenVec;
use itertools::Itertools;
use thiserror::Error;
use vm::VMUInt;

use crate::ast::{
    tokens::Identifier,
    typedef::{
        PointerKindDef, TypeDef, TypeDefArray, TypeDefData, TypeDefFn, TypeDefPointer,
        TypeDefStruct, TypeDefUnion,
    },
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeId(usize);

pub const UNKNOW_ID: TypeId = TypeId(1);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeIdData(TypeId);

impl TypeIdData {
    pub fn id(&self) -> &TypeId {
        &self.0
    }
    pub fn into_id(self) -> TypeId {
        self.0
    }
}

impl From<TypeIdData> for TypeId {
    fn from(value: TypeIdData) -> Self {
        value.0
    }
}

impl std::ops::Deref for TypeIdData {
    type Target = TypeId;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

pub const INT_ID: TypeIdData = TypeIdData(TypeId(0));
pub const UNIT_ID: TypeIdData = TypeIdData(TypeId(2));

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Type {
    Data(TypeData),
    Fn(TypeFn),
    Unknow(TypeUnknow),
}

impl Type {
    /// Returns `true` if the type is [`Data`].
    ///
    /// [`Data`]: Type::Data
    #[must_use]
    pub fn is_data(&self) -> bool {
        matches!(self, Self::Data(..))
    }

    pub fn as_data(&self) -> Option<&TypeData> {
        if let Self::Data(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub fn try_into_data(self) -> Result<TypeData, Self> {
        if let Self::Data(v) = self {
            Ok(v)
        } else {
            Err(self)
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TypeData {
    Int(TypeInt),
    Array(TypeArray),
    Composite(TypeComposite),
    Pointer(TypePointer),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeInt;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeArray {
    pub element: TypeIdData,
    pub lenght: vm::VMUInt,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeComposite {
    pub name: Option<Identifier>,
    pub fields: BTreeMap<Identifier, (vm::VMUInt, TypeIdData)>,
    pub size: vm::VMUInt,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypePointer {
    pub kind: PointerKind,
    pub pointee: TypeId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum PointerKind {
    Stack,
    Static,
}

impl From<PointerKindDef> for PointerKind {
    fn from(value: PointerKindDef) -> Self {
        match value {
            PointerKindDef::Stack(_) => Self::Stack,
            PointerKindDef::Static(_) => Self::Static,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeFn {
    pub inputs: Vec<TypeIdData>,
    pub output: TypeIdData,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeUnknow;

#[derive(Debug, Clone)]
struct TypeEntry {
    /// The type
    typ: Type,
    /// Optional specific name of the type, if declared
    name: Option<Identifier>,
}

#[derive(Clone)]
pub struct TypeTable<Solver> {
    types: FrozenVec<Box<TypeEntry>>,
    names: BTreeMap<Identifier, TypeId>,
    pub solver: Solver,
}

impl<S> TypeTable<S> {
    /// Get the id of a type
    pub fn type_id(&self, searching: Type) -> TypeId {
        // first, let's check if we have it
        for (id, TypeEntry { typ, .. }) in self.types.iter().enumerate() {
            if typ == &searching {
                return TypeId(id);
            }
        }
        // Nope. Need to add it
        self.types.push(Box::new(TypeEntry {
            typ: searching,
            name: None,
        }));
        TypeId(self.types.len() - 1)
    }

    /// Get a type from a id
    pub fn type_(&self, searching: TypeId) -> &Type {
        &self.types[searching.0].typ
    }

    /// Get the id of a datatype
    pub fn type_id_data(&self, searching: TypeData) -> TypeIdData {
        TypeIdData(self.type_id(Type::Data(searching)))
    }

    /// Get a type from a id
    pub fn type_data(&self, searching: TypeId) -> &TypeData {
        &self.types[searching.0]
            .typ
            .as_data()
            .expect("The table should create TypeIdData only for actual datatypes")
    }

    /// Change the expression solver
    ///
    /// This is sould only if the new expression solver resolve MORE expressions than the older one, giving results where the old one would have failed
    /// Is a logic error to give a new solver that return different values for the same expression
    pub fn with_solver<NS>(self, solver: NS) -> TypeTable<NS> {
        let Self {
            types,
            names,
            solver: _,
        } = self;
        TypeTable {
            types,
            names,
            solver,
        }
    }

    fn size_of(&self, id: TypeIdData) -> VMUInt {
        todo!()
    }
}

impl<S: SizeExpressionSolver<SizeError>> TypeTable<S> {
    /// Create a table with all the declared names
    pub fn new<'d>(
        type_defs: impl Iterator<Item = (Identifier, &'d TypeDef)>,
        type_used: impl Iterator<Item = Either<&'d TypeDef, &'d TypeDefData>>,
        solver: S,
    ) -> Result<Self, type_table_generation::TypeDeclareError> {
        type_table_generation::generate(
            type_defs
                .map(|(name, def)| (Some(name), Left(def)))
                .chain(type_used.map(|def| (None, def))),
            solver,
        )
    }
}

mod type_table_generation;
pub use type_table_generation::{SizeError, SizeExpressionSolver, TypeDeclareError};
