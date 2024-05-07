use std::collections::BTreeMap;

use either::Either;

use crate::ast::{
    tokens::Identifier,
    typedef::{PointerKindDef, TypeDef, TypeDefData},
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

#[derive(Debug, Clone)]
pub struct TypeTable {
    types: Vec<TypeEntry>,
    names: BTreeMap<Identifier, TypeId>,
}

impl TypeTable {
    /// Create a table with all the declared names
    pub fn new<'d, Solver: type_table_generation::SizeExpressionSolver<'d>>(
        type_defs: impl Iterator<Item = (Identifier, Either<&'d TypeDef, &'d TypeDefData>)>,
        expr_solver: Solver,
    ) -> Result<Self, type_table_generation::TypeDeclareError<Solver::Error>> {
        type_table_generation::generate(type_defs, expr_solver)
    }

    /// Get the id of a type
    pub fn type_id(&mut self, searching: Type) -> TypeId {
        // first, let's check if we have it
        for (id, TypeEntry { typ, .. }) in self.types.iter().enumerate() {
            if typ == &searching {
                return TypeId(id);
            }
        }
        // Nope. Need to add it
        self.types.push(TypeEntry {
            typ: searching,
            name: None,
        });
        TypeId(self.types.len() - 1)
    }

    /// Get a type from a id
    pub fn type_(&mut self, searching: TypeId) -> &Type {
        &self.types[searching.0].typ
    }

    /// Get the id of a datatype
    pub fn type_id_data(&mut self, searching: TypeData) -> TypeIdData {
        TypeIdData(self.type_id(Type::Data(searching)))
    }

    /// Get a type from a id
    pub fn type_data(&mut self, searching: TypeId) -> &TypeData {
        &self.types[searching.0]
            .typ
            .as_data()
            .expect("The table should create TypeIdData only for actual datatypes")
    }
}

mod type_table_generation;
pub use type_table_generation::{SizeError, SizeExpressionSolver, TypeDeclareError};
