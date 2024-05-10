#![feature(box_patterns)]

use std::collections::BTreeMap;

use elsa::FrozenVec;

use ast::{
    expression::const_expr::ConstExpressionSolver, tokens::Identifier, typedef::PointerKindDef,
    File,
};
use vm::VMUInt;

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
    /// Size of the type
    size: Option<VMUInt>,
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
        // Nope. Need to add it.
        // This should be rare, only when the compiler generate new type
        self.types.push(Box::new(TypeEntry {
            size: self.calculate_size(&searching),
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

    /// Return the size of a datatype
    pub fn size_of(&self, id: TypeIdData) -> VMUInt {
        self.types[id.0 .0]
            .size
            .expect("All datatypes should have a size")
    }

    pub fn is_data(&self, id: TypeId) -> bool {
        matches!(self.types[id.0].typ, Type::Data(_))
    }

    pub fn as_data(&self, id: TypeId) -> Option<TypeIdData> {
        self.is_data(id).then_some(TypeIdData(id))
    }

    /// Helper to calculate the size of a type
    fn calculate_size(&self, typ: &Type) -> Option<VMUInt> {
        match typ {
            Type::Data(typ) => Some(match typ {
                TypeData::Int(_) | TypeData::Pointer(_) => 1,
                TypeData::Array(TypeArray { element, lenght }) => self.size_of(*element) * lenght,
                TypeData::Composite(TypeComposite { size, .. }) => *size,
            }),
            Type::Fn(_) | Type::Unknow(_) => None,
        }
    }
}

impl TypeTable<ConstExpressionSolver> {
    /// Build a typetable from a AST
    pub fn build(ast: &File) -> Result<Self, type_table_generation::TypeDeclareError> {
        type_table_generation::generate(ast, ConstExpressionSolver)
    }
}

mod type_table_generation;
pub use type_table_generation::{SizeError, TypeDeclareError};
