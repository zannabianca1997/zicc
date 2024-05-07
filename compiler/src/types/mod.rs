use std::{cell::RefCell, collections::BTreeMap, error::Error};

use either::Either;
use elsa::FrozenVec;
use thiserror::Error;
use vm::VMUInt;

use crate::ast::{
    tokens::Identifier,
    typedef::{PointerKindDef, TypeDef, TypeDefArray, TypeDefData, TypeDefFn},
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

#[derive(Debug, Error)]
pub enum RegisterError {
    #[error("A named type was not found")]
    NameNotFound(Identifier),
    #[error("A named unsized type was used in place of a sized one")]
    UnsizedInSized(Identifier),
    #[error("An array lenght was found to be unsolvable")]
    UnsolvableLenght(#[source] Box<dyn Error>),
    #[error("An array lenght was found to be negative")]
    NegativeArrayLength,
}

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
        defs: impl Iterator<Item = (Identifier, Either<&'d TypeDef, &'d TypeDefData>)>,
        solver: S,
    ) -> Result<Self, type_table_generation::TypeDeclareError> {
        type_table_generation::generate(defs, solver)
    }
}

impl<S: SizeExpressionSolver<RegisterError>> TypeTable<S> {
    /// Register a type into the table
    pub fn register(&self, def: &TypeDef) -> Result<TypeId, RegisterError> {
        // All loops in type definitions are already solved, so we can recurse fearlessly
        Ok(match def {
            // names are a special case, as they can refer to unsized types
            TypeDef::Data(TypeDefData::Named(name)) => *self
                .names
                .get(name)
                .ok_or(RegisterError::NameNotFound(*name))?,
            TypeDef::Data(def) => self.register_data(def)?.into_id(),
            TypeDef::Fn(TypeDefFn { inputs, output, .. }) => {
                let inputs = inputs
                    .iter()
                    .map(|inp| self.register_data(inp))
                    .try_collect()?;
                let output = output
                    .as_ref()
                    .map(|(_, out)| self.register_data(out))
                    .transpose()?
                    .unwrap_or(UNIT_ID);
                self.type_id(Type::Fn(TypeFn { inputs, output }))
            }
            TypeDef::Unknow(_) => UNKNOW_ID,
        })
    }
    /// Register a data type into the table
    pub fn register_data(&self, def: &TypeDefData) -> Result<TypeIdData, RegisterError> {
        // All loops in type definitions are already solved, so we can recurse fearlessly
        Ok(match def {
            TypeDefData::Int(_) => INT_ID,
            TypeDefData::Array(TypeDefArray {
                box element,
                lenght,
                ..
            }) => {
                let element = self.register_data(element)?;
                let lenght = self
                    .solver
                    .solve(lenght, |def| {
                        let id = self.register_data(def)?;
                        Ok(self.size_of(id))
                    })
                    .map_err(|err| RegisterError::UnsolvableLenght(Box::new(err)))?
                    .try_into()
                    .map_err(|_| RegisterError::NegativeArrayLength)?;
                self.type_id_data(TypeData::Array(TypeArray { element, lenght }))
            }
            TypeDefData::Struct(_) => todo!(),
            TypeDefData::Union(_) => todo!(),
            TypeDefData::Pointer(_) => todo!(),
            TypeDefData::Named(name) => {
                let id = *self
                    .names
                    .get(name)
                    .ok_or(RegisterError::NameNotFound(*name))?;
                if matches!(self.types[id.0].typ, Type::Data(_)) {
                    TypeIdData(id)
                } else {
                    return Err(RegisterError::UnsizedInSized(*name));
                }
            }
        })
    }
}

mod type_table_generation;
pub use type_table_generation::{SizeError, SizeExpressionSolver, TypeDeclareError};
