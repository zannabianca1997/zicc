//! Type definitions

use std::collections::{BTreeMap, HashMap, HashSet};

use itertools::Itertools;
use thiserror::Error;

use super::types::{Composite, CompositeJoinError, DataType, Type};

/// Definition of a struct type
pub(super) enum StructDef {
    /// Defined before
    Referenced(String),
    /// Defined here
    Defined {
        /// Optional name to reference it after
        name: Option<String>,
        /// Fields of the struct
        fields: Vec<FieldDef>,
    },
}
impl StructDef {
    fn solve(self, defs: &mut Defs) -> Result<Composite, TypeDefError> {
        match self {
            StructDef::Referenced(name) => {
                if let Some(s) = defs.structs.get(&name) {
                    Ok(s.clone())
                } else {
                    Err(TypeDefError::UnknowStruct(name))
                }
            }
            StructDef::Defined { name, fields } => {
                // solve fields
                let mut result = Composite::empty();
                let mut offset = 0;
                for field in fields {
                    // solve the new field(s)
                    let additional_fields = field.solve_fields(defs)?;
                    let additional_fields_size = additional_fields.size();
                    // add the new fields
                    result = result.join(additional_fields.offset(offset))?;
                    // update offset
                    offset += additional_fields_size;
                }
                // eventually store the result
                if let Some(name) = name {
                    if !defs.structs.contains_key(&name) {
                        defs.structs.insert(name, result.clone());
                    } else {
                        return Err(TypeDefError::RedefinedStruct(name));
                    }
                }

                Ok(result)
            }
        }
    }
}

/// Definition of a union type
pub(super) enum UnionDef {
    /// Defined before
    Referenced(String),
    /// Defined here
    Defined {
        /// Optional name to reference it after
        name: Option<String>,
        /// Fields of the struct
        fields: Vec<FieldDef>,
    },
}
impl UnionDef {
    fn solve(self, defs: &mut Defs) -> Result<Composite, TypeDefError> {
        match self {
            UnionDef::Referenced(name) => {
                if let Some(s) = defs.unions.get(&name) {
                    Ok(s.clone())
                } else {
                    Err(TypeDefError::UnknowUnion(name))
                }
            }
            UnionDef::Defined { name, fields } => {
                // solve fields
                let mut result = Composite::empty();
                for field in fields {
                    // solve the new field(s)
                    let additional_fields = field.solve_fields(defs)?;
                    // add the new fields
                    result = result.join(additional_fields)?;
                }
                // eventually store the result
                if let Some(name) = name {
                    if !defs.unions.contains_key(&name) {
                        defs.unions.insert(name, result.clone());
                    } else {
                        return Err(TypeDefError::RedefinedUnion(name));
                    }
                }
                Ok(result)
            }
        }
    }
}

/// Definition of a field in a composite type
pub(super) enum FieldDef {
    Named(String, DataTypeDef),
    UnnamedStruct(StructDef),
    UnnamedUnion(UnionDef),
}
impl FieldDef {
    fn solve_fields(self, defs: &mut Defs) -> Result<Composite, TypeDefError> {
        match self {
            FieldDef::Named(name, t) => Ok(Composite {
                fields: BTreeMap::from([(name, (0usize, t.solve(defs)?))]),
            }),
            FieldDef::UnnamedStruct(s) => s.solve(defs),
            FieldDef::UnnamedUnion(u) => u.solve(defs),
        }
    }
}

/// Definition of a enumeration type
pub(super) enum EnumDef {
    /// Defined before
    Referenced(String),
    /// Defined here
    Defined {
        /// Optional name to reference it after
        name: Option<String>,
        /// Variants of the enum
        variants: Vec<(String, Option<isize>)>,
    },
}
impl EnumDef {
    fn solve(self, defs: &mut Defs) -> Result<DataType, TypeDefError> {
        match self {
            EnumDef::Referenced(name) => {
                if defs.enums.contains(&name) {
                    Ok(DataType::Scalar)
                } else {
                    Err(TypeDefError::UnknowEnum(name))
                }
            }
            EnumDef::Defined { name, variants } => {
                // saving this as a possible enum type
                if let Some(name) = name {
                    if defs.enums.contains(&name) {
                        return Err(TypeDefError::RedefinedEnum(name));
                    } else {
                        defs.enums.insert(name)
                    };
                }
                // saving variant names, with numbered values
                let mut counter = 0;
                for (variant, value) in variants {
                    if let Some(value) = value {
                        counter = value
                    };
                    defs.enum_variants.insert(variant, counter);
                    counter += 1;
                }
                Ok(DataType::Scalar)
            }
        }
    }
}

/// Definition of a type
pub(super) enum TypeDef {
    DataType(DataTypeDef),
    Void,
    Function {
        result: Option<DataTypeDef>,
        args: Vec<DataTypeDef>,
    },
    /// Reference to a defined type
    Reference(String),
}

impl TypeDef {
    pub(super) fn solve(self, defs: &mut Defs) -> Result<Type, TypeDefError> {
        Ok(match self {
            TypeDef::DataType(dt) => Type::DataType(dt.solve(defs)?),
            TypeDef::Void => Type::Void,
            TypeDef::Function { result, args } => Type::Function {
                result: result.map(|r| r.solve(defs)).transpose()?,
                args: args.into_iter().map(|a| a.solve(defs)).try_collect()?,
            },
            TypeDef::Reference(name) => {
                if let Some(t) = defs.types.get(&name) {
                    t.clone()
                } else {
                    return Err(TypeDefError::UnknowType(name));
                }
            }
        })
    }
}

/// Definition of a data type
pub(super) enum DataTypeDef {
    Scalar,
    Pointer(Box<TypeDef>),
    Array {
        element: Box<DataTypeDef>,
        lenght: usize,
    },
    Struct(StructDef),
    Union(UnionDef),
    Enum(EnumDef),
}
impl DataTypeDef {
    fn solve(self, defs: &mut Defs) -> Result<DataType, TypeDefError> {
        Ok(match self {
            DataTypeDef::Scalar => DataType::Scalar,
            DataTypeDef::Pointer(dest) => DataType::Pointer(Box::new(dest.solve(defs)?)),
            DataTypeDef::Array { element, lenght } => DataType::Array {
                element: Box::new(element.solve(defs)?),
                lenght,
            },
            DataTypeDef::Struct(sd) => DataType::Composite(sd.solve(defs)?),
            DataTypeDef::Union(ud) => DataType::Composite(ud.solve(defs)?),
            DataTypeDef::Enum(ed) => ed.solve(defs)?,
        })
    }
}
/// Error in type resolution
#[derive(Debug, Error)]
pub enum TypeDefError {
    #[error("Unknow enum {0}")]
    UnknowEnum(String),
    #[error("Redefined enum {0}")]
    RedefinedEnum(String),
    #[error("Unknow type {0}")]
    UnknowType(String),
    #[error("Unknow struct {0}")]
    UnknowStruct(String),
    #[error("Redefined struct {0}")]
    RedefinedStruct(String),
    #[error("Unknow union {0}")]
    UnknowUnion(String),
    #[error("Redefined union {0}")]
    RedefinedUnion(String),
    #[error(transparent)]
    DuplicateField(#[from] CompositeJoinError),
}

/// Definitions of non-objects (struct and enum)
pub(super) struct Defs {
    structs: HashMap<String, Composite>,
    unions: HashMap<String, Composite>,
    enums: HashSet<String>,
    enum_variants: HashMap<String, isize>,
    types: HashMap<String, Type>,
}
