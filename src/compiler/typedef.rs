//! Type definitions

use std::collections::HashMap;

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

/// Definition of a field in a composite type
pub(super) enum FieldDef {
    Named(String, TypeDef),
    UnnamedStruct(StructDef),
    UnnamedUnion(UnionDef),
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

/// Definition of a type
pub(super) enum TypeDef {
    DataType(DataTypeDef),
    Void,
    Function {
        result: Option<DataTypeDef>,
        args: Vec<DataTypeDef>,
    },
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
