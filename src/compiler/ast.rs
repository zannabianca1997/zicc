//! ICC Ast

use std::collections::HashMap;

use super::{
    expression::Expression,
    typedef::{DataTypeDef, StructDef, TypeDef, UnionDef},
};

/// A ICC file
struct ICCFile {
    elements: Vec<Element>,
}

/// A ICC file-level element
enum Element {
    StructDef(StructDef),
    UnionDef(UnionDef),
    TypeDef(DataDeclarator),

    /// Declaration of a file-level variable or function
    Declaration {
        decl: Declarator,
    },

    /// Definition of a file-level variable
    Definition {
        decl: DataDeclarator,
        init: Option<Initializer>,
    },

    /// Definition of a function
    Function {
        decl: FunctionDeclarator,
        body: Vec<Expression>,
    },
}

struct Declarator {
    name: String,
    type_: TypeDef,
}

struct DataDeclarator {
    name: String,
    type_: DataTypeDef,
}

struct FunctionDeclarator {
    name: String,
    result: DataTypeDef,
    args: Vec<DataDeclarator>,
}

/// Variable initialization
enum Initializer {
    Scalar(isize),
    /// Pointer to another variable
    Pointer {
        dest: String,
    },
    Vector(Vec<Initializer>),
    Struct(HashMap<String, Initializer>),
}
