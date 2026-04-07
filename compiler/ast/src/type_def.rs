use crate::tokens::{
    identifiers::Identifier,
    int_literal::IntLiteral,
    keywords::{Int, Type},
    punctuators::{Ampersand, At, BraceClose, BracketOpen, Eq, Semicolon, Underscore},
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ItemTypeDef {
    pub k_type: Type,
    pub ident: Identifier,
    pub p_eq: Eq,
    pub def: TypeDef,
    pub p_semi: Semicolon,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TypeDef {
    Named(NamedTypeDef),
    Int(IntTypeDef),
    Unknown(UnknownTypeDef),
    Array(Box<ArrayTypeDef>),
    Pointer(Box<PointerTypeDef>),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NamedTypeDef {
    pub name: Identifier,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct IntTypeDef {
    pub k_int: Int,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ArrayTypeDef {
    pub p_bracket_open: BracketOpen,
    pub element: TypeDef,
    pub p_semicolon: Semicolon,
    pub length: IntLiteral,
    pub p_bracket_close: BraceClose,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct UnknownTypeDef {
    pub p_underscore: Underscore,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct PointerTypeDef {
    pub kind: PointerKindDef,
    pub pointed: TypeDef,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum PointerKindDef {
    Relative { p_at: At },
    Absolute { p_ampersand: Ampersand },
}
