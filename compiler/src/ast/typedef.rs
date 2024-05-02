use either::Either;

use crate::span::Spanned;

use super::{expression, punctuated, tokens::*};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TypeDef {
    Data(TypeDefData),
    Fn(TypeDefFn),
    Unknow(TypeDefUnknow),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TypeDefData {
    Int(TypeDefInt),
    Array(TypeDefArray),
    Struct(TypeDefStruct),
    Union(TypeDefUnion),
    Pointer(TypeDefPointer),
    Named(Identifier),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeDefInt {
    pub int_kwd: KeywordInt,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeDefArray {
    pub bracket_open: PunctBracketOpen,
    pub element: Box<TypeDefData>,
    pub semi: PunctSemi,
    pub lenght: expression::Expression,
    pub bracket_close: PunctBracketClose,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeDefStruct {
    pub struct_kw: KeywordStruct,
    pub name: Option<Identifier>,
    pub brace_open: PunctBraceOpen,
    pub fields: punctuated::Punctuated<
        (Either<Identifier, PunctUnderscore>, PunctColon, TypeDefData),
        PunctComma,
    >,
    pub bracket_close: PunctBraceClose,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeDefUnion {
    pub union_kw: KeywordUnion,
    pub name: Option<Identifier>,
    pub brace_open: PunctBraceOpen,
    pub variants: punctuated::Punctuated<
        (Either<Identifier, PunctUnderscore>, PunctColon, TypeDefData),
        PunctComma,
    >,
    pub bracket_close: PunctBraceClose,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeDefPointer {
    pub kind: PointerKindDef,
    pub pointee: Box<TypeDef>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum PointerKindDef {
    Stack(PunctAt),
    Static(PunctAmpersand),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeDefFn {
    pub fn_kw: KeywordFn,
    pub paren_open: PunctParenOpen,
    pub inputs: punctuated::Punctuated<TypeDefData, PunctComma>,
    pub paren_close: PunctParenClose,
    pub output: Option<(PunctRightArrow, TypeDefData)>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeDefUnknow {
    pub underscore: PunctUnderscore,
}
