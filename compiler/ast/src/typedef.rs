use std::fmt::Write as _;

use display_context::DisplayWithContext;
use either::Either::{self, Left, Right};
use indenter::indented;
use string_interner::DefaultStringInterner;

use crate::ast_node::AstNode;

use super::{expression, punctuated, tokens::*};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TypeDef {
    Data(TypeDefData),
    Fn(TypeDefFn),
    Unknow(TypeDefUnknow),
}

impl AstNode for TypeDef {
    fn visited_by<Visitor: crate::ast_node::AstVisitor>(
        &self,
        visitor: &mut Visitor,
    ) -> Visitor::Result {
        todo!()
    }

    fn visited_by_mut<Visitor: crate::ast_node::AstVisitorMut>(
        &mut self,
        visitor: &mut Visitor,
    ) -> Visitor::Result {
        todo!()
    }
}

impl DisplayWithContext<DefaultStringInterner> for TypeDef {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        context: &DefaultStringInterner,
    ) -> std::fmt::Result {
        match self {
            TypeDef::Data(def) => DisplayWithContext::fmt(def, f, context),
            TypeDef::Fn(def) => DisplayWithContext::fmt(def, f, context),
            TypeDef::Unknow(def) => DisplayWithContext::fmt(def, f, context),
        }
    }
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

impl AstNode for TypeDefData {
    fn visited_by<Visitor: crate::ast_node::AstVisitor>(
        &self,
        visitor: &mut Visitor,
    ) -> Visitor::Result {
        todo!()
    }

    fn visited_by_mut<Visitor: crate::ast_node::AstVisitorMut>(
        &mut self,
        visitor: &mut Visitor,
    ) -> Visitor::Result {
        todo!()
    }
}

impl DisplayWithContext<DefaultStringInterner> for TypeDefData {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        context: &DefaultStringInterner,
    ) -> std::fmt::Result {
        match self {
            TypeDefData::Int(def) => DisplayWithContext::fmt(def, f, context),
            TypeDefData::Array(def) => DisplayWithContext::fmt(def, f, context),
            TypeDefData::Struct(def) => DisplayWithContext::fmt(def, f, context),
            TypeDefData::Union(def) => DisplayWithContext::fmt(def, f, context),
            TypeDefData::Pointer(def) => DisplayWithContext::fmt(def, f, context),
            TypeDefData::Named(def) => DisplayWithContext::fmt(def, f, context),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeDefInt {
    pub int_kwd: KeywordInt,
}

impl DisplayWithContext<DefaultStringInterner> for TypeDefInt {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        context: &DefaultStringInterner,
    ) -> std::fmt::Result {
        DisplayWithContext::fmt(&self.int_kwd, f, context)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeDefArray {
    pub bracket_open: PunctBracketOpen,
    pub element: Box<TypeDefData>,
    pub semi: PunctSemi,
    pub lenght: expression::Expression,
    pub bracket_close: PunctBracketClose,
}

impl DisplayWithContext<DefaultStringInterner> for TypeDefArray {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        context: &DefaultStringInterner,
    ) -> std::fmt::Result {
        DisplayWithContext::fmt(&self.bracket_open, f, context)?;
        DisplayWithContext::fmt(&self.element, f, context)?;
        DisplayWithContext::fmt(&self.semi, f, context)?;
        f.write_str(" ")?;
        DisplayWithContext::fmt(&self.lenght, f, context)?;
        DisplayWithContext::fmt(&self.bracket_close, f, context)?;

        Ok(())
    }
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
    pub brace_close: PunctBraceClose,
}

impl DisplayWithContext<DefaultStringInterner> for TypeDefStruct {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        context: &DefaultStringInterner,
    ) -> std::fmt::Result {
        DisplayWithContext::fmt(&self.struct_kw, f, context)?;
        if let Some(name) = &self.name {
            f.write_str(" ")?;
            DisplayWithContext::fmt(name, f, context)?;
        }
        f.write_str(" ")?;
        DisplayWithContext::fmt(&self.brace_open, f, context)?;
        display_fields(&self.fields, f, context)?;
        DisplayWithContext::fmt(&self.brace_close, f, context)?;
        Ok(())
    }
}
impl DisplayWithContext<DefaultStringInterner> for TypeDefUnion {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        context: &DefaultStringInterner,
    ) -> std::fmt::Result {
        DisplayWithContext::fmt(&self.union_kw, f, context)?;
        if let Some(name) = &self.name {
            f.write_str(" ")?;
            DisplayWithContext::fmt(name, f, context)?;
        }
        f.write_str(" ")?;
        DisplayWithContext::fmt(&self.brace_open, f, context)?;
        display_fields(&self.variants, f, context)?;
        DisplayWithContext::fmt(&self.brace_close, f, context)?;
        Ok(())
    }
}

fn display_fields(
    fields: &punctuated::Punctuated<
        (Either<Identifier, PunctUnderscore>, PunctColon, TypeDefData),
        PunctComma,
    >,
    f: &mut std::fmt::Formatter,
    context: &string_interner::StringInterner<string_interner::backend::StringBackend>,
) -> Result<(), std::fmt::Error> {
    {
        let mut f = indented(f);
        for field_or_comma in fields.iter_all() {
            match field_or_comma {
                Either::Left((name, colon, ty)) => {
                    write!(
                        f,
                        "\n{}{} {}",
                        name.with_context(context),
                        colon.with_context(context),
                        ty.with_context(context)
                    )?;
                }
                Either::Right(comma) => {
                    write!(f, "{}", comma.with_context(context))?;
                }
            }
        }
    }
    Ok(if !fields.is_empty() {
        f.write_str("\n")?;
    })
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
    pub brace_close: PunctBraceClose,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeDefPointer {
    pub kind: PointerKindDef,
    pub pointee: Box<TypeDef>,
}

impl DisplayWithContext<DefaultStringInterner> for TypeDefPointer {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        context: &DefaultStringInterner,
    ) -> std::fmt::Result {
        DisplayWithContext::fmt(&self.kind, f, context)?;
        DisplayWithContext::fmt(&self.pointee, f, context)?;
        Ok(())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum PointerKindDef {
    Stack(PunctAt),
    Static(PunctAmpersand),
}

impl DisplayWithContext<DefaultStringInterner> for PointerKindDef {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        context: &DefaultStringInterner,
    ) -> std::fmt::Result {
        match self {
            PointerKindDef::Stack(kind) => DisplayWithContext::fmt(kind, f, context),
            PointerKindDef::Static(kind) => DisplayWithContext::fmt(kind, f, context),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeDefFn {
    pub fn_kw: KeywordFn,
    pub paren_open: PunctParenOpen,
    pub inputs: punctuated::Punctuated<TypeDefData, PunctComma>,
    pub paren_close: PunctParenClose,
    pub output: Option<(PunctRightArrow, TypeDefData)>,
}

impl DisplayWithContext<DefaultStringInterner> for TypeDefFn {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        context: &DefaultStringInterner,
    ) -> std::fmt::Result {
        DisplayWithContext::fmt(&self.fn_kw, f, context)?;
        DisplayWithContext::fmt(&self.paren_open, f, context)?;
        for comma_or_input in self.inputs.iter_all() {
            match comma_or_input {
                Left(ty) => {
                    DisplayWithContext::fmt(ty, f, context)?;
                }
                Right(comma) => {
                    DisplayWithContext::fmt(comma, f, context)?;
                    f.write_str(" ")?;
                }
            }
        }
        DisplayWithContext::fmt(&self.paren_close, f, context)?;
        if let Some((arrow, output)) = &self.output {
            f.write_str(" ")?;
            DisplayWithContext::fmt(arrow, f, context)?;
            f.write_str(" ")?;
            DisplayWithContext::fmt(output, f, context)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeDefUnknow {
    pub underscore: PunctUnderscore,
}

impl DisplayWithContext<DefaultStringInterner> for TypeDefUnknow {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        context: &DefaultStringInterner,
    ) -> std::fmt::Result {
        DisplayWithContext::fmt(&self.underscore, f, context)
    }
}
