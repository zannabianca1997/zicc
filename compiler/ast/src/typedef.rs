use std::fmt::Write as _;

use display_context::DisplayWithContext;
use either::Either::{self, Left, Right};
use indenter::indented;
use string_interner::DefaultStringInterner;

use crate::{
    ast_node::{AstNode, AstVisitor, AstVisitorMut},
    extractors,
};

use super::{expression, punctuated, tokens::*};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TypeDef {
    Data(TypeDefData),
    Fn(TypeDefFn),
    Unknow(TypeDefUnknow),
}

impl AstNode for TypeDef {
    extractors! {TypeDef}
    fn visited_by<'s, Visitor: AstVisitor<'s>>(&'s self, visitor: &mut Visitor) -> Visitor::Result {
        let mut child_visitor = visitor.enter(self);
        match self {
            TypeDef::Data(def) => def.visited_by(&mut child_visitor),
            TypeDef::Fn(def) => def.visited_by(&mut child_visitor),
            TypeDef::Unknow(def) => def.visited_by(&mut child_visitor),
        };
        visitor.exit(self, child_visitor)
    }

    fn visited_by_mut<Visitor: AstVisitorMut>(&mut self, visitor: &mut Visitor) -> Visitor::Result {
        let mut child_visitor = visitor.enter_mut(self);
        match self {
            TypeDef::Data(def) => def.visited_by_mut(&mut child_visitor),
            TypeDef::Fn(def) => def.visited_by_mut(&mut child_visitor),
            TypeDef::Unknow(def) => def.visited_by_mut(&mut child_visitor),
        };
        visitor.exit_mut(self, child_visitor)
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
    extractors! {TypeDefData}
    fn visited_by<'s, Visitor: AstVisitor<'s>>(&'s self, visitor: &mut Visitor) -> Visitor::Result {
        let mut child_visitor = visitor.enter(self);
        match self {
            TypeDefData::Int(def) => def.visited_by(&mut child_visitor),
            TypeDefData::Array(def) => def.visited_by(&mut child_visitor),
            TypeDefData::Struct(def) => def.visited_by(&mut child_visitor),
            TypeDefData::Union(def) => def.visited_by(&mut child_visitor),
            TypeDefData::Pointer(def) => def.visited_by(&mut child_visitor),
            TypeDefData::Named(def) => def.visited_by(&mut child_visitor),
        };
        visitor.exit(self, child_visitor)
    }

    fn visited_by_mut<Visitor: AstVisitorMut>(&mut self, visitor: &mut Visitor) -> Visitor::Result {
        let mut child_visitor = visitor.enter_mut(self);
        match self {
            TypeDefData::Int(def) => def.visited_by_mut(&mut child_visitor),
            TypeDefData::Array(def) => def.visited_by_mut(&mut child_visitor),
            TypeDefData::Struct(def) => def.visited_by_mut(&mut child_visitor),
            TypeDefData::Union(def) => def.visited_by_mut(&mut child_visitor),
            TypeDefData::Pointer(def) => def.visited_by_mut(&mut child_visitor),
            TypeDefData::Named(def) => def.visited_by_mut(&mut child_visitor),
        };
        visitor.exit_mut(self, child_visitor)
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
impl AstNode for TypeDefInt {
    extractors! {TypeDefInt}
    fn visited_by<'s, Visitor: AstVisitor<'s>>(&'s self, visitor: &mut Visitor) -> Visitor::Result {
        let mut child_visitor = visitor.enter(self);
        self.int_kwd.visited_by(&mut child_visitor);
        visitor.exit(self, child_visitor)
    }

    fn visited_by_mut<Visitor: AstVisitorMut>(&mut self, visitor: &mut Visitor) -> Visitor::Result {
        let mut child_visitor = visitor.enter_mut(self);
        self.int_kwd.visited_by_mut(&mut child_visitor);
        visitor.exit_mut(self, child_visitor)
    }
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

impl AstNode for TypeDefArray {
    extractors! {TypeDefArray}
    fn visited_by<'s, Visitor: AstVisitor<'s>>(&'s self, visitor: &mut Visitor) -> Visitor::Result {
        let mut child_visitor = visitor.enter(self);
        self.bracket_open.visited_by(&mut child_visitor);
        self.element.visited_by(&mut child_visitor);
        self.semi.visited_by(&mut child_visitor);
        self.lenght.visited_by(&mut child_visitor);
        self.bracket_close.visited_by(&mut child_visitor);
        visitor.exit(self, child_visitor)
    }

    fn visited_by_mut<Visitor: AstVisitorMut>(&mut self, visitor: &mut Visitor) -> Visitor::Result {
        let mut child_visitor = visitor.enter_mut(self);
        self.bracket_open.visited_by_mut(&mut child_visitor);
        self.element.visited_by_mut(&mut child_visitor);
        self.semi.visited_by_mut(&mut child_visitor);
        self.lenght.visited_by_mut(&mut child_visitor);
        self.bracket_close.visited_by_mut(&mut child_visitor);
        visitor.exit_mut(self, child_visitor)
    }
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

impl AstNode for TypeDefStruct {
    extractors! {TypeDefStruct}
    fn visited_by<'s, Visitor: AstVisitor<'s>>(&'s self, visitor: &mut Visitor) -> Visitor::Result {
        let mut child_visitor = visitor.enter(self);
        self.struct_kw.visited_by(&mut child_visitor);
        if let Some(name) = &self.name {
            name.visited_by(&mut child_visitor);
        }
        self.brace_open.visited_by(&mut child_visitor);
        for child in self.fields.iter_all() {
            match child {
                Left((name, colon, ty)) => {
                    match name {
                        Left(name) => name.visited_by(&mut child_visitor),
                        Right(under) => under.visited_by(&mut child_visitor),
                    };
                    colon.visited_by(&mut child_visitor);
                    ty.visited_by(&mut child_visitor);
                }
                Right(comma) => {
                    comma.visited_by(&mut child_visitor);
                }
            }
        }
        self.brace_close.visited_by(&mut child_visitor);
        visitor.exit(self, child_visitor)
    }

    fn visited_by_mut<Visitor: AstVisitorMut>(&mut self, visitor: &mut Visitor) -> Visitor::Result {
        let mut child_visitor = visitor.enter_mut(self);
        self.struct_kw.visited_by_mut(&mut child_visitor);
        if let Some(name) = &mut self.name {
            name.visited_by_mut(&mut child_visitor);
        }
        self.brace_open.visited_by_mut(&mut child_visitor);
        for child in self.fields.iter_all_mut() {
            match child {
                Left((name, colon, ty)) => {
                    match name {
                        Left(name) => name.visited_by_mut(&mut child_visitor),
                        Right(under) => under.visited_by_mut(&mut child_visitor),
                    };
                    colon.visited_by_mut(&mut child_visitor);
                    ty.visited_by_mut(&mut child_visitor);
                }
                Right(comma) => {
                    comma.visited_by_mut(&mut child_visitor);
                }
            }
        }
        self.brace_close.visited_by_mut(&mut child_visitor);
        visitor.exit_mut(self, child_visitor)
    }
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

impl AstNode for TypeDefUnion {
    extractors! {TypeDefUnion}
    fn visited_by<'s, Visitor: AstVisitor<'s>>(&'s self, visitor: &mut Visitor) -> Visitor::Result {
        let mut child_visitor = visitor.enter(self);
        self.union_kw.visited_by(&mut child_visitor);
        if let Some(name) = &self.name {
            name.visited_by(&mut child_visitor);
        }
        self.brace_open.visited_by(&mut child_visitor);
        for child in self.variants.iter_all() {
            match child {
                Left((name, colon, ty)) => {
                    match name {
                        Left(name) => name.visited_by(&mut child_visitor),
                        Right(under) => under.visited_by(&mut child_visitor),
                    };
                    colon.visited_by(&mut child_visitor);
                    ty.visited_by(&mut child_visitor);
                }
                Right(comma) => {
                    comma.visited_by(&mut child_visitor);
                }
            }
        }
        self.brace_close.visited_by(&mut child_visitor);
        visitor.exit(self, child_visitor)
    }

    fn visited_by_mut<Visitor: AstVisitorMut>(&mut self, visitor: &mut Visitor) -> Visitor::Result {
        let mut child_visitor = visitor.enter_mut(self);
        self.union_kw.visited_by_mut(&mut child_visitor);
        if let Some(name) = &mut self.name {
            name.visited_by_mut(&mut child_visitor);
        }
        self.brace_open.visited_by_mut(&mut child_visitor);
        for child in self.variants.iter_all_mut() {
            match child {
                Left((name, colon, ty)) => {
                    match name {
                        Left(name) => name.visited_by_mut(&mut child_visitor),
                        Right(under) => under.visited_by_mut(&mut child_visitor),
                    };
                    colon.visited_by_mut(&mut child_visitor);
                    ty.visited_by_mut(&mut child_visitor);
                }
                Right(comma) => {
                    comma.visited_by_mut(&mut child_visitor);
                }
            }
        }
        self.brace_close.visited_by_mut(&mut child_visitor);
        visitor.exit_mut(self, child_visitor)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeDefPointer {
    pub kind: PointerKindDef,
    pub pointee: Box<TypeDef>,
}

impl AstNode for TypeDefPointer {
    extractors! {TypeDefPointer}
    fn visited_by<'s, Visitor: AstVisitor<'s>>(&'s self, visitor: &mut Visitor) -> Visitor::Result {
        let mut child_visitor = visitor.enter(self);
        self.kind.visited_by(&mut child_visitor);
        self.pointee.visited_by(&mut child_visitor);
        visitor.exit(self, child_visitor)
    }

    fn visited_by_mut<Visitor: AstVisitorMut>(&mut self, visitor: &mut Visitor) -> Visitor::Result {
        let mut child_visitor = visitor.enter_mut(self);
        self.kind.visited_by_mut(&mut child_visitor);
        self.pointee.visited_by_mut(&mut child_visitor);
        visitor.exit_mut(self, child_visitor)
    }
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

impl AstNode for PointerKindDef {
    extractors! {PointerKindDef}
    fn visited_by<'s, Visitor: AstVisitor<'s>>(&'s self, visitor: &mut Visitor) -> Visitor::Result {
        let mut child_visitor = visitor.enter(self);
        match self {
            PointerKindDef::Stack(at) => at.visited_by(&mut child_visitor),
            PointerKindDef::Static(amp) => amp.visited_by(&mut child_visitor),
        };
        visitor.exit(self, child_visitor)
    }

    fn visited_by_mut<Visitor: AstVisitorMut>(&mut self, visitor: &mut Visitor) -> Visitor::Result {
        let mut child_visitor = visitor.enter_mut(self);
        match self {
            PointerKindDef::Stack(at) => at.visited_by_mut(&mut child_visitor),
            PointerKindDef::Static(amp) => amp.visited_by_mut(&mut child_visitor),
        };
        visitor.exit_mut(self, child_visitor)
    }
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

impl AstNode for TypeDefFn {
    extractors! {TypeDefFn}
    fn visited_by<'s, Visitor: AstVisitor<'s>>(&'s self, visitor: &mut Visitor) -> Visitor::Result {
        let mut child_visitor = visitor.enter(self);
        self.fn_kw.visited_by(&mut child_visitor);
        self.paren_open.visited_by(&mut child_visitor);
        for child in self.inputs.iter_all() {
            match child {
                Right(inp) => inp.visited_by(&mut child_visitor),
                Left(comma) => comma.visited_by(&mut child_visitor),
            };
        }
        self.paren_close.visited_by(&mut child_visitor);
        if let Some((arrow, out)) = &self.output {
            arrow.visited_by(&mut child_visitor);
            out.visited_by(&mut child_visitor);
        }
        visitor.exit(self, child_visitor)
    }

    fn visited_by_mut<Visitor: AstVisitorMut>(&mut self, visitor: &mut Visitor) -> Visitor::Result {
        let mut child_visitor = visitor.enter_mut(self);
        self.fn_kw.visited_by_mut(&mut child_visitor);
        self.paren_open.visited_by_mut(&mut child_visitor);
        for child in self.inputs.iter_all_mut() {
            match child {
                Right(inp) => inp.visited_by_mut(&mut child_visitor),
                Left(comma) => comma.visited_by_mut(&mut child_visitor),
            };
        }
        self.paren_close.visited_by_mut(&mut child_visitor);
        if let Some((arrow, out)) = &mut self.output {
            arrow.visited_by_mut(&mut child_visitor);
            out.visited_by_mut(&mut child_visitor);
        }
        visitor.exit_mut(self, child_visitor)
    }
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

impl AstNode for TypeDefUnknow {
    extractors! {TypeDefUnknow}
    fn visited_by<'s, Visitor: AstVisitor<'s>>(&'s self, visitor: &mut Visitor) -> Visitor::Result {
        let mut child_visitor = visitor.enter(self);
        self.underscore.visited_by(&mut child_visitor);
        visitor.exit(self, child_visitor)
    }

    fn visited_by_mut<Visitor: AstVisitorMut>(&mut self, visitor: &mut Visitor) -> Visitor::Result {
        let mut child_visitor = visitor.enter_mut(self);
        self.underscore.visited_by_mut(&mut child_visitor);
        visitor.exit_mut(self, child_visitor)
    }
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
