#![feature(box_into_inner)]
#![feature(iter_intersperse)]
#![feature(box_patterns)]

use std::{cell::RefCell, fmt::Debug, rc::Rc};

use ast_node::{AstNode, AstVisitor, AstVisitorMut};
use display_context::DisplayWithContext;
use either::Either::{Left, Right};
use errors::{Accumulator, Multiple, RootAccumulator};
use logos::Logos;
use string_interner::DefaultStringInterner;
use thiserror::Error;
use typedef::{TypeDefStruct, TypeDefUnion};

use self::tokens::*;

pub mod tokens;
pub mod ast_node {

    use display_context::DisplayWithContext;
    use string_interner::DefaultStringInterner;

    use crate::{expression::*, statements::*, typedef::*, *};

    macro_rules! extractors_default {
        ($($node_type:ident)*) => {
            ::paste::paste! {
                $(
                    fn [< is_ $node_type:snake >](&self) -> bool {
                        false
                    }
                    fn [< as_ $node_type:snake >](&self) -> Option<& $node_type> {
                        None
                    }
                    fn [< as_ $node_type:snake _mut >](&mut self) -> Option<&mut  $node_type> {
                        None
                    }
                )*
            }
        };
    }

    #[macro_export]
    macro_rules! extractors {
        ($node_type:ident) => {
            ::paste::paste! {
                fn [< is_ $node_type:snake >](&self) -> bool {
                    true
                }
                fn [< as_ $node_type:snake >](&self) -> Option<& $node_type> {
                    Some(self)
                }
                fn [< as_ $node_type:snake _mut >](&mut self) -> Option<&mut  $node_type> {
                    Some(self)
                }
            }
        };
    }

    pub trait AstNode: DisplayWithContext<DefaultStringInterner> {
        fn visited_by<'s, Visitor: AstVisitor<'s>>(
            &'s self,
            visitor: &mut Visitor,
        ) -> Visitor::Result;
        fn visited_by_mut<Visitor: AstVisitorMut>(
            &mut self,
            visitor: &mut Visitor,
        ) -> Visitor::Result;

        // extractor to extract various ast nodes

        extractors_default! {
            File Item

            ItemExtern ItemStatic

            ItemFn Signature
            Statement StatementBlock StatementExpr StatementIf StatementLet StatementLoop StatementWhile StatementReturn

            ItemType
            TypeDef TypeDefFn TypeDefUnknow
            TypeDefData TypeDefArray TypeDefInt TypeDefPointer PointerKindDef TypeDefStruct TypeDefUnion
            Expression ExpressionCall ExpressionSizeOf Parenthesized MemberAccess IndexAccess

            Literal Identifier
        }
    }

    /// Visit the AST recursively
    // The lifetime of the AST is parametrized so the visitor can store references to it
    pub trait AstVisitor<'a> {
        type ChildVisitor: AstVisitor<'a>;
        type Result;
        /// Visit a node, and return the visitor that will visit the childs
        fn enter(&mut self, node: &'a impl AstNode) -> Self::ChildVisitor;
        /// Ended the child visit, exit the visitor
        fn exit(
            &mut self,
            node: &'a impl AstNode,
            child_visitor: Self::ChildVisitor,
        ) -> Self::Result;
    }

    /// Visit the AST recursively, with mutable access
    // The lifetime of the ast cannot be parametrized, as we cannot save mutable references
    pub trait AstVisitorMut {
        type ChildVisitor: AstVisitorMut;
        type Result;
        /// Visit a mutable node, and return the visitor that will visit the childs
        fn enter_mut(&mut self, node: &mut impl AstNode) -> Self::ChildVisitor;
        /// Ended the child visit, exit the visitor
        fn exit_mut(
            &mut self,
            node: &mut impl AstNode,
            child_visitor: Self::ChildVisitor,
        ) -> Self::Result;
    }

    #[derive(Debug, Clone, Copy)]
    /// A visitor that do nothing
    pub struct NullVisitor;

    impl<'a> AstVisitor<'a> for NullVisitor {
        type ChildVisitor = NullVisitor;

        type Result = ();

        fn enter(&mut self, _: &'a impl AstNode) -> Self::ChildVisitor {
            Self
        }

        fn exit(&mut self, _: &'a impl AstNode, _: Self::ChildVisitor) -> Self::Result {
            ()
        }
    }

    impl AstVisitorMut for NullVisitor {
        type ChildVisitor = NullVisitor;

        type Result = ();

        fn enter_mut(&mut self, _: &mut impl AstNode) -> Self::ChildVisitor {
            Self
        }

        fn exit_mut(&mut self, _: &mut impl AstNode, _: Self::ChildVisitor) -> Self::Result {
            ()
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct File {
    pub items: Vec<Item>,
}

impl AstNode for File {
    extractors! {File}
    fn visited_by<'s, Visitor: AstVisitor<'s>>(&'s self, visitor: &mut Visitor) -> Visitor::Result {
        let mut child_visitor = visitor.enter(self);
        for item in &self.items {
            item.visited_by(&mut child_visitor);
        }
        visitor.exit(self, child_visitor)
    }

    fn visited_by_mut<Visitor: AstVisitorMut>(&mut self, visitor: &mut Visitor) -> Visitor::Result {
        let mut child_visitor = visitor.enter_mut(self);
        for item in &mut self.items {
            item.visited_by_mut(&mut child_visitor);
        }
        visitor.exit_mut(self, child_visitor)
    }
}

impl DisplayWithContext<DefaultStringInterner> for File {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        context: &DefaultStringInterner,
    ) -> std::fmt::Result {
        for item in self.items.iter().map(Left).intersperse(Right("\n\n")) {
            DisplayWithContext::fmt(&item, f, context)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Item {
    /// Declaration of a global variable
    Static(ItemStatic),
    /// Declaration of a external identifier
    Extern(ItemExtern),
    /// Declaration of a type alias
    Type(ItemType),
    /// Declaration of a named struct
    Struct(TypeDefStruct),
    /// Declaration of a named union
    Union(TypeDefUnion),
    /// Declaration of a function
    Fn(ItemFn),
}

impl AstNode for Item {
    extractors! {Item}
    fn visited_by<'s, Visitor: AstVisitor<'s>>(&'s self, visitor: &mut Visitor) -> Visitor::Result {
        let mut child_visitor = visitor.enter(self);
        match self {
            Item::Static(child) => child.visited_by(&mut child_visitor),
            Item::Extern(child) => child.visited_by(&mut child_visitor),
            Item::Type(child) => child.visited_by(&mut child_visitor),
            Item::Fn(child) => child.visited_by(&mut child_visitor),
            Item::Struct(child) => child.visited_by(&mut child_visitor),
            Item::Union(child) => child.visited_by(&mut child_visitor),
        };
        visitor.exit(self, child_visitor)
    }

    fn visited_by_mut<Visitor: AstVisitorMut>(&mut self, visitor: &mut Visitor) -> Visitor::Result {
        let mut child_visitor = visitor.enter_mut(self);
        match self {
            Item::Static(child) => child.visited_by_mut(&mut child_visitor),
            Item::Extern(child) => child.visited_by_mut(&mut child_visitor),
            Item::Type(child) => child.visited_by_mut(&mut child_visitor),
            Item::Fn(child) => child.visited_by_mut(&mut child_visitor),
            Item::Struct(child) => child.visited_by_mut(&mut child_visitor),
            Item::Union(child) => child.visited_by_mut(&mut child_visitor),
        };
        visitor.exit_mut(self, child_visitor)
    }
}

impl DisplayWithContext<DefaultStringInterner> for Item {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        context: &DefaultStringInterner,
    ) -> std::fmt::Result {
        match self {
            Item::Static(item) => DisplayWithContext::fmt(item, f, context),
            Item::Extern(item) => DisplayWithContext::fmt(item, f, context),
            Item::Type(item) => DisplayWithContext::fmt(item, f, context),
            Item::Fn(item) => DisplayWithContext::fmt(item, f, context),
            Item::Struct(item) => DisplayWithContext::fmt(item, f, context),
            Item::Union(item) => DisplayWithContext::fmt(item, f, context),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ItemStatic {
    pub static_kw: KeywordStatic,
    pub ident: Identifier,
    pub colon: PunctColon,
    pub ty: typedef::TypeDefData,
    pub init: Option<(PunctEq, expression::Expression)>,
    pub semi: PunctSemi,
}

impl AstNode for ItemStatic {
    extractors! {ItemStatic}
    fn visited_by<'s, Visitor: AstVisitor<'s>>(&'s self, visitor: &mut Visitor) -> Visitor::Result {
        let mut child_visitor = visitor.enter(self);
        self.static_kw.visited_by(&mut child_visitor);
        self.ident.visited_by(&mut child_visitor);
        self.colon.visited_by(&mut child_visitor);
        self.ty.visited_by(&mut child_visitor);
        if let Some((eq, init)) = &self.init {
            eq.visited_by(&mut child_visitor);
            init.visited_by(&mut child_visitor);
        }
        self.semi.visited_by(&mut child_visitor);
        visitor.exit(self, child_visitor)
    }

    fn visited_by_mut<Visitor: AstVisitorMut>(&mut self, visitor: &mut Visitor) -> Visitor::Result {
        let mut child_visitor = visitor.enter_mut(self);
        self.static_kw.visited_by_mut(&mut child_visitor);
        self.ident.visited_by_mut(&mut child_visitor);
        self.colon.visited_by_mut(&mut child_visitor);
        self.ty.visited_by_mut(&mut child_visitor);
        if let Some((eq, init)) = &mut self.init {
            eq.visited_by_mut(&mut child_visitor);
            init.visited_by_mut(&mut child_visitor);
        }
        self.semi.visited_by_mut(&mut child_visitor);
        visitor.exit_mut(self, child_visitor)
    }
}

impl DisplayWithContext<DefaultStringInterner> for ItemStatic {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        context: &DefaultStringInterner,
    ) -> std::fmt::Result {
        DisplayWithContext::fmt(&self.static_kw, f, context)?;
        f.write_str(" ")?;
        DisplayWithContext::fmt(&self.ident, f, context)?;
        DisplayWithContext::fmt(&self.colon, f, context)?;
        f.write_str(" ")?;
        DisplayWithContext::fmt(&self.ty, f, context)?;
        if let Some((eq, expr)) = &self.init {
            f.write_str(" ")?;
            DisplayWithContext::fmt(eq, f, context)?;
            f.write_str(" ")?;
            DisplayWithContext::fmt(expr, f, context)?;
        }
        DisplayWithContext::fmt(&self.semi, f, context)?;
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ItemExtern {
    pub extern_kw: KeywordExtern,
    pub ident: Identifier,
    pub colon: PunctColon,
    pub ty: typedef::TypeDef,
    pub semi: PunctSemi,
}

impl AstNode for ItemExtern {
    extractors! {ItemExtern}
    fn visited_by<'s, Visitor: AstVisitor<'s>>(&'s self, visitor: &mut Visitor) -> Visitor::Result {
        let mut child_visitor = visitor.enter(self);
        self.extern_kw.visited_by(&mut child_visitor);
        self.ident.visited_by(&mut child_visitor);
        self.colon.visited_by(&mut child_visitor);
        self.ty.visited_by(&mut child_visitor);
        self.semi.visited_by(&mut child_visitor);
        visitor.exit(self, child_visitor)
    }

    fn visited_by_mut<Visitor: AstVisitorMut>(&mut self, visitor: &mut Visitor) -> Visitor::Result {
        let mut child_visitor = visitor.enter_mut(self);
        self.extern_kw.visited_by_mut(&mut child_visitor);
        self.ident.visited_by_mut(&mut child_visitor);
        self.colon.visited_by_mut(&mut child_visitor);
        self.ty.visited_by_mut(&mut child_visitor);
        self.semi.visited_by_mut(&mut child_visitor);
        visitor.exit_mut(self, child_visitor)
    }
}

impl DisplayWithContext<DefaultStringInterner> for ItemExtern {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        context: &DefaultStringInterner,
    ) -> std::fmt::Result {
        DisplayWithContext::fmt(&self.extern_kw, f, context)?;
        f.write_str(" ")?;
        DisplayWithContext::fmt(&self.ident, f, context)?;
        DisplayWithContext::fmt(&self.colon, f, context)?;
        f.write_str(" ")?;
        DisplayWithContext::fmt(&self.ty, f, context)?;
        DisplayWithContext::fmt(&self.semi, f, context)?;
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ItemType {
    pub type_kw: KeywordType,
    pub ident: Identifier,
    pub eq: PunctEq,
    pub ty: typedef::TypeDef,
    pub semi: PunctSemi,
}

impl AstNode for ItemType {
    extractors! {ItemType}
    fn visited_by<'s, Visitor: AstVisitor<'s>>(&'s self, visitor: &mut Visitor) -> Visitor::Result {
        let mut child_visitor = visitor.enter(self);
        self.type_kw.visited_by(&mut child_visitor);
        self.ident.visited_by(&mut child_visitor);
        self.eq.visited_by(&mut child_visitor);
        self.ty.visited_by(&mut child_visitor);
        self.semi.visited_by(&mut child_visitor);
        visitor.exit(self, child_visitor)
    }

    fn visited_by_mut<Visitor: AstVisitorMut>(&mut self, visitor: &mut Visitor) -> Visitor::Result {
        let mut child_visitor = visitor.enter_mut(self);
        self.type_kw.visited_by_mut(&mut child_visitor);
        self.ident.visited_by_mut(&mut child_visitor);
        self.eq.visited_by_mut(&mut child_visitor);
        self.ty.visited_by_mut(&mut child_visitor);
        self.semi.visited_by_mut(&mut child_visitor);
        visitor.exit_mut(self, child_visitor)
    }
}

impl DisplayWithContext<DefaultStringInterner> for ItemType {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        context: &DefaultStringInterner,
    ) -> std::fmt::Result {
        DisplayWithContext::fmt(&self.type_kw, f, context)?;
        f.write_str(" ")?;
        DisplayWithContext::fmt(&self.ident, f, context)?;
        f.write_str(" ")?;
        DisplayWithContext::fmt(&self.eq, f, context)?;
        f.write_str(" ")?;
        DisplayWithContext::fmt(&self.ty, f, context)?;
        DisplayWithContext::fmt(&self.semi, f, context)?;
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ItemFn {
    pub sig: Signature,
    pub body: statements::StatementBlock,
}

impl AstNode for ItemFn {
    extractors! {ItemFn}
    fn visited_by<'s, Visitor: AstVisitor<'s>>(&'s self, visitor: &mut Visitor) -> Visitor::Result {
        let mut child_visitor = visitor.enter(self);
        self.sig.visited_by(&mut child_visitor);
        self.body.visited_by(&mut child_visitor);
        visitor.exit(self, child_visitor)
    }

    fn visited_by_mut<Visitor: AstVisitorMut>(&mut self, visitor: &mut Visitor) -> Visitor::Result {
        let mut child_visitor = visitor.enter_mut(self);
        self.sig.visited_by_mut(&mut child_visitor);
        self.body.visited_by_mut(&mut child_visitor);
        visitor.exit_mut(self, child_visitor)
    }
}

impl DisplayWithContext<DefaultStringInterner> for ItemFn {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        context: &DefaultStringInterner,
    ) -> std::fmt::Result {
        DisplayWithContext::fmt(&self.sig, f, context)?;
        f.write_str("\n")?;
        DisplayWithContext::fmt(&self.body, f, context)?;
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Signature {
    pub fn_kw: KeywordFn,
    pub ident: Identifier,
    pub paren_open: PunctParenOpen,
    pub inputs: punctuated::Punctuated<(Identifier, PunctColon, typedef::TypeDefData), PunctComma>,
    pub paren_close: PunctParenClose,
    pub output: Option<(PunctRightArrow, typedef::TypeDefData)>,
}

impl AstNode for Signature {
    extractors! {Signature}
    fn visited_by<'s, Visitor: AstVisitor<'s>>(&'s self, visitor: &mut Visitor) -> Visitor::Result {
        let mut child_visitor = visitor.enter(self);
        self.fn_kw.visited_by(&mut child_visitor);
        self.ident.visited_by(&mut child_visitor);
        self.paren_open.visited_by(&mut child_visitor);
        for child in self.inputs.iter_all() {
            match child {
                Left((ident, colon, ty)) => {
                    ident.visited_by(&mut child_visitor);
                    colon.visited_by(&mut child_visitor);
                    ty.visited_by(&mut child_visitor);
                }
                Right(comma) => {
                    comma.visited_by(&mut child_visitor);
                }
            }
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
        self.ident.visited_by_mut(&mut child_visitor);
        self.paren_open.visited_by_mut(&mut child_visitor);
        for child in self.inputs.iter_all_mut() {
            match child {
                Left((ident, colon, ty)) => {
                    ident.visited_by_mut(&mut child_visitor);
                    colon.visited_by_mut(&mut child_visitor);
                    ty.visited_by_mut(&mut child_visitor);
                }
                Right(comma) => {
                    comma.visited_by_mut(&mut child_visitor);
                }
            }
        }
        self.paren_close.visited_by_mut(&mut child_visitor);
        if let Some((arrow, out)) = &mut self.output {
            arrow.visited_by_mut(&mut child_visitor);
            out.visited_by_mut(&mut child_visitor);
        }
        visitor.exit_mut(self, child_visitor)
    }
}

impl DisplayWithContext<DefaultStringInterner> for Signature {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        context: &DefaultStringInterner,
    ) -> std::fmt::Result {
        DisplayWithContext::fmt(&self.fn_kw, f, context)?;
        f.write_str(" ")?;
        DisplayWithContext::fmt(&self.ident, f, context)?;
        DisplayWithContext::fmt(&self.paren_open, f, context)?;
        for comma_or_input in self.inputs.iter_all() {
            match comma_or_input {
                Left((name, colon, ty)) => {
                    DisplayWithContext::fmt(name, f, context)?;
                    DisplayWithContext::fmt(colon, f, context)?;
                    f.write_str(" ")?;
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

pub mod statements;

pub mod typedef;

pub mod punctuated {
    use std::iter::{Chain, Map};

    use either::Either::{self, Left, Right};

    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct Punctuated<T, P> {
        pub inner: Vec<(T, P)>,
        pub last: Option<Box<T>>,
    }
    impl<T, P> Punctuated<T, P> {
        pub fn iter(&self) -> impl Iterator<Item = &T> {
            self.inner
                .iter()
                .map(|(item, _)| item)
                .chain(self.last.as_ref().map(|x| Box::as_ref(x)))
        }
        pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut T> {
            self.inner
                .iter_mut()
                .map(|(item, _)| item)
                .chain(self.last.as_mut().map(|x| Box::as_mut(x)))
        }

        pub fn iter_all(&self) -> impl Iterator<Item = Either<&T, &P>> {
            self.inner
                .iter()
                .flat_map(|(t, p)| [Left(t), Right(p)])
                .chain(self.last.as_ref().map(|x| Left(Box::as_ref(x))))
        }

        pub fn iter_all_mut(&mut self) -> impl Iterator<Item = Either<&mut T, &mut P>> {
            self.inner
                .iter_mut()
                .flat_map(|(t, p)| [Left(t), Right(p)])
                .chain(self.last.as_mut().map(|x| Left(Box::as_mut(x))))
        }

        pub fn is_empty(&self) -> bool {
            self.inner.is_empty() && self.last.is_none()
        }

        pub fn len(&self) -> usize {
            self.inner.len() + if self.last.is_some() { 1 } else { 0 }
        }
    }

    impl<T, P> IntoIterator for Punctuated<T, P> {
        type Item = T;

        type IntoIter = Chain<
            Map<<Vec<(T, P)> as IntoIterator>::IntoIter, fn((T, P)) -> T>,
            Map<<Option<Box<T>> as IntoIterator>::IntoIter, fn(Box<T>) -> T>,
        >;

        fn into_iter(self) -> Self::IntoIter {
            self.inner
                .into_iter()
                .map((|(t, _)| t) as fn((T, P)) -> T)
                .chain(
                    self.last
                        .into_iter()
                        .map(Box::into_inner as fn(Box<T>) -> T),
                )
        }
    }
}

pub mod expression;

peg::parser! {
  pub grammar parser() for [Token] {
    // EXPRESSIONS

    rule expression() -> expression::Expression = precedence!{
        lhs:@ [Token::PunctEq(op)] rhs:(@) { expression::Expression::Set(Box::new(expression::BinExpr { lhs, op, rhs })) }
        --
        lhs:(@) [Token::PunctEqEq(op)] rhs:@ { expression::Expression::Eq(Box::new(expression::BinExpr { lhs, op, rhs })) }
        lhs:(@) [Token::PunctNoEq(op)] rhs:@ { expression::Expression::NoEq(Box::new(expression::BinExpr { lhs, op, rhs })) }
        --
        lhs:(@) [Token::PunctGe(op)] rhs:@ { expression::Expression::Ge(Box::new(expression::BinExpr { lhs, op, rhs })) }
        lhs:(@) [Token::PunctGt(op)] rhs:@ { expression::Expression::Gt(Box::new(expression::BinExpr { lhs, op, rhs })) }
        lhs:(@) [Token::PunctLe(op)] rhs:@ { expression::Expression::Le(Box::new(expression::BinExpr { lhs, op, rhs })) }
        lhs:(@) [Token::PunctLt(op)] rhs:@ { expression::Expression::Lt(Box::new(expression::BinExpr { lhs, op, rhs })) }
        --
        lhs:(@) [Token::PunctPlus(op)] rhs:@ { expression::Expression::Add(Box::new(expression::BinExpr { lhs, op, rhs })) }
        lhs:(@) [Token::PunctMinus(op)] rhs:@ { expression::Expression::Sub(Box::new(expression::BinExpr { lhs, op, rhs })) }
        --
        lhs:(@) [Token::PunctStar(op)] rhs:@ { expression::Expression::Mul(Box::new(expression::BinExpr { lhs, op, rhs })) }
        --
        [Token::PunctMinus(op)] rhs:(@) {expression::Expression::Neg(Box::new(expression::UnExpr { op, rhs }))}
        [Token::PunctAmpersand(op)] rhs:(@) {expression::Expression::TakeRef(Box::new(expression::UnExpr { op: Left(op), rhs }))}
        [Token::PunctAt(op)] rhs:(@) {expression::Expression::TakeRef(Box::new(expression::UnExpr { op: Right(op), rhs }))}
        [Token::PunctStar(op)] rhs:(@) {expression::Expression::Deref(Box::new(expression::UnExpr { op, rhs }))}
        --
        [Token::Literal(n)]       { expression::Expression::Literal(Box::new(n)) }
        [Token::Identifier(name)] { expression::Expression::Name(Box::new(name)) }
        [Token::PunctParenOpen(open_par)] inner: expression() [Token::PunctParenClose(close_par)] { expression::Expression::Parenthesized(Box::new(expression::Parenthesized{ open_par, inner, close_par }))}
        [Token::KeywordSizeOf(size_of_kw)] [Token::PunctParenOpen(open_par)] ty: typedefdata() [Token::PunctParenClose(close_par)] { expression::Expression::SizeOf(Box::new(expression::ExpressionSizeOf { size_of_kw, open_par, ty, close_par }))}
        fun: @ [Token::PunctParenOpen(open_par)] inputs: punctuated(<expression()>, <[Token::PunctComma(comma)] {comma}>) [Token::PunctParenClose(close_par)] { expression::Expression::Call(Box::new(expression::ExpressionCall { fun, open_par, inputs, close_par }))}
        lhs: @ [Token::PunctBracketOpen(braket_open)] index: expression() [Token::PunctBracketClose(braket_close)] { expression::Expression::IndexAccess(Box::new(expression::IndexAccess { lhs, braket_open, index, braket_close }))}
        lhs: @ [Token::PunctDot(dot)] [Token::Identifier(member)] { expression::Expression::MemberAccess(Box::new(expression::MemberAccess { lhs, dot, member })) }
      }
    //  =

    // PUNCTUATED STUFF
    rule punctuated<E,P>(element: rule<E>, punctuator: rule<P>) -> punctuated::Punctuated<E,P>
      = inner: (e:element() p:punctuator() {(e,p)})* last:element()? {
        punctuated::Punctuated { inner, last: last.map(Box::new) }
      }

    // TYPES

    rule typedef() -> typedef::TypeDef
      = def: typedefdata()   { typedef::TypeDef::Data(def)   }
      / def: typedefunknow() { typedef::TypeDef::Unknow(def) }
      / def: typedeffn()     { typedef::TypeDef::Fn(def)     }

    rule typedefunknow() -> typedef::TypeDefUnknow
      = [Token::PunctUnderscore(underscore)] {typedef::TypeDefUnknow{underscore}}

    rule typedeffn() -> typedef::TypeDefFn
      = [Token::KeywordFn(fn_kw)]
        [Token::PunctParenOpen(paren_open)]
        inputs: punctuated(<typedefdata()>,<[Token::PunctComma(comma)] {comma}>)
        [Token::PunctParenClose(paren_close)]
        output: (
            [Token::PunctRightArrow(arrow)]
            out: typedefdata()
            { (arrow, out) }
        )?
        {
            typedef::TypeDefFn { fn_kw, paren_open, inputs, paren_close, output }
        }

    rule typedefdata() -> typedef::TypeDefData
      = def: typedefint()     { typedef::TypeDefData::Int(def)     }
      / def: typedefarray()   { typedef::TypeDefData::Array(def)   }
      / def: typedefunion()   { typedef::TypeDefData::Union(def)   }
      / def: typedefstruct()  { typedef::TypeDefData::Struct(def)  }
      / def: typedefpointer() { typedef::TypeDefData::Pointer(def) }
      / [Token::Identifier(ident)] { typedef::TypeDefData::Named(ident) }

    rule typedefint() -> typedef::TypeDefInt
      = [Token::KeywordInt(int_kwd)] { typedef::TypeDefInt{ int_kwd }}
    rule typedefarray() -> typedef::TypeDefArray
      = [Token::PunctBracketOpen(bracket_open)]
        element: typedefdata()
        [Token::PunctSemi(semi)]
        lenght: expression()
        [Token::PunctBracketClose(bracket_close)]
      {
        typedef::TypeDefArray{ bracket_open, element: Box::new(element), semi, lenght, bracket_close }
      }

    rule compositefielddef() -> (either::Either<Identifier, PunctUnderscore>, PunctColon, typedef::TypeDefData)
      = name: (
                  [Token::Identifier(ident)] {either::Either::Left(ident)}
                  / [Token::PunctUnderscore(under)] {either::Either::Right(under)}
              )
              [Token::PunctColon(colon)]
              ty: typedefdata()
              { (name, colon, ty) }

    rule typedefstruct() -> typedef::TypeDefStruct
      = [Token::KeywordStruct(struct_kw)]
        name: ([Token::Identifier(name)] {name})?
        [Token::PunctBraceOpen(brace_open)]
        fields: punctuated(<compositefielddef()>,<[Token::PunctComma(comma)] {comma}>
        )
        [Token::PunctBraceClose(brace_close)]
      {
        typedef::TypeDefStruct { struct_kw, name, brace_open, fields, brace_close }
      }

    rule typedefunion() -> typedef::TypeDefUnion
      = [Token::KeywordUnion(union_kw)]
        name: ([Token::Identifier(name)] {name})?
        [Token::PunctBraceOpen(brace_open)]
        variants: punctuated(<compositefielddef()>,<[Token::PunctComma(comma)] {comma}>
        )
        [Token::PunctBraceClose(brace_close)]
      {
        typedef::TypeDefUnion { union_kw, name, brace_open, variants, brace_close }
      }

    rule typedefpointer() -> typedef::TypeDefPointer
      = kind:(
            [Token::PunctAmpersand(amp)] { typedef::PointerKindDef::Static(amp)}
            / [Token::PunctAt(at)] { typedef::PointerKindDef::Stack(at)}
        ) pointee: typedef()
        { typedef::TypeDefPointer { kind, pointee: Box::new(pointee) }}

    // FUNCTION SIGNATURE

    rule signature() -> Signature
      = [Token::KeywordFn(fn_kw)]
        [Token::Identifier(ident)]
        [Token::PunctParenOpen(paren_open)]
        inputs: punctuated(<
            [Token::Identifier(ident)]
            [Token::PunctColon(colon)]
            ty: typedefdata()
            {(ident, colon, ty)}
        >, <[Token::PunctComma(comma)] {comma}>)
        [Token::PunctParenClose(paren_close)]
        output: (
            [Token::PunctRightArrow(arrow)]
            ty: typedefdata()
            { (arrow, ty) }
        )?
        { Signature { fn_kw, ident, paren_open, inputs, paren_close, output }}


    // STATEMENTS

    rule statement() -> statements::Statement
      = stm: statementblock()  { statements::Statement::Block(stm)  }
      / stm: statementlet()    { statements::Statement::Let(stm)    }
      / stm: statementexpr()   { statements::Statement::Expr(stm)   }
      / stm: statementloop()   { statements::Statement::Loop(stm)   }
      / stm: statementif()     { statements::Statement::If(stm)     }
      / stm: statementwhile()  { statements::Statement::While(stm)  }
      / stm: statementreturn() { statements::Statement::Return(stm) }

    rule statementblock() -> statements::StatementBlock
      = [Token::PunctBraceOpen(brace_open)]
        statements: punctuated(<statement()>, <[Token::PunctSemi(semi)] {semi}>)
        [Token::PunctBraceClose(brace_close)]
        {statements::StatementBlock{ brace_open, statements, brace_close }}

    rule statementlet() -> statements::StatementLet
      = [Token::KeywordLet(let_kw)]
        [Token::Identifier(ident)]
        [Token::PunctColon(colon)]
        ty: typedefdata()
        initializer: (
            [Token::PunctEq(eq)]
            expr: expression()
            { (eq, expr) }
        )?
        { statements::StatementLet { let_kw, ident, colon, ty, initializer }}

    rule statementexpr() -> statements::StatementExpr
      = expr: expression()
        { statements::StatementExpr { expr }}

    rule statementloop() -> statements::StatementLoop
      = [Token::KeywordLoop(loop_kw)]
        body: statementblock()
        { statements::StatementLoop { loop_kw, body }}

    rule statementif() -> statements::StatementIf
      = [Token::KeywordIf(if_kw)]
        condition: expression()
        body: statementblock()
        else_branch: (
            [Token::KeywordElse(else_kw)]
            else_body: statementblock()
            {(else_kw, else_body)}
        )?
        { statements::StatementIf{ if_kw, condition, body, else_branch }}

    rule statementwhile() -> statements::StatementWhile
      = [Token::KeywordWhile(while_kw)]
        condition: expression()
        body: statementblock()
        { statements::StatementWhile{ while_kw, condition, body }}

    rule statementreturn() -> statements::StatementReturn
      = [Token::KeywordReturn(return_kw)]
        value: ( expression() )?
        { statements::StatementReturn{ return_kw, value }}

    // ITEMS

    rule itemstatic() -> ItemStatic
      = [Token::KeywordStatic(static_kw)]
        [Token::Identifier(ident)]
        [Token::PunctColon(colon)]
        ty: typedefdata()
        init: (
            [Token::PunctEq(eq)]
            value: expression()
            { (eq, value) }
        )?
        [Token::PunctSemi(semi)]
      {
        ItemStatic { static_kw, ident, colon, ty, init, semi }
      }

    rule itemextern() -> ItemExtern
      = [Token::KeywordExtern(extern_kw)]
        [Token::Identifier(ident)]
        [Token::PunctColon(colon)]
        ty: typedef()
        [Token::PunctSemi(semi)]
      {
          ItemExtern { extern_kw, ident, colon, ty, semi }
      }

    rule itemtype() -> ItemType
      = [Token::KeywordType(type_kw)]
        [Token::Identifier(ident)]
        [Token::PunctEq(eq)]
        ty: typedef()
        [Token::PunctSemi(semi)]
      {
          ItemType { type_kw, ident, eq, ty, semi }
      }

    rule itemfn() -> ItemFn
      = sig: signature() body: statementblock() { ItemFn { sig, body }}

    rule item() -> Item
      = item: itemstatic() { Item::Static(item)}
      / item: itemextern() { Item::Extern(item)}
      / item: itemtype()   { Item::Type(item) }
      / def: typedefstruct() { Item::Struct(def) }
      / def: typedefunion() { Item::Union(def) }
      / item: itemfn() { Item::Fn(item) }

    // FILE

    pub rule file() -> File
      = items:(item() *) ![_] { File { items } }
  }
}

#[derive(Debug, Error)]
pub enum ParseError {
    #[error(transparent)]
    Lex(#[from] tokens::LexError),
    #[error(transparent)]
    Parse(#[from] peg::error::ParseError<usize>),
}

pub fn parse(
    source: &str,
    source_name: Option<&str>,
    interner: Rc<RefCell<DefaultStringInterner>>,
) -> Result<File, Multiple<ParseError>> {
    let mut errors: RootAccumulator<ParseError> = RootAccumulator::new();
    let tokens: Box<[Token]> = errors
        .handle_iter(tokens::Token::lexer_with_extras(
            source,
            tokens::LexerExtras::new(source_name, interner),
        ))
        .flatten()
        .collect();
    // No use proceeding if some errors where found while tokenizing
    errors.checkpoint()?;

    let ast = errors.handle(parser::file(&tokens));

    errors.finish(|| ast.unwrap())
}
