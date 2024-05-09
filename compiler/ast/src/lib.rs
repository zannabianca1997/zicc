#![feature(box_into_inner)]
#![feature(iter_intersperse)]

use std::fmt::Debug;

use display_context::DisplayWithContext;
use either::Either::{Left, Right};
use string_interner::DefaultStringInterner;

use self::tokens::*;

pub mod tokens;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct File {
    pub items: Vec<Item>,
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
    /// Declaration of a function
    Fn(ItemFn),
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

        pub fn iter_all(&self) -> impl Iterator<Item = Either<&T, &P>> {
            self.inner
                .iter()
                .flat_map(|(t, p)| [Left(t), Right(p)])
                .chain(self.last.as_ref().map(|x| Left(Box::as_ref(x))))
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

pub mod expression {
    use display_context::DisplayWithContext;
    use either::Either;
    use string_interner::DefaultStringInterner;

    use crate::{
        punctuated::Punctuated, Identifier, PunctAmpersand, PunctAt, PunctBracketClose,
        PunctBracketOpen, PunctComma, PunctDot, PunctEq, PunctEqEq, PunctGe, PunctGt, PunctLe,
        PunctLt, PunctNoEq, PunctParenClose, PunctParenOpen,
    };

    use super::{Literal, PunctMinus, PunctPlus, PunctStar};

    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub enum Expression {
        Literal(Box<Literal>),
        Name(Box<Identifier>),
        Set(Box<BinExpr<PunctEq>>),
        Eq(Box<BinExpr<PunctEqEq>>),
        NoEq(Box<BinExpr<PunctNoEq>>),
        Ge(Box<BinExpr<PunctGe>>),
        Gt(Box<BinExpr<PunctGt>>),
        Le(Box<BinExpr<PunctLe>>),
        Lt(Box<BinExpr<PunctLt>>),
        MemberAccess(Box<MemberAccess>),
        IndexAccess(Box<IndexAccess>),
        Deref(Box<UnExpr<PunctStar>>),
        TakeRef(Box<UnExpr<Either<PunctAmpersand, PunctAt>>>),
        Call(Box<ExpressionCall>),
        Add(Box<BinExpr<PunctPlus>>),
        Sub(Box<BinExpr<PunctMinus>>),
        Neg(Box<UnExpr<PunctMinus>>),
        Mul(Box<BinExpr<PunctStar>>),
        Parenthesized(Box<Parenthesized>),
    }

    impl DisplayWithContext<DefaultStringInterner> for Expression {
        fn fmt(
            &self,
            f: &mut std::fmt::Formatter<'_>,
            context: &DefaultStringInterner,
        ) -> std::fmt::Result {
            match self {
                Expression::Literal(expr) => DisplayWithContext::fmt(expr, f, context),
                Expression::Name(expr) => DisplayWithContext::fmt(expr, f, context),
                Expression::Set(expr) => DisplayWithContext::fmt(expr, f, context),
                Expression::Eq(expr) => DisplayWithContext::fmt(expr, f, context),
                Expression::NoEq(expr) => DisplayWithContext::fmt(expr, f, context),
                Expression::Ge(expr) => DisplayWithContext::fmt(expr, f, context),
                Expression::Gt(expr) => DisplayWithContext::fmt(expr, f, context),
                Expression::Le(expr) => DisplayWithContext::fmt(expr, f, context),
                Expression::Lt(expr) => DisplayWithContext::fmt(expr, f, context),
                Expression::MemberAccess(expr) => DisplayWithContext::fmt(expr, f, context),
                Expression::IndexAccess(expr) => DisplayWithContext::fmt(expr, f, context),
                Expression::Deref(expr) => DisplayWithContext::fmt(expr, f, context),
                Expression::TakeRef(expr) => DisplayWithContext::fmt(expr, f, context),
                Expression::Call(expr) => DisplayWithContext::fmt(expr, f, context),
                Expression::Add(expr) => DisplayWithContext::fmt(expr, f, context),
                Expression::Sub(expr) => DisplayWithContext::fmt(expr, f, context),
                Expression::Neg(expr) => DisplayWithContext::fmt(expr, f, context),
                Expression::Mul(expr) => DisplayWithContext::fmt(expr, f, context),
                Expression::Parenthesized(expr) => DisplayWithContext::fmt(expr, f, context),
            }
        }
    }

    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct MemberAccess {
        pub lhs: Expression,
        pub dot: PunctDot,
        pub member: Identifier,
    }
    impl DisplayWithContext<DefaultStringInterner> for MemberAccess {
        fn fmt(
            &self,
            f: &mut std::fmt::Formatter<'_>,
            context: &DefaultStringInterner,
        ) -> std::fmt::Result {
            DisplayWithContext::fmt(&self.lhs, f, context)?;
            DisplayWithContext::fmt(&self.dot, f, context)?;
            DisplayWithContext::fmt(&self.member, f, context)?;
            Ok(())
        }
    }

    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct ExpressionCall {
        pub fun: Identifier,
        pub open_par: PunctParenOpen,
        pub inputs: Punctuated<Expression, PunctComma>,
        pub close_par: PunctParenClose,
    }
    impl DisplayWithContext<DefaultStringInterner> for ExpressionCall {
        fn fmt(
            &self,
            f: &mut std::fmt::Formatter<'_>,
            context: &DefaultStringInterner,
        ) -> std::fmt::Result {
            DisplayWithContext::fmt(&self.fun, f, context)?;
            DisplayWithContext::fmt(&self.open_par, f, context)?;
            for input_or_comma in self.inputs.iter_all() {
                match input_or_comma {
                    Either::Left(inp) => DisplayWithContext::fmt(inp, f, context)?,
                    Either::Right(comma) => {
                        DisplayWithContext::fmt(comma, f, context)?;
                        f.write_str(" ")?;
                    }
                }
            }
            DisplayWithContext::fmt(&self.close_par, f, context)?;
            Ok(())
        }
    }

    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct IndexAccess {
        pub lhs: Expression,
        pub braket_open: PunctBracketOpen,
        pub index: Identifier,
        pub braket_close: PunctBracketClose,
    }
    impl DisplayWithContext<DefaultStringInterner> for IndexAccess {
        fn fmt(
            &self,
            f: &mut std::fmt::Formatter<'_>,
            context: &DefaultStringInterner,
        ) -> std::fmt::Result {
            DisplayWithContext::fmt(&self.lhs, f, context)?;
            DisplayWithContext::fmt(&self.braket_open, f, context)?;
            DisplayWithContext::fmt(&self.index, f, context)?;
            DisplayWithContext::fmt(&self.braket_close, f, context)?;
            Ok(())
        }
    }

    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct BinExpr<Op> {
        pub lhs: Expression,
        pub op: Op,
        pub rhs: Expression,
    }
    impl<Op> DisplayWithContext<DefaultStringInterner> for BinExpr<Op>
    where
        Op: DisplayWithContext<DefaultStringInterner>,
    {
        fn fmt(
            &self,
            f: &mut std::fmt::Formatter<'_>,
            context: &DefaultStringInterner,
        ) -> std::fmt::Result {
            DisplayWithContext::fmt(&self.lhs, f, context)?;
            DisplayWithContext::fmt(&self.op, f, context)?;
            DisplayWithContext::fmt(&self.rhs, f, context)?;
            Ok(())
        }
    }

    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct UnExpr<Op> {
        pub op: Op,
        pub rhs: Expression,
    }
    impl<Op> DisplayWithContext<DefaultStringInterner> for UnExpr<Op>
    where
        Op: DisplayWithContext<DefaultStringInterner>,
    {
        fn fmt(
            &self,
            f: &mut std::fmt::Formatter<'_>,
            context: &DefaultStringInterner,
        ) -> std::fmt::Result {
            DisplayWithContext::fmt(&self.op, f, context)?;
            DisplayWithContext::fmt(&self.rhs, f, context)?;
            Ok(())
        }
    }

    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct Parenthesized {
        pub open_par: PunctParenOpen,
        pub inner: Expression,
        pub close_par: PunctParenClose,
    }

    impl DisplayWithContext<DefaultStringInterner> for Parenthesized {
        fn fmt(
            &self,
            f: &mut std::fmt::Formatter<'_>,
            context: &DefaultStringInterner,
        ) -> std::fmt::Result {
            DisplayWithContext::fmt(&self.open_par, f, context)?;
            DisplayWithContext::fmt(&self.inner, f, context)?;
            DisplayWithContext::fmt(&self.close_par, f, context)?;
            Ok(())
        }
    }
}

peg::parser! {
  pub grammar parser() for [Token] {
    // EXPRESSIONS

    rule expression() -> expression::Expression
      = [Token::Literal(n)] {expression::Expression::Literal(Box::new(n))}

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
      = [Token::Punct(Punct::Underscore(underscore))] {typedef::TypeDefUnknow{underscore}}

    rule typedeffn() -> typedef::TypeDefFn
      = [Token::Keyword(Keyword::Fn(fn_kw))]
        [Token::Punct(Punct::ParenOpen(paren_open))]
        inputs: punctuated(<typedefdata()>,<[Token::Punct(Punct::Comma(comma))] {comma}>)
        [Token::Punct(Punct::ParenClose(paren_close))]
        output: (
            [Token::Punct(Punct::RightArrow(arrow))]
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
      = [Token::Keyword(Keyword::Int(int_kwd))] { typedef::TypeDefInt{ int_kwd }}
    rule typedefarray() -> typedef::TypeDefArray
      = [Token::Punct(Punct::BracketOpen(bracket_open))]
        element: typedefdata()
        [Token::Punct(Punct::Semi(semi))]
        lenght: expression()
        [Token::Punct(Punct::BracketClose(bracket_close))]
      {
        typedef::TypeDefArray{ bracket_open, element: Box::new(element), semi, lenght, bracket_close }
      }

    rule compositefielddef() -> (either::Either<Identifier, PunctUnderscore>, PunctColon, typedef::TypeDefData)
      = name: (
                  [Token::Identifier(ident)] {either::Either::Left(ident)}
                  / [Token::Punct(Punct::Underscore(under))] {either::Either::Right(under)}
              )
              [Token::Punct(Punct::Colon(colon))]
              ty: typedefdata()
              { (name, colon, ty) }

    rule typedefstruct() -> typedef::TypeDefStruct
      = [Token::Keyword(Keyword::Struct(struct_kw))]
        name: ([Token::Identifier(name)] {name})?
        [Token::Punct(Punct::BraceOpen(brace_open))]
        fields: punctuated(<compositefielddef()>,<[Token::Punct(Punct::Comma(comma))] {comma}>
        )
        [Token::Punct(Punct::BraceClose(brace_close))]
      {
        typedef::TypeDefStruct { struct_kw, name, brace_open, fields, brace_close }
      }

    rule typedefunion() -> typedef::TypeDefUnion
      = [Token::Keyword(Keyword::Union(union_kw))]
        name: ([Token::Identifier(name)] {name})?
        [Token::Punct(Punct::BraceOpen(brace_open))]
        variants: punctuated(<compositefielddef()>,<[Token::Punct(Punct::Comma(comma))] {comma}>
        )
        [Token::Punct(Punct::BraceClose(brace_close))]
      {
        typedef::TypeDefUnion { union_kw, name, brace_open, variants, brace_close }
      }

    rule typedefpointer() -> typedef::TypeDefPointer
      = kind:(
            [Token::Punct(Punct::Ampersand(amp))] { typedef::PointerKindDef::Static(amp)}
            / [Token::Punct(Punct::At(at))] { typedef::PointerKindDef::Stack(at)}
        ) pointee: typedef()
        { typedef::TypeDefPointer { kind, pointee: Box::new(pointee) }}


    // ITEMS

    rule itemstatic() -> ItemStatic
      = [Token::Keyword(Keyword::Static(static_kw))]
        [Token::Identifier(ident)]
        [Token::Punct(Punct::Colon(colon))]
        ty: typedefdata()
        init: (
            [Token::Punct(Punct::Eq(eq))]
            value: expression()
            { (eq, value) }
        )?
        [Token::Punct(Punct::Semi(semi))]
      {
        ItemStatic { static_kw, ident, colon, ty, init, semi }
      }

    rule itemextern() -> ItemExtern
      = [Token::Keyword(Keyword::Extern(extern_kw))]
        [Token::Identifier(ident)]
        [Token::Punct(Punct::Colon(colon))]
        ty: typedef()
        [Token::Punct(Punct::Semi(semi))]
      {
          ItemExtern { extern_kw, ident, colon, ty, semi }
      }

    rule itemtype() -> ItemType
      = [Token::Keyword(Keyword::Type(type_kw))]
        [Token::Identifier(ident)]
        [Token::Punct(Punct::Eq(eq))]
        ty: typedef()
        [Token::Punct(Punct::Semi(semi))]
      {
          ItemType { type_kw, ident, eq, ty, semi }
      }

    rule item() -> Item
      = item: itemstatic() { Item::Static(item)}
      / item: itemextern() { Item::Extern(item)}
      / item: itemtype()   { Item::Type(item)}

    // FILE

    pub rule file() -> File
      = items:(item() *) ![_] { File { items } }
  }
}
