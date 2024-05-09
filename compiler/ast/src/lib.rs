#![feature(box_into_inner)]

use std::fmt::Debug;

use self::tokens::*;

pub mod tokens;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct File {
    pub items: Vec<Item>,
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

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ItemStatic {
    pub static_kw: KeywordStatic,
    pub ident: Identifier,
    pub colon: PunctColon,
    pub ty: typedef::TypeDefData,
    pub init: Option<(PunctEq, expression::Expression)>,
    pub semi: PunctSemi,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ItemExtern {
    pub extern_kw: KeywordExtern,
    pub ident: Identifier,
    pub colon: PunctColon,
    pub ty: typedef::TypeDef,
    pub semi: PunctSemi,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ItemType {
    pub type_kw: KeywordType,
    pub ident: Identifier,
    pub eq: PunctEq,
    pub ty: typedef::TypeDef,
    pub semi: PunctSemi,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ItemFn {
    pub sig: Signature,
    pub body: statements::StatementBlock,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Signature {
    pub fn_kw: KeywordFn,
    pub ident: Identifier,
    pub paren_open: PunctParenOpen,
    pub input: punctuated::Punctuated<(Identifier, PunctColon, typedef::TypeDefData), PunctComma>,
    pub paren_close: PunctParenClose,
    pub output: Option<(PunctRightArrow, typedef::TypeDefData)>,
}

pub mod statements;

pub mod typedef;

pub mod punctuated {
    use std::iter::{Chain, Map};

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
    use super::tokens;

    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub enum Expression {
        Literal(tokens::Literal),
    }
}

peg::parser! {
  pub grammar parser() for [Token] {
    // EXPRESSIONS

    rule expression() -> expression::Expression
      = [Token::Literal(n)] {expression::Expression::Literal(n)}

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
