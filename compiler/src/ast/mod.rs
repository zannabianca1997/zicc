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
        inner: Vec<(T, P)>,
        last: Option<Box<T>>,
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
    use crate::span::Spanned;

    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub enum Expression {}

    impl Spanned for Expression {
        fn span(&self) -> crate::span::Span {
            todo!()
        }
    }
}
