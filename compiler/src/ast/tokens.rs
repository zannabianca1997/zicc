use std::hash::Hash;

use string_interner::symbol::DefaultSymbol;

use crate::span::{Pos, Span, Spanned};

macro_rules! keywords {
    ( $($ident:ident $str:literal ;)*) => {
        ::paste::paste!{
            #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
            pub enum Keyword {
                $(
                    $ident([<Keyword $ident>]),
                )*
            }
            impl Keyword {
                pub fn as_ident(&self)->Identifier {
                    todo!()
                }
            }

            $(
                #[derive(Debug, Clone, Copy)]
                pub struct [<Keyword $ident>] (Pos);
                impl [<Keyword $ident>] {
                    pub fn new()->Self {
                        Self(Pos::missing())
                    }
                    pub fn as_ident(&self)->Identifier {
                        todo!()
                    }
                }
                impl PartialEq for [<Keyword $ident>] {
                    fn eq(&self, _: &Self) -> bool {
                        true
                    }
                }
                impl Eq for [<Keyword $ident>] {}
                impl PartialOrd for [<Keyword $ident>] {
                    fn partial_cmp(&self, _: &Self) -> Option<std::cmp::Ordering> {
                        Some(std::cmp::Ordering::Equal)
                    }
                }
                impl Ord for [<Keyword $ident>] {
                    fn cmp(&self, _: &Self) -> std::cmp::Ordering {
                        std::cmp::Ordering::Equal
                    }
                }
                impl Hash for [<Keyword $ident>] {
                    fn hash<H: std::hash::Hasher>(&self, _: &mut H) {}
                }
            )*
        }
    };
}

keywords! {
    Type   "type";
    Struct "struct";
    Union  "union";
    Fn     "fn";
    If     "if";
    For    "for";
    While  "while";
    Loop   "loop";
    Pub    "pub";
    Static "static";
    Extern "extern";
    Int    "int";
    Let    "let";
    Else   "else";
}

#[derive(Debug, Clone, Copy)]
pub struct Identifier {
    symbol: DefaultSymbol,
    span: Span,
}
impl PartialEq for Identifier {
    fn eq(&self, other: &Self) -> bool {
        self.symbol == other.symbol
    }
}
impl Eq for Identifier {}
impl PartialOrd for Identifier {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.symbol.partial_cmp(&other.symbol)
    }
}
impl Ord for Identifier {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.symbol.cmp(&other.symbol)
    }
}
impl Hash for Identifier {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.symbol.hash(state);
    }
}
impl Spanned for Identifier {
    fn span(&self) -> Span {
        self.span
    }
}

macro_rules! punctuators {
    ( $($ident:ident $str:literal ;)*) => {
        ::paste::paste!{
            #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
            pub enum Punct {
                $(
                    $ident([<Punct $ident>]),
                )*
            }

            $(
                #[derive(Debug, Clone, Copy)]
                pub struct [<Punct $ident>] (Pos);
                impl PartialEq for [<Punct $ident>] {
                    fn eq(&self, _: &Self) -> bool {
                        true
                    }
                }
                impl Eq for [<Punct $ident>] {}
                impl PartialOrd for [<Punct $ident>] {
                    fn partial_cmp(&self, _: &Self) -> Option<std::cmp::Ordering> {
                        Some(std::cmp::Ordering::Equal)
                    }
                }
                impl Ord for [<Punct $ident>] {
                    fn cmp(&self, _: &Self) -> std::cmp::Ordering {
                        std::cmp::Ordering::Equal
                    }
                }
                impl Hash for [<Punct $ident>] {
                    fn hash<H: std::hash::Hasher>(&self, _: &mut H) {}
                }
            )*
        }
    };
}

punctuators! {
    Colon   ":";
    Eq      "=";
    Semi    ";";
    Comma   ",";
    Dot     ".";
    Plus    "+";
    Minus   "-";
    Star    "*";
    EqEq    "==";
    NoEq    "!=";
    Lt      "<";
    Gt      ">";
    Le      "<=";
    Ge      ">=";
    BraceOpen  "{";
    BraceClose "}";
    BracketOpen  "[";
    BracketClose "]";
    ParenOpen  "(";
    ParenClose ")";
    Ampersand  "&";
    At         "@";
    Underscore "_";
    RightArrow "->";
}
