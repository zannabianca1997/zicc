//! # Keywords
//!
//! Keywords definitions

use crate::display::Displayable;
use std::fmt::Display;

use paste::paste;

use crate::Token;

macro_rules! keywords {
    (
        [$d:tt]
        $(
            $name:ident ( $value:literal )
        ),* $(,)?
    ) => {
        paste! {
            /// A keyword
            #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
            pub enum Keyword {
                $(
                    #[doc="The `" $value "` keyword"]
                    $name($name)
                ),*
            }

            impl Keyword {
                $(
                   #[doc="The `" $value "` keyword"]
                   pub const fn [< k_ $name:snake:lower >] () -> Self { Self::$name($name) }
                )*

                /// The keyword as a string
                pub const fn as_str(&self) -> &'static str {
                    match self {
                        $(Self::$name(_) => $value),*
                    }
                }

                /// Try to parse the string as a keyword
                pub fn from_str(value: &str) -> Option<Self> {
                    match value {
                        $($value => Some( Self :: [< k_ $name:snake:lower >] () ), )*
                        _ => None
                    }
                }
            }

            impl TryFrom<Token> for Keyword {
                type Error = Token;

                fn try_from(value: Token) -> Result<Self, Token> {
                    if let Token::Keyword(value) = value {
                        Ok(value)
                    } else {
                        Err(value)
                    }
                }
            }

            impl Display for Keyword {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    write!(f, "{}", self.as_str())
                }
            }
            impl Displayable for Keyword {}

            $(
                #[doc="The `" $value "` keyword"]
                #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
                pub struct $name;

                impl $name {
                    #[doc="The `" $value "` constant"]
                    pub const fn as_str(&self) -> &'static str {
                        $value
                    }
                }

                impl Display for $name {
                    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                        write!(f, "{}", $value)
                    }
                }
                impl Displayable for $name {}

                impl From<$name> for Keyword {
                    fn from(value: $name) -> Self {
                        Self::$name(value)
                    }
                }

                impl TryFrom<Keyword> for $name {
                    type Error = Keyword;

                    fn try_from(value: Keyword) -> Result<Self, Keyword> {
                        if let Keyword::$name(value) = value {
                            Ok(value)
                        } else {
                            Err(value)
                        }
                    }
                }
            )*

            macro_rules! logos_keywords {
                (
                    $d ( #[$d ($d enum_attrs:tt)*] )* $d visibility: vis enum $d enum_name : ident { $d ($d enum_variants:tt)* }
                ) => {
                    $d ( #[$d ($d enum_attrs)*] )* $d visibility enum $d enum_name {
                        $d ($d enum_variants)*
                        $(
                            #[token($value, |_| crate::keywords::Keyword::[< k_ $name:snake:lower >] ())]
                        )*
                        Keyword(crate::keywords::Keyword),
                    }
                };
            }
            pub(super) use logos_keywords;

            #[cfg(test)]
            mod tests {
                use super::*;

                $(
                    #[test]
                    fn [< keyword_ $name:snake:lower _should_parse >] () {
                        assert_eq!(Keyword::from_str($value), Some( Keyword:: [< k_ $name:snake:lower >] () ))
                    }
                )*
            }
        }
    };
}

keywords! {
    // need to build the internal `logos_keywords` macro declaration
    [$]

    // functions
    Fn("fn"),
    Return("return"),

    // variables
    Let("let"),

    // datatype
    Type("type"),
    Int("int"),
}
