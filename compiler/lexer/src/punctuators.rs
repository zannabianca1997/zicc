//! # Punctuators
//!
//! Punctuators definitions

use std::fmt::Display;

use paste::paste;

use crate::Token;

macro_rules! punctuators {
    (
        [$d:tt]
        $(
            $name:ident ( $value:literal )
        ),* $(,)?
    ) => {
        paste! {
            /// A punctuator
            #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
            pub enum Punctuator {
                $(
                    #[doc="The `" $value "` punctuator"]
                    $name($name)
                ),*
            }

            impl Punctuator {
                $(
                   #[doc="The `" $value "` punctuator"]
                   pub const fn [< $name:snake:lower >] () -> Self { Self::$name($name) }
                )*

                /// The punctuator as a string
                pub const fn as_str(&self) -> &'static str {
                    match self {
                        $(Self::$name(_) => $value),*
                    }
                }

                /// Try to parse the string as a punctuator
                pub fn from_str(value: &str) -> Option<Self> {
                    match value {
                        $($value => Some( Self :: [< $name:snake:lower >] () ), )*
                        _ => None
                    }
                }
            }

            impl TryFrom<Token> for Punctuator {
                type Error = Token;

                fn try_from(value: Token) -> Result<Self, Token> {
                    if let Token::Punctuator(value) = value {
                        Ok(value)
                    } else {
                        Err(value)
                    }
                }
            }

            impl Display for Punctuator {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    write!(f, "{}", self.as_str())
                }
            }


            $(
                #[doc="The `" $value "` punctuator"]
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


                impl From<$name> for Punctuator {
                    fn from(value: $name) -> Self {
                        Self::$name(value)
                    }
                }

                impl TryFrom<Punctuator> for $name {
                    type Error = Punctuator;

                    fn try_from(value: Punctuator) -> Result<Self, Punctuator> {
                        if let Punctuator::$name(value) = value {
                            Ok(value)
                        } else {
                            Err(value)
                        }
                    }
                }
            )*

            macro_rules! logos_punctuators {
                (
                    $d ( #[$d ($d enum_attrs:tt)*] )* $d visibility: vis enum $d enum_name : ident { $d ($d enum_variants:tt)* }
                ) => {
                    $d ( #[$d ($d enum_attrs)*] )* $d visibility enum $d enum_name {
                        $d ($d enum_variants)*
                        $(
                            #[token($value, |_| crate::punctuators::Punctuator::[< $name:snake:lower >] ())]
                        )*
                        Punctuator(crate::punctuators::Punctuator),
                    }
                };
            }
            pub(super) use logos_punctuators;


            #[cfg(test)]
            mod tests {
                use super::*;

                $(
                    #[test]
                    fn [< punctuator_ $name:snake:lower _should_parse >] () {
                        assert_eq!(Punctuator::from_str($value), Some( Punctuator:: [< $name:snake:lower >] () ))
                    }
                )*
            }
        }
    };
}

punctuators! {
    // need to build the internal `logos_punctuators` macro declaration
    [$]

    // Misc
    Semicolon(";"),
    Colon(":"),
    Comma(","),
    Eq("="),
    Underscore("_"),
    Ampersand("&"),
    At("@"),
    Dot("."),
    RightArrow("->"),

    // Math
    Plus("+"),
    Minus("-"),
    Star("*"),

    // Booleans
    EqEq("=="),
    Neq("!="),
    Lt("<"),
    Le("<="),
    Gt(">"),
    Ge(">="),
    And("&&"),
    Or("||"),
    Not("!"),

    // Parentheses
    ParenthesesOpen("("),
    ParenthesesClose(")"),
    BracketOpen("["),
    BracketClose("]"),
    BraceOpen("{"),
    BraceClose("}"),
}
