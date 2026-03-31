//! # Punctuators
//!
//! Punctuators definitions

use std::fmt::Display;

use paste::paste;

macro_rules! punctuators {
    (
        $(
            $name:ident ( $value:literal )
        ),* $(,)?
    ) => {
        paste! {
            /// A punctuator
            #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
            pub enum Punctuator {
                $(
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
            )*
        }
    };
}

punctuators! {
    // Misc
    Semicolon(";"),
    Colon(":"),
    Comma(","),
    Eq("="),
    Underscore("_"),
    Ampersand("&"),
    At("@"),
    Dot("."),

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
