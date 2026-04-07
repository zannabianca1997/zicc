//! # Keywords
//!
//! Keywords definitions

use std::fmt::Display;

use paste::paste;

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

            impl Display for Keyword {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    write!(f, "{}", self.as_str())
                }
            }

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

                impl From<$name> for Keyword {
                    fn from(value: $name) -> Self {
                        Self::$name(value)
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
                            #[token($value, |_| crate::tokens::keywords::Keyword::[< k_ $name:snake:lower >] ())]
                        )*
                        Keyword(crate::tokens::keywords::Keyword),
                    }
                };
            }
            pub(super) use logos_keywords;
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
