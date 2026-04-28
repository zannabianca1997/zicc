//! Identifiers

use std::{
    fmt::{self, Display as _},
    hash::BuildHasher,
};

use base64::{
    Engine,
    alphabet::Alphabet,
    engine::{GeneralPurpose, general_purpose::NO_PAD},
    prelude::BASE64_STANDARD,
};
use derive_more::Display;
use lazy_regex::{Lazy, Regex, regex};
use string_interner::{DefaultSymbol, StringInterner};
pub use zicc_compiler_lexer::identifiers::Identifier as CompilerIdentifier;
use zicc_display::DisplayWith;

type Symbol = DefaultSymbol;

/// Full identifier for the assembler
///
/// There are two types of identifiers: named and unnamed.
///
/// Unnamed identifiers cannot be exported, and as such they can freely be
/// renominated to merge two files.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Identifier {
    /// Unnamed identifiers: `$0`, `$1`, ...
    Unnamed { code: u32 },
    /// Named identifiers
    Named {
        /// Name of the identifier
        name: CompilerIdentifier,
        /// Optional provenance
        provenance: Option<Provenance>,
    },
    /// Special identifiers
    Special(SpecialIdentifier),
}

impl DisplayWith for Identifier {
    fn fmt_with(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        interner: &string_interner::DefaultStringInterner,
    ) -> std::fmt::Result {
        match self {
            Identifier::Unnamed { code } => write!(f, "${code}"),
            Identifier::Named { name, provenance } => {
                name.fmt_with(f, interner)?;
                if let Some(provenance) = provenance {
                    provenance.fmt_with(f, interner)?;
                }
                Ok(())
            }
            Identifier::Special(special_identifier) => special_identifier.fmt(f),
        }
    }
}

/// Special identifiers
///
/// Identifiers that point to special point of the file
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Display)]
pub enum SpecialIdentifier {
    /// `$start`: point to the start of the code, after the runtime trampoline
    #[display("$start")]
    Start,
    /// `$end`: point to the end of the code, and the start of the stack
    #[display("$start")]
    End,
    /// `$unit_start`: point to the start of the current compilation unit
    #[display("$unit_start")]
    UnitStart,
    /// `$unit_end`: point to the end of the current compilation unit
    #[display("$unit_end")]
    UnitEnd,
}

/// Regular expression to match identifiers
pub static PROVENANCE_RE: &Lazy<Regex> = regex!(r#"^[\._a-zA-Z0-9]+(?:@[\._a-zA-Z0-9]+)*$"#);

/// Provenance of an identifier
///
/// To isolate symbols between compilation units, symbols can have an optional
/// provenance as multiple `@` prefixed base64 string (additional chars are `.`
/// and `_`)
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Provenance(DefaultSymbol);

impl Provenance {
    /// Create a new provenance
    ///
    /// This will validate the given string against the regex [`PROVENANCE_RE`],
    /// and return [`Some`] only if a match is found
    pub fn new<B, H>(value: &str, interner: &mut StringInterner<B, H>) -> Option<Self>
    where
        B: string_interner::backend::Backend<Symbol = Symbol>,
        H: BuildHasher,
    {
        if !PROVENANCE_RE.is_match(value) {
            return None;
        }
        Some(Self(interner.get_or_intern(value)))
    }

    /// Decode the provenance into multiple byte strings
    pub fn decode<B, H>(self, interner: &StringInterner<B, H>) -> impl Iterator<Item = Vec<u8>>
    where
        B: string_interner::backend::Backend<Symbol = Symbol>,
        H: BuildHasher,
    {
        interner.resolve(self.0).unwrap().split('@').map(|value| {
            BASE64_PROVENANCE
                .decode(value)
                .expect("The regex should ensure decodability")
        })
    }

    /// Add a namespace to the provenance
    pub fn namespaced_to<B, H>(
        self,
        interner: &mut StringInterner<B, H>,
        namespace: impl AsRef<[u8]>,
    ) -> Self
    where
        B: string_interner::backend::Backend<Symbol = Symbol>,
        H: BuildHasher,
    {
        let mut current = interner.resolve(self.0).unwrap().to_owned();
        current.push('@');
        BASE64_PROVENANCE.encode_string(namespace, &mut current);
        Self(interner.get_or_intern(current))
    }
}

/// Base64 with alphabet using `.` and `_` as last chars
///
/// Both standard and url safe uses `-` and `+` that might confuse simpler parsers.
const BASE64_PROVENANCE: GeneralPurpose = GeneralPurpose::new(
    &{
        let Ok(alphabet) =
            Alphabet::new("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789._")
        else {
            unreachable!()
        };
        alphabet
    },
    NO_PAD,
);

impl DisplayWith for Provenance {
    fn fmt_with(
        &self,
        f: &mut fmt::Formatter<'_>,
        interner: &string_interner::DefaultStringInterner,
    ) -> fmt::Result {
        match interner.resolve(self.0) {
            Some(s) => s.fmt(f),
            None => Err(std::fmt::Error),
        }
    }
}
