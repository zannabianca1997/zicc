//! Identifiers

use std::hash::BuildHasher;

use lazy_regex::{Lazy, Regex, regex};
use string_interner::{DefaultSymbol, StringInterner};
pub use zicc_compiler_lexer::identifiers::Identifier as CompilerIdentifier;

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

/// Special identifiers
///
/// Identifiers that point to special point of the file
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum SpecialIdentifier {
    /// `$start`: point to the start of the code, after the runtime trampoline
    Start,
    /// `$end`: point to the end of the code, and the start of the stack
    End,
    /// `$unit_start`: point to the start of the current compilation unit
    UnitStart,
    /// `$unit_end`: point to the end of the current compilation unit
    UnitEnd,
}

/// Regular expression to match identifiers
pub static PROVENANCE_RE: &Lazy<Regex> = regex!(r#"^[-_a-zA-Z0-9]+(?:@[-_a-zA-Z0-9]+)$"#);

/// Provenance of an identifier
///
/// To isolate symbols between compilation units, symbols can have
/// an optional provenance as multiple `@` prefixed base64 string
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Provenance(pub(crate) DefaultSymbol);

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
}
