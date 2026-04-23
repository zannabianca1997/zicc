//! Identifiers

use string_interner::DefaultSymbol;

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
        name: zicc_compiler_lexer::identifiers::Identifier,
        /// Optional provenance
        provenance: Option<Provenance>,
    },
}

/// Provenance of an identifier
///
/// To isolate symbols between compilation units, symbols can have
/// an optional provenance as multiple `@` prefixed base64 string
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Provenance(pub(crate) DefaultSymbol);
