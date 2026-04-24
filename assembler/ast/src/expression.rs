pub use zicc_compiler_lexer::int_literal::IntLiteral;

use crate::identifier::Identifier;

/// A solvable expression
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Expr {
    /// Constant value: `3`
    Constant { value: IntLiteral },
    /// Labelled position plus offset: `a + 3`
    Offset {
        label: Identifier,
        offset: Option<IntLiteral>,
    },
}
