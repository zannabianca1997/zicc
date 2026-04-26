use std::fmt::Pointer;

pub use zicc_compiler_lexer::int_literal::IntLiteral;
use zicc_display::DisplayWith;

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

impl DisplayWith for Expr {
    fn fmt_with(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        interner: &string_interner::DefaultStringInterner,
    ) -> std::fmt::Result {
        match self {
            Expr::Constant { value } => value.fmt(f),
            Expr::Offset { label, offset } => {
                label.fmt_with(f, interner)?;
                if let Some(offset) = offset {
                    if !offset.is_negative() {
                        write!(f, "+")?;
                    }
                    write!(f, "{offset}")?;
                }
                Ok(())
            }
        }
    }
}
