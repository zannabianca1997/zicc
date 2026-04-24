use zicc_compiler_lexer::int_literal::IntLiteral;

use crate::{expression::Expr, labelled::Labelled};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Directive {
    /// List of value to copy verbatim
    Data(Vec<Labelled<Expr>>),
    /// Specific number of zeros
    ///
    /// Not labelled as this is not a memory position
    Zeros(IntLiteral),
}
