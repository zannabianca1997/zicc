use zicc_compiler_lexer::int_literal::IntLiteral;
use zicc_display::DisplayWith;

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

impl DisplayWith for Directive {
    fn fmt_with(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        interner: &string_interner::DefaultStringInterner,
    ) -> std::fmt::Result {
        match self {
            Directive::Data(data) => {
                write!(f, "DATA")?;
                for value in data {
                    write!(f, " ")?;
                    value.fmt_with(f, interner)?;
                }
            }
            Directive::Zeros(len) => write!(f, "ZEROS {len}")?,
        };

        Ok(())
    }
}
