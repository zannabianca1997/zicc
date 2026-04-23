use chumsky::{Parser, prelude::todo};
use zicc_assembler_ast::directive::Directive;

use crate::ParserExtra;

/// A single directive
pub(crate) fn directive<'s>() -> impl Parser<'s, &'s str, Directive, ParserExtra<'s>> {
    todo()
}
