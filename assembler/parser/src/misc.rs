use chumsky::{
    Parser,
    prelude::{any, just},
    text::newline,
};

use crate::ParserExtra;

/// Separator between two lines
pub(crate) fn line_separator<'s>() -> impl Parser<'s, &'s str, (), ParserExtra<'s>> {
    // End of line comment: `;` followed by any char that is not a newline
    just(";")
        .then(any().and_is(newline().not()).repeated())
        .or_not()
        .then(newline())
        .ignored()
        .labelled("end of instruction")
}
