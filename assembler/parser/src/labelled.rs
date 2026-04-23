use chumsky::{IterParser, Parser, prelude::just, text::inline_whitespace};
use zicc_assembler_ast::labelled::Labelled;

use crate::{ParserExtra, identifier::identifier};

/// Parse something preceded by a list of labels, like `a: b: <something>`
pub(crate) fn labelled<'s, T>(
    parser: impl Parser<'s, &'s str, T, ParserExtra<'s>>,
) -> impl Parser<'s, &'s str, Labelled<T>, ParserExtra<'s>> {
    identifier()
        .then_ignore(just(":").padded_by(inline_whitespace()))
        .labelled("label")
        .repeated()
        .collect()
        .then(parser)
        .map(|(labels, item)| Labelled { labels, item })
}
