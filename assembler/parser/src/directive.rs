use chumsky::{IterParser, Parser, prelude::just, text::inline_whitespace};
use zicc_assembler_ast::directive::Directive;

use crate::{
    ParserExtra,
    expr::{expr, int_literal},
    labelled::labelled,
};

fn data<'s>() -> impl Parser<'s, &'s str, Directive, ParserExtra<'s>> {
    just("DATA")
        .then_ignore(inline_whitespace().at_least(1))
        .ignore_then(
            labelled(expr())
                .separated_by(inline_whitespace().at_least(1))
                .collect(),
        )
        .map(Directive::Data)
        .labelled("data directive")
}

fn zeros<'s>() -> impl Parser<'s, &'s str, Directive, ParserExtra<'s>> {
    just("ZEROS")
        .then_ignore(inline_whitespace().at_least(1))
        .ignore_then(int_literal())
        .map(Directive::Zeros)
        .labelled("zeros directive")
}

/// A single directive
pub(crate) fn directive<'s>() -> impl Parser<'s, &'s str, Directive, ParserExtra<'s>> {
    data().or(zeros()).labelled("directive")
}
