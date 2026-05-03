use chumsky::{
    Parser,
    prelude::just,
    text::{digits, inline_whitespace},
};
use zicc_assembler_ast::expression::{Expr, IntLiteral};

use crate::{ParserExtra, identifier::identifier};

pub(crate) fn int_literal<'s>() -> impl Parser<'s, &'s str, IntLiteral, ParserExtra<'s>> {
    digits(10)
        .to_slice()
        .map(|d: &str| IntLiteral::parse(d).unwrap())
        .labelled("int literal")
}
fn sign<'s>() -> impl Parser<'s, &'s str, bool, ParserExtra<'s>> {
    just("+").to(false).or(just("-").to(true)).labelled("sign")
}
fn int_literal_with_opt_sign<'s>() -> impl Parser<'s, &'s str, IntLiteral, ParserExtra<'s>> {
    sign()
        .then_ignore(inline_whitespace())
        .or_not()
        .map(|s| s.unwrap_or(false))
        .then(int_literal())
        .map(|(neg, value)| if neg { -value } else { value })
}
fn int_literal_with_sign<'s>() -> impl Parser<'s, &'s str, IntLiteral, ParserExtra<'s>> {
    sign()
        .then_ignore(inline_whitespace())
        .then(int_literal())
        .map(|(neg, value)| if neg { -value } else { value })
}

/// An expression
///
/// ICA has very simple expressions: either integer, or label plus offset
pub(crate) fn expr<'s>() -> impl Parser<'s, &'s str, Expr, ParserExtra<'s>> {
    int_literal_with_opt_sign()
        .map(|value| Expr::Constant { value })
        .labelled("constant")
        .or(identifier()
            .then(
                inline_whitespace()
                    .ignore_then(int_literal_with_sign())
                    .or_not(),
            )
            .map(|(label, offset)| Expr::Offset {
                label,
                offset: offset.unwrap_or_else(|| IntLiteral::ZERO),
            })
            .labelled("label and offset"))
}
