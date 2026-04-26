use chumsky::{
    Parser,
    error::Rich,
    input::MapExtra,
    prelude::{just, one_of},
    text::{ascii::ident, int},
};
use zicc_assembler_ast::identifier::{
    CompilerIdentifier, Identifier, Provenance, SpecialIdentifier,
};

use crate::ParserExtra;

/// A single identifier
pub(crate) fn identifier<'s>() -> impl Parser<'s, &'s str, Identifier, ParserExtra<'s>> {
    named_identifier()
        .or(unnamed_identifier())
        .or(special_identifier())
        .labelled("identifier")
}

fn named_identifier<'s>() -> impl Parser<'s, &'s str, Identifier, ParserExtra<'s>> {
    ident::<_, ParserExtra>()
        .try_map_with(|i, e| {
            CompilerIdentifier::new(i, e.state().interner)
                .ok_or(Rich::custom(e.span(), "Invalid identifier"))
        })
        .labelled("compiler identifier")
        .then(just("@").ignore_then(provenance()).or_not())
        .map(|(name, provenance)| Identifier::Named { name, provenance })
        .labelled("named identifier")
}

fn unnamed_identifier<'s>() -> impl Parser<'s, &'s str, Identifier, ParserExtra<'s>> {
    just("$")
        .ignore_then(
            int(10)
                .try_map(|s: &str, span| s.parse().map_err(|e| Rich::custom(span, e)))
                .labelled("integer label"),
        )
        .map(|code| Identifier::Unnamed { code })
        .labelled("unnamed identifier")
}

fn special_identifier<'s>() -> impl Parser<'s, &'s str, Identifier, ParserExtra<'s>> {
    just("$start")
        .to(SpecialIdentifier::Start)
        .or(just("$end").to(SpecialIdentifier::End))
        .or(just("$unit_start").to(SpecialIdentifier::UnitStart))
        .or(just("$unit_end").to(SpecialIdentifier::UnitEnd))
        .map(Identifier::Special)
        .labelled("special identifier")
}

/// Identifier provenance
fn provenance<'s>() -> impl Parser<'s, &'s str, Provenance, ParserExtra<'s>> {
    one_of('a'..'z')
        .or(one_of('A'..'Z'))
        .or(one_of('0'..'9'))
        .or(one_of("-_"))
        .repeated()
        .at_least(1)
        .labelled("base64 string")
        .separated_by(just("@"))
        .to_slice()
        .map_with(|s, extra: &mut MapExtra<&str, ParserExtra>| {
            Provenance::new(s, &mut extra.state().interner)
                .expect("the parser should match only valid provenances")
        })
        .labelled("provenance")
}
