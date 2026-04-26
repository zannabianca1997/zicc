//! Hand rolled parser
//!
//! Is this faster than the one in [`zicc-assembler-parser`]? Idk.

use std::collections::BTreeSet;

use snafu::{OptionExt, ResultExt};
use string_interner::DefaultStringInterner;
use zicc_assembler_ast::{
    expression::{Expr, IntLiteral},
    identifier::{CompilerIdentifier, Identifier, Provenance, SpecialIdentifier},
    labelled::Labelled,
};

use crate::{
    InvalidIntLiteralSnafu, InvalidNamedIdentifierSnafu, InvalidUnnamedIdentifierSnafu, ParseError,
};

pub(crate) fn parse(
    source: &str,
    interner: &mut DefaultStringInterner,
) -> Result<Vec<Labelled<Expr>>, ParseError> {
    source
        .split(',')
        .map(|e| parse_labelled_expr(e, interner))
        .collect()
}

fn parse_labelled_expr(
    mut source: &str,
    interner: &mut DefaultStringInterner,
) -> Result<Labelled<Expr>, ParseError> {
    let mut labels = BTreeSet::new();

    while let Some((label, rest)) = source.split_once(':') {
        labels.insert(parse_ident(label.trim(), interner)?);
        source = rest;
    }

    Ok(Labelled {
        labels,
        item: parse_expr(source, interner)?,
    })
}

fn parse_expr(source: &str, interner: &mut DefaultStringInterner) -> Result<Expr, ParseError> {
    let source = source.trim();
    if source.starts_with(|ch: char| ch.is_digit(10) || ch == '-') {
        return Ok(Expr::Constant {
            value: parse_int(source)?,
        });
    }
    let (ident, offset) = if let Some((ident, value)) = source.rsplit_once('+') {
        (ident.trim_end(), parse_int(value.trim_start())?)
    } else if let Some((ident, value)) = source.rsplit_once('-') {
        (ident.trim_end(), -parse_int(value.trim_start())?)
    } else {
        (source, IntLiteral::ZERO)
    };

    Ok(Expr::Offset {
        label: parse_ident(ident, interner)?,
        offset,
    })
}

fn parse_ident(
    source: &str,
    interner: &mut DefaultStringInterner,
) -> Result<Identifier, ParseError> {
    Ok(if let Some(source) = source.strip_prefix('$') {
        match source {
            "start" => Identifier::Special(SpecialIdentifier::Start),
            "end" => Identifier::Special(SpecialIdentifier::End),
            "unit_start" => Identifier::Special(SpecialIdentifier::UnitStart),
            "unit_end" => Identifier::Special(SpecialIdentifier::UnitEnd),
            int => Identifier::Unnamed {
                code: u32::from_str_radix(int, 10).context(InvalidUnnamedIdentifierSnafu)?,
            },
        }
    } else {
        let (name, provenance) = source
            .split_once('@')
            .map_or((source, None), |(s, p)| (s, Some(p)));

        let name = CompilerIdentifier::new(name, interner).context(InvalidNamedIdentifierSnafu)?;
        let provenance = provenance
            .map(|p| Provenance::new(p, interner).context(InvalidNamedIdentifierSnafu))
            .transpose()?;

        Identifier::Named { name, provenance }
    })
}

fn parse_int(source: &str) -> Result<IntLiteral, ParseError> {
    IntLiteral::parse(source).context(InvalidIntLiteralSnafu)
}
