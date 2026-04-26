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
    let source = source.trim();
    if source.is_empty() {
        return Ok(vec![]);
    }
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

#[cfg(test)]
mod tests {
    use std::collections::BTreeSet;

    use string_interner::DefaultStringInterner;
    use zicc_assembler_ast::{
        expression::Expr,
        identifier::{Identifier, SpecialIdentifier},
        labelled::Labelled,
    };

    use super::*;

    fn interner() -> DefaultStringInterner {
        DefaultStringInterner::new()
    }

    // ---- parse_int ----

    /// Positive integers should parse to their expected values
    #[test]
    fn parse_int_should_handle_positive_integers() {
        assert_eq!(parse_int("0").unwrap(), IntLiteral::ZERO);
        assert_eq!(parse_int("42").unwrap(), IntLiteral::parse("42").unwrap());
        assert_eq!(parse_int("999").unwrap(), IntLiteral::parse("999").unwrap());
    }

    /// Negative integers should parse correctly
    #[test]
    fn parse_int_should_handle_negative_integers() {
        assert_eq!(parse_int("-7").unwrap(), IntLiteral::parse("-7").unwrap());
        assert_eq!(parse_int("-0").unwrap(), IntLiteral::ZERO);
    }

    /// Plus-prefixed numbers should strip the prefix
    #[test]
    fn parse_int_should_handle_plus_prefix() {
        assert_eq!(parse_int("+7").unwrap(), IntLiteral::parse("7").unwrap());
        assert_eq!(parse_int("+0").unwrap(), IntLiteral::ZERO);
    }

    /// An empty string should fail to parse as an integer
    #[test]
    fn parse_int_should_reject_empty_string() {
        assert!(parse_int("").is_err());
    }

    /// Non-numeric strings should be rejected
    #[test]
    fn parse_int_should_reject_non_numeric() {
        assert!(parse_int("abc").is_err());
        assert!(parse_int("1.5").is_err());
    }

    // ---- parse_ident ----

    /// Unnamed identifiers like `$0`, `$42` should parse to `Identifier::Unnamed`
    #[test]
    fn parse_ident_should_parse_unnamed_identifiers() {
        let mut interner = interner();
        assert_eq!(
            parse_ident("$0", &mut interner).unwrap(),
            Identifier::Unnamed { code: 0 },
        );
        assert_eq!(
            parse_ident("$42", &mut interner).unwrap(),
            Identifier::Unnamed { code: 42 },
        );
    }

    /// Special identifiers `$start`, `$end`, `$unit_start`, `$unit_end`
    /// should map to their corresponding `SpecialIdentifier` variants
    #[test]
    fn parse_ident_should_parse_special_identifiers() {
        let mut interner = interner();
        assert_eq!(
            parse_ident("$start", &mut interner).unwrap(),
            Identifier::Special(SpecialIdentifier::Start),
        );
        assert_eq!(
            parse_ident("$end", &mut interner).unwrap(),
            Identifier::Special(SpecialIdentifier::End),
        );
        assert_eq!(
            parse_ident("$unit_start", &mut interner).unwrap(),
            Identifier::Special(SpecialIdentifier::UnitStart),
        );
        assert_eq!(
            parse_ident("$unit_end", &mut interner).unwrap(),
            Identifier::Special(SpecialIdentifier::UnitEnd),
        );
    }

    /// Named identifiers without provenance should parse to `Identifier::Named`
    #[test]
    fn parse_ident_should_parse_named_identifiers() {
        let mut interner = interner();
        let name = "foo";
        let expected = Identifier::Named {
            name: zicc_assembler_ast::identifier::CompilerIdentifier::new(name, &mut interner)
                .unwrap(),
            provenance: None,
        };
        assert_eq!(parse_ident(name, &mut interner).unwrap(), expected);
    }

    /// Named identifiers with a provenance (`name@provenance`) should include
    /// the provenance in the result
    #[test]
    fn parse_ident_should_parse_named_identifiers_with_provenance() {
        let mut interner = interner();
        let result = parse_ident("foo@bar", &mut interner).unwrap();
        let name =
            zicc_assembler_ast::identifier::CompilerIdentifier::new("foo", &mut interner).unwrap();
        let provenance =
            zicc_assembler_ast::identifier::Provenance::new("bar", &mut interner).unwrap();
        assert_eq!(
            result,
            Identifier::Named {
                name,
                provenance: Some(provenance),
            },
        );
    }

    /// A bare `$` with no code should fail (empty unnamed identifier)
    #[test]
    fn parse_ident_should_reject_bare_dollar() {
        let mut interner = interner();
        assert!(parse_ident("$", &mut interner).is_err());
    }

    /// Non-numeric content after `$` should fail (e.g. `$abc`)
    #[test]
    fn parse_ident_should_reject_non_numeric_unnamed() {
        let mut interner = interner();
        assert!(parse_ident("$abc", &mut interner).is_err());
    }

    /// An identifier starting with `@` is invalid
    #[test]
    fn parse_ident_should_reject_bare_at() {
        let mut interner = interner();
        assert!(parse_ident("@foo", &mut interner).is_err());
    }

    /// A trailing `@` with no provenance name is invalid
    #[test]
    fn parse_ident_should_reject_trailing_at() {
        let mut interner = interner();
        assert!(parse_ident("foo@", &mut interner).is_err());
    }

    // ---- parse_expr ----

    /// Standalone integers should parse as `Expr::Constant`
    #[test]
    fn parse_expr_should_parse_constant_integers() {
        let mut interner = interner();
        assert_eq!(
            parse_expr("42", &mut interner).unwrap(),
            Expr::Constant {
                value: IntLiteral::parse("42").unwrap(),
            },
        );
        assert_eq!(
            parse_expr("-7", &mut interner).unwrap(),
            Expr::Constant {
                value: IntLiteral::parse("-7").unwrap(),
            },
        );
        assert_eq!(
            parse_expr("0", &mut interner).unwrap(),
            Expr::Constant {
                value: IntLiteral::ZERO,
            },
        );
    }

    /// A bare label (no offset) should parse as `Expr::Offset` with offset zero
    #[test]
    fn parse_expr_should_parse_label_only_as_offset() {
        let mut interner = interner();
        let name = "foo";
        let label = Identifier::Named {
            name: zicc_assembler_ast::identifier::CompilerIdentifier::new(name, &mut interner)
                .unwrap(),
            provenance: None,
        };
        assert_eq!(
            parse_expr(name, &mut interner).unwrap(),
            Expr::Offset {
                label,
                offset: IntLiteral::ZERO,
            },
        );
    }

    /// `label+N` should parse as `Expr::Offset` with a positive offset
    #[test]
    fn parse_expr_should_parse_label_with_positive_offset() {
        let mut interner = interner();
        let label = Identifier::Named {
            name: zicc_assembler_ast::identifier::CompilerIdentifier::new("foo", &mut interner)
                .unwrap(),
            provenance: None,
        };
        assert_eq!(
            parse_expr("foo+3", &mut interner).unwrap(),
            Expr::Offset {
                label,
                offset: IntLiteral::parse("3").unwrap(),
            },
        );
    }

    /// `label-N` should parse as `Expr::Offset` with a negative offset
    #[test]
    fn parse_expr_should_parse_label_with_negative_offset() {
        let mut interner = interner();
        let label = Identifier::Named {
            name: zicc_assembler_ast::identifier::CompilerIdentifier::new("bar", &mut interner)
                .unwrap(),
            provenance: None,
        };
        assert_eq!(
            parse_expr("bar-5", &mut interner).unwrap(),
            Expr::Offset {
                label,
                offset: IntLiteral::parse("-5").unwrap(),
            },
        );
    }

    /// Special identifiers can be used as labels in expressions
    #[test]
    fn parse_expr_should_parse_special_ident() {
        let mut interner = interner();
        assert_eq!(
            parse_expr("$start", &mut interner).unwrap(),
            Expr::Offset {
                label: Identifier::Special(SpecialIdentifier::Start),
                offset: IntLiteral::ZERO,
            },
        );
        assert_eq!(
            parse_expr("$end+1", &mut interner).unwrap(),
            Expr::Offset {
                label: Identifier::Special(SpecialIdentifier::End),
                offset: IntLiteral::parse("1").unwrap(),
            },
        );
    }

    /// Unnamed identifiers can be used as labels in expressions
    #[test]
    fn parse_expr_should_parse_unnamed_ident() {
        let mut interner = interner();
        assert_eq!(
            parse_expr("$42", &mut interner).unwrap(),
            Expr::Offset {
                label: Identifier::Unnamed { code: 42 },
                offset: IntLiteral::ZERO,
            },
        );
    }

    /// Named identifiers with provenance and an offset should parse correctly
    #[test]
    fn parse_expr_should_parse_ident_with_provenance_and_offset() {
        let mut interner = interner();
        let name =
            zicc_assembler_ast::identifier::CompilerIdentifier::new("foo", &mut interner).unwrap();
        let provenance =
            zicc_assembler_ast::identifier::Provenance::new("bar", &mut interner).unwrap();
        assert_eq!(
            parse_expr("foo@bar+7", &mut interner).unwrap(),
            Expr::Offset {
                label: Identifier::Named {
                    name,
                    provenance: Some(provenance),
                },
                offset: IntLiteral::parse("7").unwrap(),
            },
        );
    }

    /// Whitespace around the expression should be trimmed
    #[test]
    fn parse_expr_should_trim_whitespace() {
        let mut interner = interner();
        assert_eq!(
            parse_expr("  42  ", &mut interner).unwrap(),
            Expr::Constant {
                value: IntLiteral::parse("42").unwrap(),
            },
        );
        assert_eq!(
            parse_expr("  foo  ", &mut interner).unwrap(),
            Expr::Offset {
                label: Identifier::Named {
                    name: zicc_assembler_ast::identifier::CompilerIdentifier::new(
                        "foo",
                        &mut interner
                    )
                    .unwrap(),
                    provenance: None,
                },
                offset: IntLiteral::ZERO,
            },
        );
    }

    // ---- parse_labelled_expr ----

    /// An expression without any labels should produce an empty label set
    #[test]
    fn parse_labelled_expr_should_parse_unlabelled_expr() {
        let mut interner = interner();
        let result = parse_labelled_expr("42", &mut interner).unwrap();
        assert!(result.labels.is_empty());
        assert_eq!(
            result.item,
            Expr::Constant {
                value: IntLiteral::parse("42").unwrap(),
            },
        );
    }

    /// A single `label:` before an expression should produce one label
    #[test]
    fn parse_labelled_expr_should_parse_single_label() {
        let mut interner = interner();
        let result = parse_labelled_expr("label: 42", &mut interner).unwrap();
        let expected_label = Identifier::Named {
            name: zicc_assembler_ast::identifier::CompilerIdentifier::new("label", &mut interner)
                .unwrap(),
            provenance: None,
        };
        let mut expected_labels = BTreeSet::new();
        expected_labels.insert(expected_label);
        assert_eq!(result.labels, expected_labels);
        assert_eq!(
            result.item,
            Expr::Constant {
                value: IntLiteral::parse("42").unwrap(),
            },
        );
    }

    /// Multiple labels separated by colons should all be collected
    #[test]
    fn parse_labelled_expr_should_parse_multiple_labels() {
        let mut interner = interner();
        let result = parse_labelled_expr("a: b: foo", &mut interner).unwrap();
        let label_a = Identifier::Named {
            name: zicc_assembler_ast::identifier::CompilerIdentifier::new("a", &mut interner)
                .unwrap(),
            provenance: None,
        };
        let label_b = Identifier::Named {
            name: zicc_assembler_ast::identifier::CompilerIdentifier::new("b", &mut interner)
                .unwrap(),
            provenance: None,
        };
        let label_foo = Identifier::Named {
            name: zicc_assembler_ast::identifier::CompilerIdentifier::new("foo", &mut interner)
                .unwrap(),
            provenance: None,
        };
        let mut expected_labels = BTreeSet::new();
        expected_labels.insert(label_a);
        expected_labels.insert(label_b);
        assert_eq!(result.labels, expected_labels);
        assert_eq!(
            result.item,
            Expr::Offset {
                label: label_foo,
                offset: IntLiteral::ZERO,
            },
        );
    }

    /// Unnamed identifiers (`$0`, `$1`) can be used as labels
    #[test]
    fn parse_labelled_expr_should_handle_unnamed_labels() {
        let mut interner = interner();
        let result = parse_labelled_expr("$0: $1: 42", &mut interner).unwrap();
        let mut expected_labels = BTreeSet::new();
        expected_labels.insert(Identifier::Unnamed { code: 0 });
        expected_labels.insert(Identifier::Unnamed { code: 1 });
        assert_eq!(result.labels, expected_labels);
        assert_eq!(
            result.item,
            Expr::Constant {
                value: IntLiteral::parse("42").unwrap(),
            },
        );
    }

    /// Special identifiers can be used as labels
    #[test]
    fn parse_labelled_expr_should_handle_special_labels() {
        let mut interner = interner();
        let result = parse_labelled_expr("$start: foo", &mut interner).unwrap();
        let mut expected_labels = BTreeSet::new();
        expected_labels.insert(Identifier::Special(SpecialIdentifier::Start));
        assert_eq!(result.labels, expected_labels);
    }

    /// Whitespace around label names and colons should be trimmed
    #[test]
    fn parse_labelled_expr_should_trim_label_whitespace() {
        let mut interner = interner();
        let result = parse_labelled_expr("  label  :  42", &mut interner).unwrap();
        let expected_label = Identifier::Named {
            name: zicc_assembler_ast::identifier::CompilerIdentifier::new("label", &mut interner)
                .unwrap(),
            provenance: None,
        };
        let mut expected_labels = BTreeSet::new();
        expected_labels.insert(expected_label);
        assert_eq!(result.labels, expected_labels);
    }

    // ---- parse (main) ----

    /// A single expression without commas should produce one result
    #[test]
    fn parse_should_handle_single_expression() {
        let mut interner = interner();
        let result = parse("42", &mut interner).unwrap();
        assert_eq!(result.len(), 1);
        assert_eq!(
            result[0],
            Labelled::unlabelled(Expr::Constant {
                value: IntLiteral::parse("42").unwrap(),
            }),
        );
    }

    /// Multiple comma-separated expressions should each be parsed
    #[test]
    fn parse_should_handle_multiple_expressions() {
        let mut interner = interner();
        let result = parse("42, foo", &mut interner).unwrap();
        assert_eq!(result.len(), 2);
        assert_eq!(
            result[0],
            Labelled::unlabelled(Expr::Constant {
                value: IntLiteral::parse("42").unwrap(),
            }),
        );
        let label_foo = Identifier::Named {
            name: zicc_assembler_ast::identifier::CompilerIdentifier::new("foo", &mut interner)
                .unwrap(),
            provenance: None,
        };
        assert_eq!(
            result[1],
            Labelled::unlabelled(Expr::Offset {
                label: label_foo,
                offset: IntLiteral::ZERO,
            }),
        );
    }

    /// Labelled expressions separated by commas should all be parsed correctly
    #[test]
    fn parse_should_handle_labelled_expressions() {
        let mut interner = interner();
        let result = parse("label: 42, other: foo", &mut interner).unwrap();
        assert_eq!(result.len(), 2);

        let label_label =
            zicc_assembler_ast::identifier::CompilerIdentifier::new("label", &mut interner)
                .unwrap();
        let label_other =
            zicc_assembler_ast::identifier::CompilerIdentifier::new("other", &mut interner)
                .unwrap();
        let label_foo =
            zicc_assembler_ast::identifier::CompilerIdentifier::new("foo", &mut interner).unwrap();

        let mut expected_labels_0 = BTreeSet::new();
        expected_labels_0.insert(Identifier::Named {
            name: label_label,
            provenance: None,
        });
        assert_eq!(
            result[0],
            Labelled {
                labels: expected_labels_0,
                item: Expr::Constant {
                    value: IntLiteral::parse("42").unwrap(),
                },
            },
        );

        let mut expected_labels_1 = BTreeSet::new();
        expected_labels_1.insert(Identifier::Named {
            name: label_other,
            provenance: None,
        });
        assert_eq!(
            result[1],
            Labelled {
                labels: expected_labels_1,
                item: Expr::Offset {
                    label: Identifier::Named {
                        name: label_foo,
                        provenance: None,
                    },
                    offset: IntLiteral::ZERO,
                },
            },
        );
    }

    /// An empty input string should produce an error
    #[test]
    fn parse_should_handle_empty_input() {
        let mut interner = interner();
        let result = parse("  ", &mut interner).unwrap();
        assert_eq!(result.len(), 0)
    }

    /// Whitespace around commas and expressions should be handled gracefully
    #[test]
    fn parse_should_handle_whitespace() {
        let mut interner = interner();
        let result = parse(" 42 , foo ", &mut interner).unwrap();
        assert_eq!(result.len(), 2);
    }

    /// Expressions with positive and negative offsets should parse correctly
    #[test]
    fn parse_should_handle_expression_with_offsets() {
        let mut interner = interner();
        let result = parse("bar+3, baz-5", &mut interner).unwrap();
        assert_eq!(result.len(), 2);

        let label_bar = Identifier::Named {
            name: zicc_assembler_ast::identifier::CompilerIdentifier::new("bar", &mut interner)
                .unwrap(),
            provenance: None,
        };
        let label_baz = Identifier::Named {
            name: zicc_assembler_ast::identifier::CompilerIdentifier::new("baz", &mut interner)
                .unwrap(),
            provenance: None,
        };

        assert_eq!(
            result[0],
            Labelled::unlabelled(Expr::Offset {
                label: label_bar,
                offset: IntLiteral::parse("3").unwrap(),
            }),
        );
        assert_eq!(
            result[1],
            Labelled::unlabelled(Expr::Offset {
                label: label_baz,
                offset: IntLiteral::parse("-5").unwrap(),
            }),
        );
    }

    /// Special and unnamed identifiers as expressions should work
    #[test]
    fn parse_should_handle_special_and_unnamed_identifiers() {
        let mut interner = interner();
        let result = parse("$start, $0", &mut interner).unwrap();
        assert_eq!(result.len(), 2);
        assert_eq!(
            result[0],
            Labelled::unlabelled(Expr::Offset {
                label: Identifier::Special(SpecialIdentifier::Start),
                offset: IntLiteral::ZERO,
            }),
        );
        assert_eq!(
            result[1],
            Labelled::unlabelled(Expr::Offset {
                label: Identifier::Unnamed { code: 0 },
                offset: IntLiteral::ZERO,
            }),
        );
    }

    /// Invalid syntax should propagate errors
    #[test]
    fn parse_should_reject_invalid_identifiers() {
        let mut interner = interner();
        assert!(parse("$", &mut interner).is_err());
        assert!(parse("@foo", &mut interner).is_err());
        assert!(parse("$abc", &mut interner).is_err());
    }

    /// A label chain parsing many labels should collect them all
    #[test]
    fn parse_should_handle_many_labels() {
        let mut interner = interner();
        let result = parse("a: b: c: d: e: 42", &mut interner).unwrap();
        assert_eq!(result.len(), 1);
        assert_eq!(result[0].labels.len(), 5);
    }
}
