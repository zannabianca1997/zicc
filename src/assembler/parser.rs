//! Parse an assembly file

use std::{
    assert_matches::{assert_matches, debug_assert_matches},
    collections::HashSet,
    num::ParseIntError,
    ops::Neg,
};

use either::Either::{self, Left, Right};
use pest::{iterators::Pair, Parser};
use pest_derive::Parser;
use thiserror::Error;

use crate::{assembler::label::Labellable, ICValue};

use super::{
    assembly_file::{Line, RawAssemblyFile},
    directive::Directive,
    instruction::{Instruction, ReadParam, WriteParam},
    label::{Label, Labelled},
    relocatable::RlValue,
};

#[derive(Parser)]
#[grammar = "assembler/grammar.pest"]
struct AssemblyParser;

#[derive(Debug, Error)]
pub enum ParseError {
    #[error("Error while parsing source")]
    Parsing(
        #[from]
        #[source]
        pest::error::Error<Rule>,
    ),
    #[error("Label {0} is defined twice")]
    RedefinitedLabel(Label),
    #[error("Numeric label is too long for usize")]
    NumLabelTooLong(#[source] ParseIntError),
    #[error("Numeric literal is too long for usize")]
    LiteralTooLong(#[source] ParseIntError),
}

pub type Result<T> = std::result::Result<T, ParseError>;

pub fn parse(source: &str) -> Result<RawAssemblyFile> {
    // parse the text
    let parsed = AssemblyParser::parse(Rule::file, source)?;
    // keep track of the label meeted
    let mut meeted_labels = HashSet::new();
    // convert to memory rapresentation
    Ok(RawAssemblyFile(
        parsed
            .map(|line| parse_line(line, &mut meeted_labels))
            .collect::<Result<_>>()?,
    ))
}

fn parse_line(src: Pair<Rule>, meeted_labels: &mut HashSet<Label>) -> Result<Line> {
    debug_assert_matches!(src.as_rule(), Rule::line);
    let mut src = src.into_inner();
    // Read labels
    let labels = parse_labels(src.next().unwrap(), meeted_labels)?;
    // parse content
    let content = src
        .next()
        .map(|content| {
            match content.as_rule() {
                Rule::instruction => Right(parse_instruction(content, meeted_labels)),
                Rule::directive => Left(parse_directive(content, meeted_labels)),
                _ => unreachable!(),
            }
            .factor_err()
        })
        .transpose()?;

    Ok(labels.map(|_| content))
}

fn parse_labels(src: Pair<Rule>, meeted_labels: &mut HashSet<Label>) -> Result<Labelled<()>> {
    debug_assert_matches!(src.as_rule(), Rule::labels);
    // parse labels
    let mut lbls = HashSet::new();
    for lbl in src.into_inner() {
        let lbl = parse_label(lbl)?;
        if meeted_labels.contains(&lbl) {
            return Err(ParseError::RedefinitedLabel(lbl));
        }
        meeted_labels.insert(lbl.clone());
        lbls.insert(lbl);
    }
    Ok(Labelled { inner: (), lbls })
}

fn parse_label(src: Pair<Rule>) -> Result<Label> {
    Ok(match src.as_rule() {
        Rule::number => Label::Numeric(src.as_str().parse().map_err(ParseError::NumLabelTooLong)?),
        Rule::local_identifier => Label::Local(src.as_str()[1..].parse().unwrap()),
        Rule::identifier => Label::Global(src.as_str().parse().unwrap()),
        _ => unreachable!(),
    })
}

fn parse_instruction(src: Pair<Rule>, meeted_labels: &mut HashSet<Label>) -> Result<Instruction> {
    debug_assert_matches!(src.as_rule(), Rule::instruction);
    // split by type of arguments
    use Instruction::*;
    let mut pairs = src.into_inner();
    let kw = pairs.next().unwrap().as_rule();
    Ok(match kw {
        Rule::add_kw | Rule::mul_kw | Rule::seq_kw | Rule::slt_kw => {
            let a = parse_labelled_read_param(pairs.next().unwrap(), meeted_labels)?;
            let b = parse_labelled_read_param(pairs.next().unwrap(), meeted_labels)?;
            let c = parse_labelled_write_param(pairs.next().unwrap(), meeted_labels)?;
            match kw {
                Rule::add_kw => ADD(a, b, c),
                Rule::mul_kw => MUL(a, b, c),
                Rule::seq_kw => SEQ(a, b, c),
                Rule::slt_kw => SLT(a, b, c),
                _ => unreachable!(),
            }
        }
        Rule::jz_kw | Rule::jnz_kw => {
            let a = parse_labelled_read_param(pairs.next().unwrap(), meeted_labels)?;
            let b = parse_labelled_read_param(pairs.next().unwrap(), meeted_labels)?;
            match kw {
                Rule::jz_kw => JZ(a, b),
                Rule::jnz_kw => JNZ(a, b),
                _ => unreachable!(),
            }
        }
        Rule::out_kw => OUT(parse_labelled_read_param(
            pairs.next().unwrap(),
            meeted_labels,
        )?),
        Rule::incb_kw => INCB(parse_labelled_read_param(
            pairs.next().unwrap(),
            meeted_labels,
        )?),
        Rule::in_kw => IN(parse_labelled_write_param(
            pairs.next().unwrap(),
            meeted_labels,
        )?),
        Rule::halt_kw => HALT,
        _ => unreachable!(),
    })
}

fn parse_directive(src: Pair<Rule>, meeted_labels: &mut HashSet<Label>) -> Result<Directive> {
    todo!()
}

fn parse_labelled_write_param(
    src: Pair<Rule>,
    meeted_labels: &mut HashSet<Label>,
) -> Result<Labelled<WriteParam>> {
    debug_assert_matches!(src.as_rule(), Rule::labelled_write_param);
    let mut src = src.into_inner();
    let labels = parse_labels(src.next().unwrap(), meeted_labels)?;
    let content = parse_write_param(src.next().unwrap())?;
    Ok(labels.map(|_| content))
}

fn parse_labelled_read_param(
    src: Pair<Rule>,
    meeted_labels: &mut HashSet<Label>,
) -> Result<Labelled<ReadParam>> {
    debug_assert_matches!(src.as_rule(), Rule::labelled_read_param);
    let mut src = src.into_inner();
    let labels = parse_labels(src.next().unwrap(), meeted_labels)?;
    let content = parse_read_param(src.next().unwrap())?;
    Ok(labels.map(|_| content))
}

fn parse_write_param(src: Pair<Rule>) -> Result<WriteParam> {
    Ok(match src.as_rule() {
        Rule::position_param => WriteParam::Position(parse_number_or_label_and_offset(
            src.into_inner().next().unwrap(),
        )?),
        Rule::relative_param => WriteParam::Relative(parse_number_maybe_signed(src)?),
        _ => unreachable!(),
    })
}

fn parse_read_param(src: Pair<Rule>) -> Result<ReadParam> {
    Ok(match src.as_rule() {
        Rule::position_param => ReadParam::Position(parse_number_or_label_and_offset(
            src.into_inner().next().unwrap(),
        )?),
        Rule::immediate_param => ReadParam::Immediate(parse_number_or_label_and_offset(
            src.into_inner().next().unwrap(),
        )?),
        Rule::relative_param => ReadParam::Relative(parse_number_maybe_signed(src)?),
        _ => unreachable!(),
    })
}

fn parse_number_or_label_and_offset(src: Pair<Rule>) -> Result<RlValue> {
    Ok(match src.as_rule() {
        Rule::number => {
            RlValue::Absolute(src.as_str().parse().map_err(ParseError::LiteralTooLong)?)
        }
        Rule::label_and_offset => {
            let mut src = src.into_inner();
            let lbl = parse_label(src.next().unwrap())?;
            let offset = match src.next().map(|v| v.as_rule()) {
                Some(r) => {
                    let num = src
                        .next()
                        .unwrap()
                        .as_str()
                        .parse::<ICValue>()
                        .map_err(ParseError::LiteralTooLong)?;
                    match r {
                        Rule::minus => num.neg(),
                        Rule::plus => num,
                        _ => unreachable!(),
                    }
                }
                None => ICValue(0),
            };
            RlValue::Reference { lbl, offset }
        }
        _ => unreachable!(),
    })
}

fn parse_number_maybe_signed(src: Pair<Rule>) -> Result<ICValue> {
    let mut src = src.into_inner();
    let a = src.next().unwrap();
    Ok(ICValue(
        match a.as_rule() {
            Rule::number => a,
            Rule::minus => src.next().unwrap(),
            _ => unreachable!(),
        }
        .as_str()
        .parse()
        .map_err(ParseError::LiteralTooLong)?,
    ))
}
