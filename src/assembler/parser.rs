//! Parse an assembly file

use std::{
    assert_matches::debug_assert_matches, collections::HashSet, num::ParseIntError, ops::Neg,
};

use pest::{iterators::Pair, Parser};
use pest_derive::Parser;
use thiserror::Error;

use crate::intcode::ICValue;

use super::{
    assembly_file::{AssemblyFile, Line},
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
    #[error("Numeric label is too long for usize")]
    NumLabelTooLong(#[source] ParseIntError),
    #[error("Numeric literal is too long for usize")]
    LiteralTooLong(#[source] ParseIntError),
}

pub type Result<T> = std::result::Result<T, ParseError>;

pub fn parse(source: &str) -> Result<AssemblyFile> {
    // parse the text
    let parsed = AssemblyParser::parse(Rule::file, source)?;
    // convert to memory rapresentation
    Ok(AssemblyFile(
        parsed
            .take_while(|line| matches!(line.as_rule(), Rule::line))
            .map(parse_line)
            .collect::<Result<_>>()?,
    ))
}

fn parse_line(src: Pair<Rule>) -> Result<Line> {
    debug_assert_matches!(src.as_rule(), Rule::line);
    let mut src = src.into_inner();
    // Read labels
    let labels = parse_labels(src.next().unwrap())?;
    // parse content
    let content = src
        .next()
        .map(|content| match content.as_rule() {
            Rule::instruction => parse_instruction(content).map(Directive::Instruction),
            Rule::directive => parse_directive(content),
            _ => unreachable!(),
        })
        .transpose()?;

    Ok(labels.map(|_| content))
}

fn parse_labels(src: Pair<Rule>) -> Result<Labelled<()>> {
    debug_assert_matches!(src.as_rule(), Rule::labels);
    // parse labels
    let mut lbls = HashSet::new();
    for lbl in src.into_inner() {
        lbls.insert(parse_label(lbl)?);
    }
    Ok(Labelled { inner: (), lbls })
}

fn parse_label(src: Pair<Rule>) -> Result<Label> {
    Ok(match src.as_rule() {
        Rule::numeric => Label::Numeric(
            src.as_str()[1..]
                .parse()
                .map_err(ParseError::NumLabelTooLong)?,
        ),
        Rule::local_identifier => Label::Local(src.as_str()[1..].parse().unwrap()),
        Rule::identifier => Label::Global(src.as_str().parse().unwrap()),
        _ => unreachable!(),
    })
}

fn parse_instruction(src: Pair<Rule>) -> Result<Instruction> {
    debug_assert_matches!(src.as_rule(), Rule::instruction);
    // split by type of arguments
    use Instruction::*;
    let mut pairs = src.into_inner();
    let kw = pairs.next().unwrap().as_rule();
    Ok(match kw {
        Rule::add_kw | Rule::mul_kw | Rule::seq_kw | Rule::slt_kw => {
            let a = parse_labelled_read_param(pairs.next().unwrap())?;
            let b = parse_labelled_read_param(pairs.next().unwrap())?;
            let c = parse_labelled_write_param(pairs.next().unwrap())?;
            match kw {
                Rule::add_kw => ADD(a, b, c),
                Rule::mul_kw => MUL(a, b, c),
                Rule::seq_kw => SEQ(a, b, c),
                Rule::slt_kw => SLT(a, b, c),
                _ => unreachable!(),
            }
        }
        Rule::jz_kw | Rule::jnz_kw => {
            let a = parse_labelled_read_param(pairs.next().unwrap())?;
            let b = parse_labelled_read_param(pairs.next().unwrap())?;
            match kw {
                Rule::jz_kw => JZ(a, b),
                Rule::jnz_kw => JNZ(a, b),
                _ => unreachable!(),
            }
        }
        Rule::out_kw => OUT(parse_labelled_read_param(pairs.next().unwrap())?),
        Rule::incb_kw => INCB(parse_labelled_read_param(pairs.next().unwrap())?),
        Rule::in_kw => IN(parse_labelled_write_param(pairs.next().unwrap())?),
        Rule::halt_kw => HALT,
        _ => unreachable!(),
    })
}

fn parse_directive(src: Pair<Rule>) -> Result<Directive> {
    debug_assert_matches!(src.as_rule(), Rule::directive);
    use Directive::*;
    let mut pairs = src.into_inner();
    let kw = pairs.next().unwrap().as_rule();
    Ok(match kw {
        Rule::data_kw => DATA(
            pairs
                .map(parse_labelled_signed_value)
                .collect::<Result<_>>()?,
        ),
        Rule::zeros_kw => ZEROS(
            pairs
                .next()
                .unwrap()
                .as_str()
                .parse()
                .map_err(ParseError::LiteralTooLong)?,
        ),
        Rule::jmp_kw => JMP(parse_labelled_read_param(pairs.next().unwrap())?),
        Rule::mov_kw => {
            let a = pairs.next().unwrap();
            let b = pairs.next().unwrap();
            if let Some(len) = pairs.next() {
                let a = parse_read_param(a)?;
                let b = parse_write_param(b)?;
                let len = len.as_str().parse().map_err(ParseError::LiteralTooLong)?;
                debug_assert_ne!(
                    len, 1,
                    "`mov` len should not be produced as token if equal to 1"
                );
                MOVM(a, b, len)
            } else {
                let a = parse_labelled_read_param(a)?;
                let b = parse_labelled_write_param(b)?;
                MOV(a, b)
            }
        }
        Rule::load_kw => {
            let a = pairs.next().unwrap();
            let b = pairs.next().unwrap();
            if let Some(len) = pairs.next() {
                let a = parse_read_param(a)?;
                let b = parse_write_param(b)?;
                let len = len.as_str().parse().map_err(ParseError::LiteralTooLong)?;
                debug_assert_ne!(
                    len, 1,
                    "`load` len should not be produced as token if equal to 1"
                );
                LOADM(a, b, len)
            } else {
                let a = parse_labelled_read_param(a)?;
                let b = parse_labelled_write_param(b)?;
                LOAD(a, b)
            }
        }
        Rule::store_kw => {
            let a = pairs.next().unwrap();
            let b = pairs.next().unwrap();
            if let Some(len) = pairs.next() {
                let a = parse_read_param(a)?;
                let b = parse_read_param(b)?;
                let len = len.as_str().parse().map_err(ParseError::LiteralTooLong)?;
                debug_assert_ne!(
                    len, 1,
                    "`store` len should not be produced as token if equal to 1"
                );
                STOREM(a, b, len)
            } else {
                let a = parse_labelled_read_param(a)?;
                let b = parse_labelled_read_param(b)?;
                STORE(a, b)
            }
        }
        Rule::push_kw => {
            let a = pairs.next().unwrap();
            if let Some(len) = pairs.next() {
                let a = parse_read_param(a)?;
                let len = len.as_str().parse().map_err(ParseError::LiteralTooLong)?;
                debug_assert_ne!(
                    len, 1,
                    "`push` len should not be produced as token if equal to 1"
                );
                PUSHM(a, len)
            } else {
                let a = parse_labelled_read_param(a)?;
                PUSH(a)
            }
        }
        Rule::pop_kw => {
            let a = pairs.next().unwrap();
            if let Some(len) = pairs.next() {
                let a = parse_write_param(a)?;
                let len = len.as_str().parse().map_err(ParseError::LiteralTooLong)?;
                debug_assert_ne!(
                    len, 1,
                    "`push` len should not be produced as token if equal to 1"
                );
                POPM(a, len)
            } else {
                let a = parse_labelled_write_param(a)?;
                POP(a)
            }
        }
        Rule::call_kw => CALL(parse_labelled_read_param(pairs.next().unwrap())?),
        Rule::ret_kw => RET,
        _ => unreachable!(),
    })
}

fn parse_labelled_signed_value(src: Pair<Rule>) -> Result<Labelled<RlValue>> {
    debug_assert_matches!(src.as_rule(), Rule::labelled_signed_value);
    let mut src = src.into_inner();
    let labels = parse_labels(src.next().unwrap())?;
    let content = parse_signed_number_or_label_and_offset(src.next().unwrap())?;
    Ok(labels.map(|_| content))
}

fn parse_labelled_write_param(src: Pair<Rule>) -> Result<Labelled<WriteParam>> {
    debug_assert_matches!(src.as_rule(), Rule::labelled_write_param);
    let mut src = src.into_inner();
    let labels = parse_labels(src.next().unwrap())?;
    let content = parse_write_param(src.next().unwrap())?;
    Ok(labels.map(|_| content))
}

fn parse_labelled_read_param(src: Pair<Rule>) -> Result<Labelled<ReadParam>> {
    debug_assert_matches!(src.as_rule(), Rule::labelled_read_param);
    let mut src = src.into_inner();
    let labels = parse_labels(src.next().unwrap())?;
    let content = parse_read_param(src.next().unwrap())?;
    Ok(labels.map(|_| content))
}

fn parse_write_param(src: Pair<Rule>) -> Result<WriteParam> {
    Ok(match src.as_rule() {
        Rule::position_param => WriteParam::Position(parse_number_or_label_and_offset(
            src.into_inner().next().unwrap(),
        )?),
        Rule::relative_param => {
            WriteParam::Relative(parse_signed_number(src.into_inner().next().unwrap())?)
        }
        _ => unreachable!(),
    })
}

fn parse_read_param(src: Pair<Rule>) -> Result<ReadParam> {
    Ok(match src.as_rule() {
        Rule::position_param => ReadParam::Position(parse_number_or_label_and_offset(
            src.into_inner().next().unwrap(),
        )?),
        Rule::immediate_param => ReadParam::Immediate(parse_signed_number_or_label_and_offset(
            src.into_inner().next().unwrap(),
        )?),
        Rule::relative_param => {
            ReadParam::Relative(parse_signed_number(src.into_inner().next().unwrap())?)
        }
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

fn parse_signed_number_or_label_and_offset(src: Pair<Rule>) -> Result<RlValue> {
    Ok(match src.as_rule() {
        Rule::signed_number => {
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

fn parse_signed_number(src: Pair<Rule>) -> Result<ICValue> {
    debug_assert_matches!(src.as_rule(), Rule::signed_number);
    Ok(ICValue(
        src.as_str().parse().map_err(ParseError::LiteralTooLong)?,
    ))
}
