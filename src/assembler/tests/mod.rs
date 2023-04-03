//! Test for the assembler

use std::{assert_matches::assert_matches, collections::HashMap, error::Report};

use lazy_static::lazy_static;
use serde::{Deserialize, Serialize};

use crate::assembler::{
    assembly_file::AssembleError, directive::ExpandError, instruction::GenerateInstructionError,
    parser::ParseError, relocatable::AppendError,
};

use super::parser::parse;

#[derive(Debug, Serialize, Deserialize)]
struct TestCase {
    src: String,
    #[serde(flatten)]
    result: AssemblyTestResult,
}

#[derive(Debug, Serialize, Deserialize)]
#[serde(untagged)]
enum AssemblyTestResult {
    ParseErr { parse_err: ParseErrorSpec },
    AssembleErr { assemble_err: AssembleErrorSpec },
    Assembled { io: Box<[IOExample]> },
}

#[derive(Debug, Serialize, Deserialize)]
enum ParseErrorSpec {}
#[derive(Debug, Serialize, Deserialize)]
enum AssembleErrorSpec {
    RedefinitedLabel,
}

#[derive(Debug, Serialize, Deserialize)]
struct IOExample {
    inp: Box<[i64]>,
    out: Box<[i64]>,
}

lazy_static! {
    static ref CASES: HashMap<String, TestCase> =
        serde_yaml::from_str(include_str!("sources.yaml"))
            .expect("`sources.yaml` must be deserializable");
}

#[test]
fn test_parse() {
    for (name, TestCase { src, result }) in CASES.iter() {
        let obtained = parse(src);
        match result {
            AssemblyTestResult::ParseErr { parse_err } => {
                assert!(obtained.is_err(), "Test {name:?} shouldn't parse, but did.");
                match *parse_err {}
            }
            AssemblyTestResult::AssembleErr { .. } | AssemblyTestResult::Assembled { .. } => {
                assert!(
                    obtained.is_ok(),
                    "Test {name:?} should parse, but did not: {}",
                    Report::new(obtained.unwrap_err()).pretty(true)
                )
            }
        }
    }
}

#[test]
fn test_assemble() {
    for (name, TestCase { src, result }) in CASES.iter() {
        if matches!(result, AssemblyTestResult::ParseErr { .. }) {
            continue; // skip sources that should not parse
        }
        let obtained = parse(src)
            .expect("Source not tagged as parse_error should parse")
            .assemble();
        match result {
            AssemblyTestResult::ParseErr { .. } => unreachable!(),
            AssemblyTestResult::AssembleErr { assemble_err } => {
                assert!(
                    obtained.is_err(),
                    "Test {name:?} shouldn't assemble, but did."
                );
                match assemble_err {
                    AssembleErrorSpec::RedefinitedLabel => {
                        assert_matches!(
                            obtained.unwrap_err(),
                            AssembleError::Expand(ExpandError::GenerateInstruction(
                                GenerateInstructionError(AppendError::DuplicateLabelDefError(_))
                            )) | AssembleError::Append(AppendError::DuplicateLabelDefError(_))
                        )
                    }
                }
            }
            AssemblyTestResult::Assembled { .. } => {
                assert!(
                    obtained.is_ok(),
                    "Test {name:?} should assemble, but did not: {}",
                    Report::new(obtained.unwrap_err()).pretty(true)
                )
            }
        }
    }
}
