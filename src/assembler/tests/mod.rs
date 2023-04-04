//! Test for the assembler

use std::{assert_matches::assert_matches, collections::HashSet, error::Report, iter::repeat};

use serde::{Deserialize, Serialize};

use crate::{
    assembler::{
        assembly_file::AssembleError, directive::ExpandError,
        instruction::GenerateInstructionError, relocatable::AppendError,
    },
    intcode::ICValue,
    machine::{ICMachine, ICMachineData},
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
    ParseErr {
        parse_err: ParseErrorSpec,
    },
    AssembleErr {
        assemble_err: AssembleErrorSpec,
    },
    Assembled {
        io: Option<Box<[IOExample]>>,
        assembled: Option<Box<[ICValue]>>,
    },
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

fn test_parse(name: &str, TestCase { src, result }: &TestCase) {
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

fn test_assemble(name: &str, TestCase { src, result }: &TestCase) {
    if matches!(result, AssemblyTestResult::ParseErr { .. }) {
        return; // skip sources that should not parse
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

fn test_emit(name: &str, TestCase { src, result }: &TestCase) {
    if let AssemblyTestResult::Assembled { .. } = result {
        let obtained = parse(src)
            .expect("Source not tagged as parse_error should parse")
            .assemble()
            .expect("Source not tagged as assemble_error should assemble");
        // emit the file
        assert!(
            obtained.is_free(),
            "Test {name} assembled in a incomplete fragment: {:?} are free labels",
            obtained.referenced_labels().collect::<HashSet<_>>()
        )
    }
}

// check that display is ok, and round trips
fn test_display(_name: &str, TestCase { src, .. }: &TestCase) {
    if let Ok(a) = parse(src) {
        let displayed = a.to_string();
        let b = parse(&displayed);
        match b {
            Ok(b) => assert_eq!(
                a.assemble(),
                b.assemble(),
                "Reparsed display should assemble to the same fragment"
            ),
            Err(err) => panic!(
                "Display failed to parse: {}",
                Report::from(err).pretty(true)
            ),
        }
    }
}

fn test_io(name: &str, TestCase { src, result }: &TestCase) {
    if let AssemblyTestResult::Assembled { io: Some(io), .. } = result {
        let program = parse(src)
            .expect("Source not tagged as parse_error should parse")
            .assemble()
            .expect("Source not tagged as assemble_error should assemble")
            .emit()
            .expect("Sources should be a complete program");
        let program: Box<[_]> = program.into_iter().map(|v| v.0 as _).collect();
        for (n, IOExample { inp, out }) in io.iter().enumerate() {
            let mut machine = ICMachineData::new(program.as_ref());
            for i in inp.iter() {
                machine
                    .give_input(*i)
                    .expect("Reference intcode implementation input should be infallible")
            }
            use crate::machine::ICMachineStopState::*;
            match machine.run() {
                EmptyInput => {
                    panic!("Machine for test {name} was not satisfied with the input {n}")
                }
                RuntimeErr(err) => {
                    panic!("Machine for test {name} with input {n} crashed at runtime: {}\n\nComplete parsed program:\n{}", err,parse(src).unwrap())
                }
                Halted => {
                    // collect output
                    let obtained = repeat(()).scan((), |(), ()| machine.get_output()).collect();
                    assert_eq!(
                        out, &obtained,
                        "Output of machine {name} with input {n} was not matching"
                    )
                }
            }
        }
    }
}

fn test_assembled(name: &str, TestCase { src, result }: &TestCase) {
    if let AssemblyTestResult::Assembled {
        assembled: Some(assembled),
        ..
    } = result
    {
        let program = parse(src)
            .expect("Source not tagged as parse_error should parse")
            .assemble()
            .expect("Source not tagged as assemble_error should assemble")
            .emit()
            .expect("Sources should be a complete program");
        assert_eq!(
            program.as_ref(),
            assembled.as_ref(),
            "Program {name} did not compile to the given code"
        )
    }
}

macro_rules! testcase {
    ($($name:ident => $path:literal,)*) => {
        $(
        mod $name {
            use super::{test_assemble, test_assembled, test_display, test_emit, test_io, test_parse, TestCase};
            use lazy_static::lazy_static;

            lazy_static! {
                static ref CASE: TestCase = serde_yaml::from_str(include_str!($path))
                    .expect(format!("Source {} should deserialize", $path).as_str());
            }

            #[test]
            fn parse() {
                test_parse(stringify!($name), &CASE)
            }
            #[test]
            fn assemble() {
                test_assemble(stringify!($name), &CASE)
            }
            #[test]
            fn display() {
                test_display(stringify!($name), &CASE)
            }
            #[test]
            fn emit() {
                test_emit(stringify!($name), &CASE)
            }
            #[test]
            fn io() {
                test_io(stringify!($name), &CASE)
            }
            #[test]
            fn assembled() {
                test_assembled(stringify!($name), &CASE)
            }
        }
    )*
    };
}

testcase! {
    out => "sources/out.yaml",
    in_out=> "sources/in_out.yaml",
    jz=> "sources/jz.yaml",
    cat=> "sources/cat.yaml",
    add=> "sources/add.yaml",
    mul=> "sources/mul.yaml",
    slt=> "sources/slt.yaml",
    seq=> "sources/seq.yaml",
    incb=> "sources/incb.yaml",
    halt=> "sources/halt.yaml",
    data=> "sources/data.yaml",
    data_2=> "sources/data_2.yaml",
    zeros=> "sources/zeros.yaml",
    jmp=> "sources/jmp.yaml",
    offsets=> "sources/offsets.yaml",
}
