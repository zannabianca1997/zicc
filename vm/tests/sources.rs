//! Test sources from `sources/ints`

use either::Either::{self, Left, Right};
use serde::Deserialize;
use zicc_limits::Value;
use zicc_vm::Vm;
use zicc_vm_program::Program;

fn default_stream() -> Either<Vec<Value>, String> {
    Left(vec![])
}

/// A test for this source
#[derive(Debug, Deserialize)]
struct TestCase {
    #[serde(default = "default_stream", with = "either::serde_untagged")]
    input: Either<Vec<Value>, String>,
    #[serde(default = "default_stream", with = "either::serde_untagged")]
    output: Either<Vec<Value>, String>,
}

fn test_harness(source: &'static [u8], name: &'static str) {
    let program = Program::parse(source).unwrap();
    let TestCase {
        input,
        output: expected,
    } = program.info.metadata["tests"][name]
        .clone()
        .try_into()
        .unwrap();

    let input = flatten_string_streams(input);
    let expected = flatten_string_streams(expected);

    let mut vm = Vm::new(program);

    let mut output = Vec::with_capacity(expected.len());

    vm.drive(&mut &*input, &mut output)
        .expect("The source should run without errors");

    debug_assert_eq!(
        unflatten_string_streams(expected),
        unflatten_string_streams(output),
        "The program should give the same output"
    )
}

fn flatten_string_streams(s: Either<Vec<Value>, String>) -> Vec<Value> {
    match s {
        Left(s) => s,
        Right(s) => s.chars().map(|ch| (ch as u32).into()).collect(),
    }
}

fn unflatten_string_streams(s: Vec<Value>) -> Either<Vec<Value>, String> {
    let mut string = String::with_capacity(s.len());
    for ch in &s {
        let Some(ch) = u32::try_from(ch).ok().and_then(char::from_u32) else {
            return Left(s);
        };
        string.push(ch);
    }
    Right(string)
}

mod tests {
    include! {env!("ZICC_INTS_TEST_SOURCES")}
}
