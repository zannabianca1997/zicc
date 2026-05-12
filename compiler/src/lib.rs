#![doc = include_str!("../README.md")]

use std::cell::RefCell;

use snafu::{ResultExt, Snafu};
use string_interner::DefaultStringInterner;
use zicc_compiler_program::Program;

pub mod cli;

mod type_table;

#[derive(Debug, Snafu)]
pub enum CompileError {
    #[snafu(display("Error in filling the type table"))]
    Types { source: type_table::Error },
}

pub fn compile(
    program: Program,
    _interner: &RefCell<DefaultStringInterner>,
) -> Result<zicc_assembler_program::Program, CompileError> {
    // Step 1: filling the type table
    let mut type_table = type_table::fill(&program).context(TypesSnafu)?;

    Ok(zicc_assembler_program::Program::default())
}
