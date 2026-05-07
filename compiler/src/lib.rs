#![doc = include_str!("../README.md")]

use std::cell::RefCell;

use snafu::Snafu;
use string_interner::DefaultStringInterner;

pub mod cli;

#[derive(Debug, Snafu)]
pub enum CompileError {
    #[snafu(display("Compilation of `.ic` sources is not yet implemented"))]
    NotYetImplemented,
}

pub fn compile(
    _program: zicc_compiler_program::Program,
    _interner: &RefCell<DefaultStringInterner>,
) -> Result<zicc_assembler_program::Program, CompileError> {
    Err(CompileError::NotYetImplemented)
}
