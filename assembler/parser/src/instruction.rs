use chumsky::{Parser, prelude::todo};
use zicc_assembler_ast::instruction::Instruction;

use crate::ParserExtra;

/// A single instruction
pub(crate) fn instruction<'s>() -> impl Parser<'s, &'s str, Instruction, ParserExtra<'s>> {
    todo()
}
