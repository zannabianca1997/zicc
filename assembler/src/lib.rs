#![feature(never_type)]
#![feature(box_into_inner)]
#![feature(box_patterns)]

use assemble::AssembleError;
use ast::ParseErrorContent;
use thiserror::Error;

pub mod assemble;
pub mod ast;
pub mod lexer;

#[derive(Debug, Error)]
pub enum Error {
    #[error(transparent)]
    Parse(#[from] ParseErrorContent),
    #[error(transparent)]
    Assemble(#[from] AssembleError),
}
