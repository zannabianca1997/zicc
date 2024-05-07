use std::{
    cell::RefCell,
    io::{stdin, Read},
    rc::Rc,
};

use errors::{Accumulator, RootAccumulator};
use peg::error::ParseError;
use string_interner::StringInterner;
use thiserror::Error;

#[derive(Debug, Error)]
enum Error {
    #[error(transparent)]
    LexError(#[from] compiler::ast::tokens::LexError),
    #[error(transparent)]
    ParseError(#[from] ParseError<usize>),
}

fn main() {
    let mut source = String::new();
    stdin().read_to_string(&mut source).unwrap();
    let source = source;

    let mut errors = RootAccumulator::new();

    let tokens = compiler::ast::tokens::lex(
        &source,
        None,
        errors.as_mapped(Error::LexError),
        Rc::new(RefCell::new(StringInterner::new())),
    );

    let ast = errors.handle(compiler::ast::parser::file(&tokens));

    match errors.finish(|| ast.unwrap()) {
        Ok(ast) => {
            println!("{ast:#?}")
        }
        Err(errs) => {
            for err in errs {
                eprintln!("{err}")
            }
        }
    }
}
