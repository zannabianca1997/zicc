#![feature(error_reporter)]

use std::{
    error::Report,
    io::{read_to_string, stdin},
};

use assembler::{
    assemble::Code,
    ast::{AstNode, File, ParseError},
};
use errors::{Accumulator, RootAccumulator};
use itertools::Itertools;

fn main() {
    let input = read_to_string(stdin()).unwrap();
    let mut errors = RootAccumulator::<assembler::Error>::new();
    let ast = File::parse(&input, &mut errors.as_mapped(|err: ParseError| err.into()));
    if let Err(errs) = errors.checkpoint() {
        for err in errs {
            eprintln!("{}", err)
        }
        return;
    }
    let ast = ast.unwrap();

    let mut code = Code::new(&mut errors);
    code.push_unit(ast);
    let code = code.codegen();

    match errors.finish_with(code) {
        Ok(code) => println!("{}", code.into_iter().format(", ")),
        Err(errs) => {
            for err in errs {
                eprintln!("{}", Report::new(err))
            }
        }
    }
}
