#![feature(error_reporter)]

use std::{
    error::Report,
    io::{read_to_string, stdin},
};

use assembler::{
    assemble::Code,
    ast::{constant_folding::ConstantFolding, File},
};
use errors::Accumulator;
use itertools::Itertools;

fn main() {
    let input = read_to_string(stdin()).unwrap();
    let mut errors = Accumulator::<assembler::Error>::new();
    let ast = match File::parse(&input) {
        Ok(f) => f.constant_folding(),
        Err(err) => {
            eprintln!("{err}");
            return;
        }
    };
    let mut code = Code::new(&mut errors);
    code.push_unit(ast);
    let code = code.codegen();

    match errors.finish_with(code) {
        Ok(code) => print!("{}", code.into_iter().format(", ")),
        Err(errs) => {
            for err in errs {
                eprintln!("{}", Report::new(err))
            }
        }
    }
}
