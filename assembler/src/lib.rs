#![feature(never_type)]
#![feature(box_into_inner)]
#![feature(box_patterns)]
#![feature(type_alias_impl_trait)]

use assemble::AssembleError;
use ast::ParseError;
use thiserror::Error;

pub mod assemble;
pub mod ast;
pub mod lexer;

#[derive(Debug, Error)]
pub enum Error {
    #[error(transparent)]
    Parse(#[from] ParseError),
    #[error(transparent)]
    Assemble(#[from] AssembleError),
}

#[cfg(test)]
mod test {
    use super::{
        assemble::{AssembleError, Code},
        ast::{File, ParseError},
        lexer::Lexer,
    };
    use errors::RootAccumulator;
    use itertools::Itertools;
    use vm::{ICMachine, ICMachineData, VMInt};

    struct Source {
        descr: Option<&'static str>,
        source: &'static str,
    }

    struct Test {
        descr: Option<&'static str>,
        r#in: &'static [VMInt],
        out: &'static [VMInt],
    }

    fn test_lex(source: &'static Source) {
        for token in Lexer::new(source.source) {
            if let Err(err) = token {
                panic!("Error in tokenizing: {err}")
            }
        }
    }
    fn test_parse(source: &'static Source) {
        let mut errors = RootAccumulator::<ParseError>::new();
        let ast = File::parse(source.source, &mut errors);
        match errors.finish_with(ast) {
            Ok(Some(_)) => (),
            Ok(None) => panic!("No errors where thrown, but no ast was produced"),
            Err(errs) => {
                panic!("Errors in parsing:\n\n{}", errs.into_iter().format("\n"))
            }
        }
    }
    fn test_assemble(source: &'static Source) {
        let mut errors = RootAccumulator::<ParseError>::new();
        let ast = File::parse(source.source, &mut errors);
        errors.checkpoint().unwrap();
        let ast = ast.unwrap();
        let mut errors = RootAccumulator::<AssembleError>::new();
        let mut code = Code::new(&mut errors);
        code.push_unit(ast);
        let _ = code.codegen();
        if let Err(errs) = errors.checkpoint() {
            panic!("Errors in parsing:\n\n{}", errs.into_iter().format("\n"))
        }
    }
    fn test_io(
        source: &'static Source,
        Test {
            descr: _,
            r#in,
            out: expected,
        }: &'static Test,
    ) {
        let mut errors = RootAccumulator::<ParseError>::new();
        let ast = File::parse(source.source, &mut errors);
        errors.checkpoint().unwrap();
        let ast = ast.unwrap();
        let mut errors = RootAccumulator::<AssembleError>::new();
        let mut code = Code::new(&mut errors);
        code.push_unit(ast);
        let code = code.codegen();
        errors.checkpoint().unwrap();
        dbg!(&code);

        let mut vm = ICMachineData::new(&code);
        for &input in r#in.iter() {
            vm.give_input(input).unwrap()
        }
        match vm.run() {
            vm::ICMachineStopState::EmptyInput => {
                panic!("The program was not satisfied with the input given")
            }
            vm::ICMachineStopState::RuntimeErr(err) => panic!("The program stopped with: {err}"),
            vm::ICMachineStopState::Halted => {
                struct IterOutput<'vm, VM>(&'vm mut VM);
                impl<VM: ICMachine> Iterator for IterOutput<'_, VM> {
                    type Item = VM::IntType;

                    fn next(&mut self) -> Option<Self::Item> {
                        self.0.get_output()
                    }
                }
                let out = IterOutput(&mut vm).collect_vec();
                assert_eq!(&out, expected, "The output does not corrispond")
            }
        }
    }

    include! {env!("EXAMPLES")}
}
