#![feature(never_type)]
#![feature(box_into_inner)]
#![feature(box_patterns)]

pub use ast::ParseError;
use errors::Accumulator;

pub mod ast;

pub fn parse<'s, A>(source: &'s str, errors: &mut A) -> Option<ast::File<'s>>
where
    A: Accumulator,
    A::Error: From<ParseError>,
{
    ast::File::parse(source, &mut errors.as_mapped(|err: ParseError| err.into()))
}

#[cfg(test)]
#[test_sources::test_sources]
fn parse_sources(source: &str) {
    use itertools::Itertools;

    let mut errors = errors::RootAccumulator::<ParseError>::new();
    let ast = parse(source, &mut errors);

    match errors.finish_with(ast) {
        Ok(Some(_)) => (),
        Ok(None) => panic!("No error was thrown, but no ast was produced!"),
        Err(errs) => panic!("Errors in parsing:\n\n{}", errs.into_iter().format("\n")),
    }
}
