use std::io::{read_to_string, stdin};

use assembler::{
    ast::{File, ParseError},
    tokens::{tokenize, TokenizeError},
};
use codespan_reporting::{
    files::SimpleFile,
    term::{
        self,
        termcolor::{ColorChoice, StandardStream},
    },
};
use errors::{Accumulator, SourceError, Spanned};
use thiserror::Error;

#[derive(Debug, Clone, Error)]
enum Error {
    #[error(transparent)]
    TokenizeError(#[from] TokenizeError),
    #[error(transparent)]
    ParseError(#[from] ParseError),
}
impl Spanned for Error {
    fn span(&self) -> std::ops::Range<usize> {
        match self {
            Error::TokenizeError(err) => err.span(),
            Error::ParseError(err) => err.span(),
        }
    }
}
impl SourceError for Error {
    fn severity(&self) -> errors::Severity {
        errors::Severity::Error
    }
}

fn main() {
    let input = read_to_string(stdin()).expect("Cannot read stdin");
    let mut errors = Accumulator::<Error>::new();
    let tokens = tokenize(&input, &mut errors);
    let ast = File::parse(tokens.as_slice(), &mut errors);

    let file = SimpleFile::new("<stdin>", &input);
    let writer = StandardStream::stderr(ColorChoice::Always);
    let config = codespan_reporting::term::Config::default();

    for err in errors.checkpoint().err().into_iter().flatten() {
        let diagnostic = err.into_diagnostic(());
        term::emit(&mut writer.lock(), &config, &file, &diagnostic)
            .expect("Cannot print diagnostic");
    }

    println!("{ast:?}");
}
