use std::io::{read_to_string, stdin};

use assembler::tokens::{tokenize, TokenizeError};
use codespan_reporting::{
    diagnostic::{Diagnostic, Label},
    files::SimpleFile,
    term::{
        self,
        termcolor::{ColorChoice, StandardStream},
    },
};
use errors::{Accumulator, SourceError, Spanned};

fn main() {
    let input = read_to_string(stdin()).expect("Cannot read stdin");
    let mut errors = Accumulator::<TokenizeError>::new();
    let tokens = tokenize(&input, &mut errors);

    let file = SimpleFile::new("<stdin>", &input);
    let writer = StandardStream::stderr(ColorChoice::Always);
    let config = codespan_reporting::term::Config::default();

    let tokens = Diagnostic::note().with_labels(
        tokens
            .as_slice()
            .tokens()
            .into_iter()
            .map(|t| {
                Label::secondary((), t.span()).with_message(match t {
                    assembler::tokens::Token::Identifier(_) => "Identifier",
                    assembler::tokens::Token::Number(_) => "Number",
                    assembler::tokens::Token::Punctuator(_) => "",
                })
            })
            .collect(),
    );
    term::emit(&mut writer.lock(), &config, &file, &tokens).expect("Cannot print diagnostic");

    for err in errors.checkpoint().err().into_iter().flatten() {
        let diagnostic = err.into_diagnostic(());
        term::emit(&mut writer.lock(), &config, &file, &diagnostic)
            .expect("Cannot print diagnostic");
    }
}
