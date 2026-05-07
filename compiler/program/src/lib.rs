use std::cell::RefCell;
use std::ops::Range;
use std::str::Utf8Error;

use snafu::Snafu;
use string_interner::DefaultStringInterner;

pub use zicc_compiler_parser::ParserError;

pub struct Program {
    pub content: zicc_compiler_ast::File,
}

impl Program {
    pub fn parse<'s>(
        source: &'s [u8],
        interner: &'s RefCell<DefaultStringInterner>,
        mut error_handler: impl ErrorHandler,
    ) -> Result<Self, ParseError> {
        let source = str::from_utf8(source)?;
        match zicc_compiler_parser::parse(source, interner).into_result() {
            Ok(ast) => Ok(Self { content: ast }),
            Err(errors) => {
                for err in errors {
                    error_handler.handle(err, |span| span);
                }
                Err(ParseError::ParsingFailed)
            }
        }
    }
}

pub trait ErrorHandler {
    fn handle(&mut self, err: ParserError<'_>, span_mapper: impl Fn(Range<usize>) -> Range<usize>);
}

#[derive(Debug, Snafu)]
pub enum ParseError {
    #[snafu(transparent)]
    NotUtf8 {
        source: Utf8Error,
    },
    ParsingFailed,
}
