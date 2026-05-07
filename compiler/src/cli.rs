//! Cli harness to the compiler

use std::cell::RefCell;
use std::io::{self, Read as _};
use std::ops::Range;

use ariadne::{Color, Label, Report, ReportKind, Source};
use clap::Parser;
use clap_stdin::{FileOrStdin, FileOrStdout, StdinError};
use snafu::{ResultExt, Snafu};
use string_interner::DefaultStringInterner;
use zicc_compiler_program::{ErrorHandler, ParseError, ParserError};

use crate::CompileError;

/// Compile IntCode
///
/// Compile a IntCode source file into IntCode Assembly
#[derive(Debug, Clone, Parser)]
#[clap(version)]
pub struct Cli {
    /// Input file to compile, or `-` if reading from stdin
    #[clap(default_value = "-")]
    pub input: FileOrStdin,

    /// Output file, or `-` if writing to stdout
    #[clap(default_value = "-")]
    pub output: FileOrStdout,
}

#[derive(Debug, Snafu)]
pub enum Error {
    Input {
        source: StdinError,
    },
    Output {
        source: io::Error,
    },
    #[snafu(transparent)]
    Parse {
        source: ParseError,
    },
    #[snafu(transparent)]
    Compile {
        source: CompileError,
    },
}

pub struct AriadneErrorHandler<'a> {
    pub source_name: &'a str,
    pub source: &'a Source,
}

impl<'a> ErrorHandler for AriadneErrorHandler<'a> {
    fn handle(&mut self, err: ParserError<'_>, span_mapper: impl Fn(Range<usize>) -> Range<usize>) {
        let span = span_mapper(err.span().clone().into());
        let msg = format!("{:?}", err.reason());
        Report::build(ReportKind::Error, (self.source_name, span.start..span.end))
            .with_message(format!("Parse error in {}", self.source_name))
            .with_label(
                Label::new((self.source_name, span.start..span.end))
                    .with_message(msg)
                    .with_color(Color::Red),
            )
            .finish()
            .print((self.source_name, self.source))
            .unwrap();
    }
}

pub fn main(Cli { input, output }: Cli) -> Result<(), Error> {
    let interner = RefCell::new(DefaultStringInterner::new());

    let source_name = input.filename().to_string();

    let program = {
        let mut buf = Vec::new();
        input
            .into_reader()
            .context(InputSnafu)?
            .read_to_end(&mut buf)
            .map_err(StdinError::from)
            .context(InputSnafu)?;

        let source_text = String::from_utf8_lossy(&buf).into_owned();
        let ariadne_source = Source::from(source_text);
        let error_handler = AriadneErrorHandler {
            source_name: &source_name,
            source: &ariadne_source,
        };

        zicc_compiler_program::Program::parse(&buf, &interner, error_handler)?
    };

    let compiled = crate::compile(program, &interner)?;

    let mut writer = output.into_writer().context(OutputSnafu)?;
    compiled
        .dump(&mut writer, &interner.borrow())
        .context(OutputSnafu)?;

    Ok(())
}
