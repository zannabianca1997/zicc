use std::cell::RefCell;
use std::ops::Range;
use std::path::Path;

use ariadne::{Color, Label, Report, ReportKind, Source};
use snafu::{ResultExt, Snafu};
use string_interner::DefaultStringInterner;
use zicc_compiler_program::{ErrorHandler, ParseError};

#[derive(Debug, Snafu)]
pub enum CompileError {
    #[snafu(display("IO error reading source file"))]
    Io { source: std::io::Error },
    #[snafu(transparent)]
    Parse { source: ParseError },
    #[snafu(display("Compilation of `.ic` sources is not yet implemented"))]
    NotYetImplemented,
}

struct AriadneErrorHandler<'a> {
    source_name: &'a str,
    source: &'a Source,
}

impl<'a> ErrorHandler for AriadneErrorHandler<'a> {
    fn handle(
        &mut self,
        err: zicc_compiler_program::ParserError<'_>,
        span_mapper: impl Fn(Range<usize>) -> Range<usize>,
    ) {
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

pub fn compile(
    files: &[&Path],
    interner: &RefCell<DefaultStringInterner>,
) -> Result<Vec<zicc_assembler_program::Program>, CompileError> {
    let results = Vec::with_capacity(files.len());

    for file in files {
        let source = std::fs::read(file).context(IoSnafu)?;
        let source_name = file.to_string_lossy().to_string();
        let source_text = String::from_utf8_lossy(&source).into_owned();
        let ariadne_source = Source::from(source_text);
        let error_handler = AriadneErrorHandler {
            source_name: &source_name,
            source: &ariadne_source,
        };

        let _ast = zicc_compiler_program::Program::parse(&source, interner, error_handler)?;

        return Err(CompileError::NotYetImplemented);
    }

    Ok(results)
}
