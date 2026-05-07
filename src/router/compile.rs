use std::cell::RefCell;
use std::path::Path;

use ariadne::Source;
use snafu::{ResultExt, Snafu};
use string_interner::DefaultStringInterner;
use zicc_compiler::cli::AriadneErrorHandler;
use zicc_compiler_program::ParseError;

#[derive(Debug, Snafu)]
pub enum CompilePhaseError {
    #[snafu(display("IO error reading source file"))]
    Io { source: std::io::Error },
    #[snafu(transparent)]
    Parse { source: ParseError },
    #[snafu(transparent)]
    Compile { source: zicc_compiler::CompileError },
}

pub fn compile(
    files: &[&Path],
    interner: &RefCell<DefaultStringInterner>,
) -> Result<Vec<zicc_assembler_program::Program>, CompilePhaseError> {
    let mut results = Vec::with_capacity(files.len());

    for file in files {
        let source = std::fs::read(file).context(IoSnafu)?;
        let source_name = file.to_string_lossy().to_string();
        let source_text = String::from_utf8_lossy(&source).into_owned();
        let ariadne_source = Source::from(source_text);
        let error_handler = AriadneErrorHandler {
            source_name: &source_name,
            source: &ariadne_source,
        };

        let program = zicc_compiler_program::Program::parse(&source, interner, error_handler)?;
        results.push(zicc_compiler::compile(program, interner)?);
    }

    Ok(results)
}
