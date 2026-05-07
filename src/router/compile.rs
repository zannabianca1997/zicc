use std::cell::RefCell;
use std::path::Path;

use snafu::{ResultExt, Snafu};
use string_interner::DefaultStringInterner;

#[derive(Debug, Snafu)]
pub enum CompileError {
    #[snafu(display("IO error reading source file"))]
    Io { source: std::io::Error },
    #[snafu(display("Parse error in source file: {details}"))]
    Parse { details: String },
    #[snafu(display("Compilation of `.ic` sources is not yet implemented"))]
    NotYetImplemented,
}

pub fn compile(
    files: &[&Path],
    interner: &RefCell<DefaultStringInterner>,
) -> Result<Vec<zicc_assembler_program::Program>, CompileError> {
    let results = Vec::with_capacity(files.len());

    for file in files {
        let source = std::fs::read_to_string(file).context(IoSnafu)?;

        let _ast = zicc_compiler_parser::parse(&source, interner)
            .into_result()
            .map_err(|errors| CompileError::Parse {
                details: errors
                    .iter()
                    .map(|e| format!("{:?}", e))
                    .collect::<Vec<_>>()
                    .join("\n"),
            })?;

        return Err(CompileError::NotYetImplemented);
    }

    Ok(results)
}
