use std::path::Path;

use ariadne::Source;
use snafu::{ResultExt, Snafu};
use string_interner::DefaultStringInterner;
use zicc_assembler::cli::AriadneErrorHandler;

#[derive(Debug, Snafu)]
pub enum AssemblePhaseError {
    #[snafu(display("IO error reading assembly file"))]
    Io { source: std::io::Error },
    #[snafu(transparent)]
    Parse { source: zicc_assembler_program::ParseError },
    #[snafu(transparent)]
    Assemble { source: zicc_assembler::AssembleError },
}

pub fn assemble(
    ica_files: &[&Path],
    compiled_asm: Vec<zicc_assembler_program::Program>,
    interner: &mut DefaultStringInterner,
) -> Result<Vec<zicc_linker_program::Program>, AssemblePhaseError> {
    let mut programs = Vec::with_capacity(ica_files.len() + compiled_asm.len());

    for path in ica_files {
        let source = std::fs::read(path).context(IoSnafu)?;
        let source_name = path.to_string_lossy().to_string();
        let source_text = String::from_utf8_lossy(&source).into_owned();
        let ariadne_source = Source::from(source_text);
        let error_handler = AriadneErrorHandler {
            source_name: &source_name,
            source: &ariadne_source,
        };
        let program =
            zicc_assembler_program::Program::parse(&source, interner, error_handler)?;
        programs.push(zicc_assembler::assemble(program)?);
    }

    for program in compiled_asm {
        programs.push(zicc_assembler::assemble(program)?);
    }

    Ok(programs)
}
