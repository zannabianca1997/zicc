use std::path::Path;

use snafu::{ResultExt, Snafu};
use string_interner::DefaultStringInterner;

#[derive(Debug, Snafu)]
pub enum LinkPhaseError {
    #[snafu(display("IO error reading object file"))]
    Io { source: std::io::Error },
    #[snafu(transparent)]
    Parse {
        source: zicc_linker_program::ParseError,
    },
    #[snafu(transparent)]
    Link { source: zicc_linker::LinkError },
    #[snafu(transparent)]
    MakeExec {
        source: zicc_linker::MakeExecutableError,
    },
}

pub fn load_and_link(
    icob_files: &[&Path],
    mut assembled: Vec<zicc_linker_program::Program>,
    interner: &mut DefaultStringInterner,
) -> Result<zicc_linker_program::Program, LinkPhaseError> {
    let mut units = Vec::with_capacity(icob_files.len() + assembled.len());

    for path in icob_files {
        let source = std::fs::read(path).context(IoSnafu)?;
        let program = zicc_linker_program::Program::parse(&source, interner)?;
        units.push(program);
    }

    units.append(&mut assembled);

    Ok(zicc_linker::link(units, interner)?)
}

pub fn make_exec(
    linked: zicc_linker_program::Program,
    interner: &mut DefaultStringInterner,
) -> Result<zicc_vm_program::Program, LinkPhaseError> {
    Ok(zicc_linker::make_executable(linked, interner)?)
}
