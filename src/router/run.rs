use std::fs;
use std::io;
use std::path::Path;

use clap_stdin::FileOrStdout;
use snafu::{ResultExt, Snafu};
use string_interner::DefaultStringInterner;
use zicc_vm::Vm;
use zicc_vm_stream::std_io::{IoReader, IoWriter};

#[derive(Debug, Snafu)]
pub enum RunError {
    #[snafu(display("IO error"))]
    Io { source: io::Error },
    #[snafu(transparent)]
    VmParse { source: zicc_vm_program::ParseError },
    #[snafu(transparent)]
    VmDrive {
        source: zicc_vm::DriveError<zicc_vm_stream::std_io::Error, zicc_vm_stream::std_io::Error>,
    },
}

pub fn write_assembly(
    program: &zicc_assembler_program::Program,
    output: FileOrStdout,
    interner: &DefaultStringInterner,
) -> Result<(), RunError> {
    let mut dest = output.into_writer().context(IoSnafu)?;
    program.dump(&mut dest, interner).context(IoSnafu)
}

pub fn write_object(
    program: &zicc_linker_program::Program,
    output: FileOrStdout,
    interner: &DefaultStringInterner,
) -> Result<(), RunError> {
    let mut dest = output.into_writer().context(IoSnafu)?;
    program.dump(&mut dest, interner).context(IoSnafu)
}

pub fn write_executable(
    program: &zicc_vm_program::Program,
    output: FileOrStdout,
) -> Result<(), RunError> {
    let mut dest = output.into_writer().context(IoSnafu)?;
    program.dump(&mut dest).context(IoSnafu)
}

pub fn run(program: zicc_vm_program::Program) -> Result<(), RunError> {
    let input = IoReader::new(program.info.input, io::stdin().lock());
    let output = IoWriter::new(program.info.output, io::stdout().lock());
    let mut vm = Vm::new(program);
    Ok(vm.drive(input, output)?)
}

pub fn run_file(path: &Path) -> Result<(), RunError> {
    let source = fs::read(path).context(IoSnafu)?;
    let program = zicc_vm_program::Program::parse(&source)?;
    run(program)
}
