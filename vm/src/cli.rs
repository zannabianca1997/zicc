//! Cli harness to the vm
use std::{fs, io, path::PathBuf};

use clap::Parser;
use clap_stdin::{FileOrStdin, FileOrStdout, StdinError};
use snafu::{ResultExt, Snafu};

use crate::{
    DriveError, Vm,
    program::Program,
    stream::{Format, Reader, Writer},
};

/// Intcode virtual machine
///
/// Execute a program in IntCode. Input and output can be specified in program
/// frontmatter.
#[derive(Debug, Clone, Parser)]
#[clap(version, about, long_about)]
pub struct Cli {
    /// Program to execute
    pub program: PathBuf,

    /// Input file, or `-` if reading from stdin
    #[clap(short, long, default_value = "-")]
    pub input: FileOrStdin,

    /// Override default input format
    ///
    /// If not provided, the input format is read from the program frontmatter,
    /// or defaults to `ints`.
    #[clap(long)]
    pub input_format: Option<Format>,

    /// Output file, or `-` if writing to stdout
    #[clap(short, long, default_value = "-")]
    pub output: FileOrStdout,

    /// Override default output format
    ///
    /// If not provided, the output format is read from the program frontmatter,
    /// or defaults to `ints`.
    #[clap(long)]
    pub output_format: Option<Format>,
}

#[derive(Debug, Snafu)]
pub enum Error {
    ReadProgram { source: io::Error },
    ParseProgram { source: crate::program::ParseError },
    Input { source: StdinError },
    Output { source: io::Error },

    Runtime { source: DriveError },
}

pub fn main(
    Cli {
        program,
        input,
        input_format,
        output,
        output_format,
    }: Cli,
) -> Result<(), Error> {
    let program =
        Program::parse(&fs::read(program).context(ReadProgramSnafu)?).context(ParseProgramSnafu)?;

    let input = Reader::new(
        input_format.unwrap_or(program.info.input),
        input.into_reader().context(InputSnafu)?,
    );

    let output = Writer::new(
        output_format.unwrap_or(program.info.output),
        output.into_writer().context(OutputSnafu)?,
    );

    let mut vm = Vm::new(program);

    vm.drive(input, output).context(RuntimeSnafu)?;

    Ok(())
}
