//! Cli harness to the assembler

use std::io::{self, Read as _};

use clap::Parser;
use clap_stdin::{FileOrStdin, FileOrStdout, StdinError};
use snafu::{ResultExt, Snafu};
use string_interner::DefaultStringInterner;
use zicc_assembler_program::{ParseError, Program};

use crate::{AssembleError, assemble};

/// Assemble IntCode Assembly
///
/// Assemble a IntCode Assembly file into a IntCode Objects file
#[derive(Debug, Clone, Parser)]
#[clap(version)]
pub struct Cli {
    /// Input files to assemble, or `-` if reading from stdin
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
    Assemble {
        source: AssembleError,
    },
}

pub fn main(Cli { input, output }: Cli) -> Result<(), Error> {
    let mut interner = DefaultStringInterner::new();

    let program = {
        let mut buf = Vec::new();
        input
            .into_reader()
            .context(InputSnafu)?
            .read_to_end(&mut buf)
            .map_err(StdinError::from)
            .context(InputSnafu)?;
        Program::parse(&buf, &mut interner, |_| todo!())?
    };

    let assembled = assemble(program)?;

    assembled
        .dump(output.into_writer().context(OutputSnafu)?, &interner)
        .context(OutputSnafu)?;

    Ok(())
}
