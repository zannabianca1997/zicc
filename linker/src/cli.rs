//! Cli harness to the linker

use std::io::{self, Read as _};

use clap::Parser;
use clap_stdin::{FileOrStdin, FileOrStdout, StdinError};
use snafu::{ResultExt, Snafu};
use string_interner::DefaultStringInterner;
use zicc_linker_program::{ParseError, Program};

use crate::{LinkError, MakeExecutableError, make_executable};

/// Links objects files
///
/// Link multiple intcode objects files together to produce an executable. Can
/// also produce a joined intcode objects file that can further be linked with
/// other files.
#[derive(Debug, Clone, Parser)]
#[clap(version)]
pub struct Cli {
    /// Input files to link together
    ///
    /// At maximum one can be replaced with '-' to read from standard input
    #[clap(num_args=1.., default_value = "-")]
    pub input: Vec<FileOrStdin>,

    /// Output file, or `-` if writing to stdout
    #[clap(short, long, default_value = "-")]
    pub output: FileOrStdout,

    /// Only link the sources, do not produce an executable
    #[clap(short = 'L', long)]
    pub link_only: bool,
}

#[derive(Debug, Snafu)]
pub enum Error {
    Input {
        source: StdinError,
    },
    Output {
        source: io::Error,
    },
    Parse {
        source: ParseError,
    },
    #[snafu(transparent)]
    Link {
        source: LinkError,
    },
    #[snafu(transparent)]
    MakeExecutable {
        source: MakeExecutableError,
    },
}

pub fn main(
    Cli {
        input,
        output,
        link_only,
    }: Cli,
) -> Result<(), Error> {
    let mut interner = DefaultStringInterner::new();

    let mut units = Vec::with_capacity(input.len());

    for input in input {
        let mut reader = input.into_reader().context(InputSnafu)?;
        let mut buffer = vec![];
        reader
            .read_to_end(&mut buffer)
            .map_err(StdinError::from)
            .context(InputSnafu)?;

        let program = Program::parse(&buffer, &mut interner).context(ParseSnafu)?;

        units.push(program);
    }

    let linked = crate::link(units, &mut interner)?;

    if link_only {
        linked
            .dump(output.into_writer().context(OutputSnafu)?, &interner)
            .context(OutputSnafu)?;
        return Ok(());
    }

    let executable = make_executable(linked, &mut interner)?;

    executable
        .dump(output.into_writer().context(OutputSnafu)?)
        .context(OutputSnafu)
}
