//! Run a intcode file

#![feature(error_reporter)]

use std::{
    error::Report,
    fs::File,
    io::{self, stdin, stdout, BufWriter, Read, Write},
    num::ParseIntError,
    path::PathBuf,
};

use clap::{Parser, ValueEnum};
use either::Either::{self, Left, Right};
use lazy_regex::{regex, regex_find};
use thiserror::Error;
use zicc::{
    machine::{ICMAchineInputErr, ICMachine, ICMachineData, ICRuntimeErr},
    ByteOrder, ICFormat, ICReadError, NewAsciiFormatError,
};

/// Cli arguments
#[derive(Debug, Parser)]
struct Args {
    /// Program file
    program: PathBuf,
    /// Program file format
    #[arg(short, long)]
    fmt: Option<FileFormatDiscriminant>,
    /// If program is ascii, the separator used to split values
    #[arg(short, long)]
    sep: Option<char>,
    /// If program is binary, the byte order
    #[arg(short, long)]
    endianness: Option<ByteOrder>,
    /// Input stream format
    #[arg(short, long)]
    input_fmt: Option<StreamFormatDiscriminant>,
    /// Output stream format
    #[arg(short, long)]
    output_fmt: Option<StreamFormatDiscriminant>,
}

#[derive(Debug)]
struct Setup {
    program: PathBuf,
    fmt: ICFormat,
    input_fmt: StreamFormatDiscriminant,
    output_fmt: StreamFormatDiscriminant,
}
impl TryFrom<Args> for Setup {
    fn try_from(
        Args {
            program,
            fmt,
            sep,
            endianness,
            input_fmt,
            output_fmt,
        }: Args,
    ) -> Result<Self, Self::Error> {
        use FileFormatDiscriminant::*;
        Ok(Self {
            program,
            fmt: match fmt {
                Some(Ascii) => match sep {
                    Some(sep) => ICFormat::ascii(sep)?,
                    None => ICFormat::ascii_default(),
                },
                Some(Binary) => match endianness {
                    Some(endianness) => ICFormat::binary(endianness),
                    None => ICFormat::binary_default(),
                },
                None => Default::default(),
            },
            input_fmt: input_fmt.unwrap_or_default(),
            output_fmt: output_fmt.unwrap_or_default(),
        })
    }

    type Error = NewAsciiFormatError;
}

#[derive(Debug, Default, Clone, Copy, ValueEnum)]
enum FileFormatDiscriminant {
    #[default]
    Ascii,
    Binary,
}

#[derive(Debug, Default, Clone, Copy, ValueEnum)]
enum StreamFormatDiscriminant {
    /// Integers splitted by any char
    Values,
    /// Every byte is a value. -1 for EOF
    #[default]
    Bytes,
}

#[derive(Debug, Error)]
#[error("Value {0} is out of range for a byte")]
struct ByteOutOfRangeError(i64);

#[derive(Debug, Error)]
enum MainError {
    #[error(transparent)]
    Clap(#[from] clap::Error),
    #[error("Invalid separator")]
    AsciiSeparator(#[from] NewAsciiFormatError),
    #[error("Error in opening input file {0:?}")]
    ProgramOpen(PathBuf, #[source] io::Error),
    #[error("Error in reading input file")]
    Program(
        #[source]
        #[from]
        ICReadError,
    ),
    #[error("Error in input")]
    Input(#[source] Either<io::Error, ParseIntError>),
    #[error("Error in output")]
    Output(#[source] Either<io::Error, ByteOutOfRangeError>),
    #[error("Error while giving input to the machine")]
    MachineInput(
        #[source]
        #[from]
        ICMAchineInputErr,
    ),
    #[error("Error during runtime")]
    Runtime(
        #[source]
        #[from]
        ICRuntimeErr,
    ),
}

fn main_unreported() -> Result<(), MainError> {
    let Setup {
        program,
        fmt,
        input_fmt,
        output_fmt,
    } = Args::try_parse()
        .map_err(MainError::from)
        .and_then(|args| Setup::try_from(args).map_err(Into::into))?;
    // Read the input program
    let prog = {
        let mut file = File::open(&program).map_err(|err| MainError::ProgramOpen(program, err))?;
        fmt.read(&mut file)
    }?
    .into_iter()
    .map(Into::into)
    .collect::<Vec<_>>();
    // Create the machine
    let mut machine = ICMachineData::new(&prog);
    use zicc::machine::ICMachineStopState::*;
    loop {
        match machine.run() {
            EmptyInput => {
                for input in take_input(input_fmt).map_err(MainError::Input)? {
                    machine.give_input(input).map_err(MainError::MachineInput)?
                }
            }
            RuntimeErr(err) => return Err(err.into()),
            Halted => {
                drain_output(&mut machine, output_fmt).map_err(MainError::Output)?;
                return Ok(());
            }
        }
    }
}

fn main() -> Result<(), Report<MainError>> {
    main_unreported().map_err(|err| Report::from(err).pretty(true))
}

fn take_input(
    input_fmt: StreamFormatDiscriminant,
) -> Result<Vec<i64>, Either<io::Error, ParseIntError>> {
    match input_fmt {
        StreamFormatDiscriminant::Values => {
            // read until newline
            let mut line = String::new();
            match stdin().read_line(&mut line) {
                Ok(0) => Err(Left(io::Error::from(io::ErrorKind::UnexpectedEof))),
                Ok(_) => regex!(r"-?\d+")
                    .find_iter(&line)
                    .map(|v| v.as_str().parse().map_err(Right))
                    .collect(),
                Err(err) => Err(Left(err)),
            }
        }
        StreamFormatDiscriminant::Bytes => {
            // read available input
            let mut buf = [0; 1024];
            match stdin().read(&mut buf) {
                Ok(0) => Ok(vec![-1]),
                Ok(n) => Ok(buf[..n].into_iter().map(|v| *v as _).collect()),
                Err(err) => Err(Left(err)),
            }
        }
    }
}

fn drain_output(
    machine: &mut ICMachineData,
    output_fmt: StreamFormatDiscriminant,
) -> Result<(), Either<io::Error, ByteOutOfRangeError>> {
    let mut out = BufWriter::new(stdout());
    match output_fmt {
        StreamFormatDiscriminant::Values => {
            while let Some(v) = machine.get_output() {
                write!(out, "{v} ").map_err(Left)?
            }
        }
        StreamFormatDiscriminant::Bytes => {
            while let Some(v) = machine.get_output() {
                if 0 <= v && v < 256 {
                    write!(out, "{}", (v as u8) as char).map_err(Left)?
                } else {
                    return Err(Right(ByteOutOfRangeError(v)));
                }
            }
        }
    }
    // write out the remaining buffer
    out.flush().map_err(Left)
}
