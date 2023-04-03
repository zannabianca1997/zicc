//! Convert a file from a intcode format to another

#![feature(error_reporter)]

use std::{
    error::Report,
    fs::File,
    io::{self, stdin, stdout},
    path::PathBuf,
};

use clap::{Parser, ValueEnum};
use thiserror::Error;
use zicc::{ByteOrder, ICFormat, ICReadError, NewAsciiFormatError};

/// Cli arguments
#[derive(Debug, Parser)]
struct Args {
    /// Input file. If absent, read from standard input
    #[arg(short, long)]
    input: Option<PathBuf>,
    /// Input file format
    #[arg(short = 'f', long)]
    input_fmt: Option<FormatDiscriminant>,
    /// If input is ascii, the separator used to split values
    #[arg(short = 's', long)]
    input_sep: Option<char>,
    /// If input is binary, the byte order
    #[arg(short = 'e', long)]
    input_endianness: Option<ByteOrder>,
    /// Output file. If absent, write to standard output
    #[arg(short, long)]
    output: Option<PathBuf>,
    /// Output file format
    #[arg(short = 'F', long)]
    output_fmt: Option<FormatDiscriminant>,
    /// If output is ascii, the separator used to split values
    #[arg(short = 'S', long)]
    output_sep: Option<char>,
    /// If output is binary, the byte order
    #[arg(short = 'E', long)]
    output_endianness: Option<ByteOrder>,
}

#[derive(Debug)]
struct Setup {
    input: Option<PathBuf>,
    input_fmt: ICFormat,
    output: Option<PathBuf>,
    output_fmt: ICFormat,
}
impl TryFrom<Args> for Setup {
    fn try_from(
        Args {
            input,
            input_fmt,
            input_sep,
            input_endianness,
            output,
            output_fmt,
            output_sep,
            output_endianness,
        }: Args,
    ) -> Result<Self, Self::Error> {
        use FormatDiscriminant::*;
        Ok(Self {
            input,
            input_fmt: match input_fmt {
                Some(Ascii) => match input_sep {
                    Some(sep) => ICFormat::ascii(sep)?,
                    None => ICFormat::ascii_default(),
                },
                Some(Binary) => match input_endianness {
                    Some(endianness) => ICFormat::binary(endianness),
                    None => ICFormat::binary_default(),
                },
                None => Default::default(),
            },
            output,
            output_fmt: match output_fmt {
                Some(Ascii) => match output_sep {
                    Some(sep) => ICFormat::ascii(sep)?,
                    None => ICFormat::ascii_default(),
                },
                Some(Binary) => match output_endianness {
                    Some(endianness) => ICFormat::binary(endianness),
                    None => ICFormat::binary_default(),
                },
                None => Default::default(),
            },
        })
    }

    type Error = NewAsciiFormatError;
}

#[derive(Debug, Default, Clone, Copy, ValueEnum)]
enum FormatDiscriminant {
    #[default]
    Ascii,
    Binary,
}

#[derive(Debug, Error)]
enum MainError {
    #[error(transparent)]
    Clap(#[from] clap::Error),
    #[error("Invalid separator")]
    AsciiSeparator(#[from] NewAsciiFormatError),
    #[error("Error in opening input file {0:?}")]
    InputOpen(PathBuf, #[source] io::Error),
    #[error("Error in input")]
    Input(
        #[from]
        #[source]
        ICReadError,
    ),
    #[error("Error in opening output file {0:?}")]
    OutputOpen(PathBuf, #[source] io::Error),
    #[error("Error in output")]
    Output(#[source] io::Error),
}

fn main_unreported() -> Result<(), MainError> {
    let Setup {
        input,
        input_fmt,
        output,
        output_fmt,
    } = Args::try_parse()
        .map_err(MainError::from)
        .and_then(|args| Setup::try_from(args).map_err(MainError::from))?;
    // Read the input program
    let prog = match input {
        Some(path) => {
            let mut file = File::open(&path).map_err(|err| MainError::InputOpen(path, err))?;
            input_fmt.read(&mut file)
        }
        None => input_fmt.read(&mut stdin()),
    }
    .map_err(MainError::from)?;
    // write the output program
    match output {
        Some(path) => {
            let mut file = File::create(&path).map_err(|err| MainError::OutputOpen(path, err))?;
            output_fmt
                .write(&prog, &mut file)
                .map_err(MainError::Output)?
        }
        None => output_fmt
            .write(&prog, &mut stdout())
            .map_err(MainError::Output)?,
    };
    Ok(())
}

fn main() -> Result<(), Report<MainError>> {
    main_unreported().map_err(|err| Report::from(err).pretty(true))
}
