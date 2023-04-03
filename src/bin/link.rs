//! Join multiple intcode files in a single executable

#![feature(error_reporter)]

use std::{
    collections::HashSet,
    error::Report,
    fs::File,
    io::{self, stdin, stdout, Read},
    path::PathBuf,
};

use clap::{Parser, ValueEnum};
use either::Either::{Left, Right};
use thiserror::Error;
use zicc::{
    assembler::{parse, AppendError, AssembleError, ICProgramFragment, Label, ParseError},
    icfile::{ByteOrder, ICFormat, NewAsciiFormatError},
};

/// Cli arguments
#[derive(Debug, Parser)]
struct Args {
    /// Input files
    inputs: Vec<PathBuf>,
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
    inputs: Vec<PathBuf>,
    output: Option<PathBuf>,
    output_fmt: ICFormat,
}
impl TryFrom<Args> for Setup {
    fn try_from(
        Args {
            inputs,
            output,
            output_fmt,
            output_sep,
            output_endianness,
        }: Args,
    ) -> Result<Self, Self::Error> {
        use FormatDiscriminant::*;
        Ok(Self {
            inputs,
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
    #[error("Error in reading input file")]
    Input(#[source] io::Error),
    #[error("Error in linking")]
    Link(
        #[source]
        #[from]
        AppendError,
    ),
    #[error("Error in writing output file")]
    Output(#[source] io::Error),
    #[error("Error in deserializing yaml input file")]
    DeserializeYaml(
        #[source]
        #[from]
        serde_yaml::Error,
    ),
    #[error("Labels {0:?} were not resolved")]
    UnresolvedLabels(HashSet<Label>),
}

fn main_unreported() -> Result<(), MainError> {
    let Setup {
        output,
        output_fmt,
        inputs,
    } = Args::try_parse()?.try_into()?;
    // link program
    let mut result = ICProgramFragment::empty();
    for path in inputs {
        let mut file = File::open(path).map_err(MainError::Input)?;
        let mut fragment: ICProgramFragment = serde_yaml::from_reader(&mut file)?;
        fragment.remove_labels(|lbl| !lbl.is_global());
        result = result.join(fragment)?;
    }
    // emit
    if result.is_free() {
        let result = result.emit().unwrap();
        // output it
        match output {
            Some(path) => {
                let mut file = File::create(&path).map_err(|err| MainError::Output(err))?;
                output_fmt
                    .write(&result, &mut file)
                    .map_err(MainError::Output)?
            }
            None => output_fmt
                .write(&result, &mut stdout())
                .map_err(MainError::Output)?,
        };
        Ok(())
    } else {
        return Err(MainError::UnresolvedLabels(
            result.referenced_labels().cloned().collect(),
        ));
    }
}

fn main() -> Result<(), Report<MainError>> {
    main_unreported().map_err(|err| Report::from(err).pretty(true))
}
