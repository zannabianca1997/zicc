//! Assemble a intcode assembly file in a intcode object file

#![feature(error_reporter)]

use std::{
    default,
    error::Report,
    fs::File,
    io::{self, stdin, stdout, Read, Write},
    path::PathBuf,
};

use clap::{Parser, ValueEnum};
use either::Either::{self, Left, Right};
use thiserror::Error;
use zicc::{parse_assembly, AssembleError, AssemblyParseError};

/// Cli arguments
#[derive(Debug, Parser)]
struct Args {
    /// Program file. If absent, read from stdin
    #[arg(short, long)]
    input: Option<PathBuf>,
    /// Output file. If absent, write to stdout
    #[arg(short, long)]
    output: Option<PathBuf>,
    /// Output format
    #[arg(short, long, default_value = "yaml")]
    format: SerializeFormat,
    /// Do not strip local object labels
    #[arg(long)]
    dont_strip: bool,
}

#[derive(Debug, PartialEq, Eq, ValueEnum, Clone, Copy, Default)]
enum SerializeFormat {
    /// Human readable format
    #[default]
    YAML,
}

#[derive(Debug, Error)]
enum MainError {
    #[error(transparent)]
    Clap(#[from] clap::Error),
    #[error("Error in reading input file")]
    Input(#[source] io::Error),
    #[error("Error in parsing assembly code")]
    Parse(
        #[source]
        #[from]
        AssemblyParseError,
    ),
    #[error("Error in assembling")]
    Assemble(
        #[source]
        #[from]
        AssembleError,
    ),
    #[error("Error in writing output file")]
    Output(#[source] io::Error),
    #[error("Error in serializing yaml file")]
    SerializeYaml(
        #[source]
        #[from]
        serde_yaml::Error,
    ),
}

fn main_unreported() -> Result<(), MainError> {
    let Args {
        input,
        output,
        format,
        dont_strip,
    } = Args::try_parse()?;
    // read input file
    let input = if let Some(path) = input {
        let mut buf = String::new();
        File::open(path)
            .and_then(|mut f| f.read_to_string(&mut buf))
            .map_err(MainError::Input)?;
        buf.into_boxed_str()
    } else {
        let mut buf = String::new();
        stdin().read_to_string(&mut buf).map_err(MainError::Input)?;
        buf.into_boxed_str()
    };
    // parse file
    let parsed = parse_assembly(&input)?;
    // assemble
    let mut assembled = parsed.assemble()?;
    if !dont_strip {
        assembled.remove_labels(|label| !label.is_global())
    }
    // dump
    let output = if let Some(path) = output {
        Left(File::create(path).map_err(MainError::Output)?)
    } else {
        Right(stdout())
    };
    match format {
        SerializeFormat::YAML => serde_yaml::to_writer(output, &assembled)?,
    }
    Ok(())
}

fn main() -> Result<(), Report<MainError>> {
    main_unreported().map_err(|err| Report::from(err).pretty(true))
}
