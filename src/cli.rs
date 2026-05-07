use std::path::PathBuf;

use clap::{Args, Parser, Subcommand};
use clap_stdin::FileOrStdout;
use snafu::Snafu;

/// Zanna's IntCode Compiler
///
/// Compile, assemble, link and run an high level language to IntCode.
#[derive(Debug, Clone, Parser)]
#[clap(version, args_conflicts_with_subcommands = true)]
pub struct Cli {
    #[clap(flatten)]
    pub router_args: Option<RouterArgs>,

    #[clap(subcommand)]
    pub command: Option<Command>,
}

#[derive(Debug, Clone, Subcommand)]
pub enum Command {
    Compiler(zicc_compiler::cli::Cli),
    Assembler(zicc_assembler::cli::Cli),
    Linker(zicc_linker::cli::Cli),
    Vm(zicc_vm::cli::Cli),
}

#[derive(Debug, Clone, Args)]
pub struct RouterArgs {
    /// Input files to compile, assemble, link or run
    #[arg(num_args = 1.., required = true)]
    pub inputs: Vec<PathBuf>,

    /// Output file; if omitted, run the program.
    ///
    /// The flag alone or with `-` defaults to writing to stdout.
    #[arg(short, long, num_args = 0..=1, default_missing_value = "-")]
    pub output: Option<FileOrStdout>,

    /// Only link the sources, do not produce an executable
    #[arg(short = 'L', long)]
    pub link_only: bool,
}

#[derive(Debug, Snafu)]
pub enum Error {
    #[snafu(transparent)]
    Compiler { source: zicc_compiler::cli::Error },
    #[snafu(transparent)]
    Assembler { source: zicc_assembler::cli::Error },
    #[snafu(transparent)]
    Linker { source: zicc_linker::cli::Error },
    #[snafu(transparent)]
    Vm { source: zicc_vm::cli::Error },
    #[snafu(transparent)]
    Router { source: crate::router::Error },
}

pub fn main(cli: Cli) -> Result<(), Error> {
    match cli {
        Cli {
            router_args: None,
            command: Some(command),
        } => match command {
            Command::Compiler(cli) => zicc_compiler::cli::main(cli)?,
            Command::Assembler(cli) => zicc_assembler::cli::main(cli)?,
            Command::Linker(cli) => zicc_linker::cli::main(cli)?,
            Command::Vm(cli) => zicc_vm::cli::main(cli)?,
        },
        Cli {
            router_args: Some(router_args),
            command: None,
        } => {
            let RouterArgs {
                inputs,
                output,
                link_only,
            } = router_args;
            crate::router::route(inputs, output, link_only)?
        }
        _ => unreachable!(
            "`clap` parsing should guarantee that either common args or router args are given"
        ),
    }

    Ok(())
}
