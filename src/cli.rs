use clap::{Args, Parser, Subcommand};
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
    Assembler(zicc_assembler::cli::Cli),
    Linker(zicc_linker::cli::Cli),
    Vm(zicc_vm::cli::Cli),
}

#[derive(Debug, Clone, Args)]
pub struct RouterArgs {}

#[derive(Debug, Snafu)]
pub enum Error {
    #[snafu(transparent)]
    Assembler { source: zicc_assembler::cli::Error },
    #[snafu(transparent)]
    Linker { source: zicc_linker::cli::Error },
    #[snafu(transparent)]
    Vm { source: zicc_vm::cli::Error },
    #[snafu(transparent)]
    Router { source: super::Error },
}

pub fn main(cli: Cli) -> Result<(), Error> {
    match cli {
        Cli {
            router_args: None,
            command: Some(command),
        } => match command {
            Command::Assembler(cli) => zicc_assembler::cli::main(cli)?,
            Command::Linker(cli) => zicc_linker::cli::main(cli)?,
            Command::Vm(cli) => zicc_vm::cli::main(cli)?,
        },
        Cli {
            router_args: Some(RouterArgs {}),
            command: None,
        } => crate::router()?,
        _ => unreachable!(
            "`clap` parsing should guarantee that either common args or router args are given"
        ),
    }

    Ok(())
}
