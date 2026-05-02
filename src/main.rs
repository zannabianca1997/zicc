use clap::Parser;
use snafu::Snafu;

/// Zanna's IntCode Compiler
///
/// Compile, assemble, link and run an high level language to IntCode.
#[derive(Debug, Clone, Parser)]
#[clap(version)]
enum Cli {
    Assembler(zicc_assembler::cli::Cli),
    Linker(zicc_linker::cli::Cli),
    Vm(zicc_vm::cli::Cli),
}

#[derive(Debug, Snafu)]
enum Error {
    #[snafu(transparent)]
    Assembler { source: zicc_assembler::cli::Error },
    #[snafu(transparent)]
    Linker { source: zicc_linker::cli::Error },
    #[snafu(transparent)]
    Vm { source: zicc_vm::cli::Error },
}

fn main() -> Result<(), Error> {
    match Cli::parse() {
        Cli::Assembler(cli) => zicc_assembler::cli::main(cli)?,
        Cli::Linker(cli) => zicc_linker::cli::main(cli)?,
        Cli::Vm(cli) => zicc_vm::cli::main(cli)?,
    }

    Ok(())
}
