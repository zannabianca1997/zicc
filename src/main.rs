use clap::Parser;
use snafu::Snafu;

/// Zanna's IntCode Compiler
///
/// Compile, assemble, link and run an high level language to IntCode.
#[derive(Debug, Clone, Parser)]
#[clap(version)]
enum Cli {
    Vm(zicc_vm::cli::Cli),
}

#[derive(Debug, Snafu)]
enum Error {
    #[snafu(transparent)]
    Vm { source: zicc_vm::cli::Error },
}

fn main() -> Result<(), Error> {
    match Cli::parse() {
        Cli::Vm(cli) => zicc_vm::cli::main(cli)?,
    }

    Ok(())
}
