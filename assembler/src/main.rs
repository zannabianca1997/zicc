use clap::Parser;
use zicc_assembler::cli;

fn main() -> Result<(), cli::Error> {
    let cli = cli::Cli::parse();

    cli::main(cli)
}
