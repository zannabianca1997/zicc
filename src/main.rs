use clap::Parser;
use zicc::cli;

fn main() -> Result<(), cli::Error> {
    let cli = cli::Cli::parse();

    cli::main(cli)
}
