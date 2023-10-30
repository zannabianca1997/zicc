use clap::Parser;

use zicc::Cli;

fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();
    zicc::cli(cli)
}
