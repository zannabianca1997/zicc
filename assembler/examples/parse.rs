use std::io::{read_to_string, stdin};

use assembler::ast::{File, ParseError};
use errors::RootAccumulator;

fn main() {
    let input = read_to_string(stdin()).unwrap();
    let mut errors = RootAccumulator::<ParseError>::new();
    match File::parse(&input, &mut errors) {
        Some(f) => {
            println!("{f:?}");
        }
        None => {
            for err in errors.checkpoint().unwrap_err() {
                eprintln!("{err}")
            }
        }
    }
}
