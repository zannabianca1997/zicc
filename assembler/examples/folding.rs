use std::io::{read_to_string, stdin};

use assembler::ast::{AstNode, File, ParseError};
use errors::RootAccumulator;

fn main() {
    let input = read_to_string(stdin()).unwrap();
    let mut errors = RootAccumulator::<ParseError>::new();
    match File::parse(&input, &mut errors) {
        Some(f) => {
            println!("Before:\n{f:?}");
            let f = f.constant_folding();
            println!("After:\n{f:?}");
        }
        None => {
            for err in errors.checkpoint().unwrap_err() {
                eprintln!("{err}")
            }
        }
    }
}
