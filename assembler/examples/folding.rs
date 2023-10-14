use std::io::{read_to_string, stdin};

use assembler::ast::{constant_folding::ConstantFolding, File};

fn main() {
    let input = read_to_string(stdin()).unwrap();
    match File::parse(&input) {
        Ok(f) => {
            println!("Before: {f:?}");
            let f = f.constant_folding();
            println!("After: {f:?}");
        }
        Err(err) => eprintln!("{err}"),
    }
}
