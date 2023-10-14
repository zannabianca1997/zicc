use std::io::{read_to_string, stdin};

use assembler::ast::File;

fn main() {
    let input = read_to_string(stdin()).unwrap();
    match File::parse(&input) {
        Ok(f) => println!("{f:?}"),
        Err(err) => eprintln!("{err}"),
    }
}
