use std::io::{read_to_string, stdin};

use assembler::lexer::Lexer;

fn main() {
    let input = read_to_string(stdin()).unwrap();
    for token in Lexer::new(&input) {
        match token {
            Ok((start, tok, end)) => println!("{start}..{end}\t=> {tok}"),
            Err(err) => {
                println!("! {err}")
            }
        }
    }
}
