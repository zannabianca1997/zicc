#![feature(const_trait_impl)]
#![feature(assert_matches)]
#![feature(is_some_and)]

mod codegen;
mod lexer;
mod parser;

pub use lexer::lex;
pub use parser::parse;
