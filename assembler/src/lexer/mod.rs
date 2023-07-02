mod span;
mod token;

use std::{mem, fmt::Display};

pub use span::*;
use thiserror::Error;
pub use token::*;

#[derive(Debug,Error)]
pub enum Error {
    UnknownToken(Span),
}
impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::UnknownToken(pos) => write!(f, "Unknow token at {pos:?}"),
        }
    }
}
type Result<T> = std::result::Result<T, Error>;

#[must_use]
pub fn lex(input: &str) -> impl Iterator<Item = Result<Token>> + '_ {
    input.lines().flat_map(|line| 
    // remove the eventual comment after the ';'
    line.rsplit_once(';')
        .map(|(l, _)| l)
        .unwrap_or(line)
        // split by whitespace
        .split([' ', '\t'])
        .filter(|s| !s.is_empty())
        .map(|s| Token::parse(s,  unsafe {
            // Safety: `s` is always a subslice of `input`, and `input` is alive as long as the iterator is alive
            subslice_pos(input, s)
        }))
        .chain(Some(Ok(Token::Punctuator(Punctuator::Newline, unsafe {
            // Safety: `line` is always a subslice of `input`, and `input` is alive as long as the iterator is alive
            subslice_pos(input, line)
        } + line.len()))))
    )
}

/// Find the index of a subslice
/// 
/// # Safety
/// This function is safe as long as `sub` is a subslice of `text`
#[must_use]
unsafe fn subslice_pos(text: &str, sub: &str) -> usize {
    sub.as_ptr().offset_from(text.as_ptr()) as usize
}

#[cfg(test)]
mod tests {
    use itertools::Itertools;

    use super::{lex, Error};

    #[test]
    fn empty() {
        let source = "";
        let (tokens, errors): (Vec<_>, Vec<_>) = lex(source).partition_result();
        assert!(tokens.is_empty()&&errors.is_empty())
    }

    #[test]
    fn add() {
        let source = "add 1 2 3\n";
        let (tokens, errors): (Vec<_>, Vec<_>) = lex(source).partition_result();
        assert!(errors.is_empty());
        assert_eq!(tokens, ["add","1","2","3","\n"]);
    }

    #[test]
    fn newline_added() {
        let source = "add 1 2 3";
        let (tokens, errors): (Vec<_>, Vec<_>) = lex(source).partition_result();
        assert!(errors.is_empty());
        assert_eq!(tokens, ["add","1","2","3","\n"]);
    }
}