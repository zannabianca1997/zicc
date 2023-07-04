mod span;
pub mod tokens;

use std::{fmt::Display};

pub use span::*;
pub use tokens::Token;
use thiserror::Error;
use utils::collect_oks_or_errs::CollectOksOrErrs;

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
impl Spanned for Error {
    fn span(&self) -> Span {
        match self {
            Error::UnknownToken(s) => *s,
        }
    }
}

#[must_use]
pub fn lex(input: &str) -> std::result::Result<Vec<Token>, Vec<Error>> {
    input.lines().flat_map(|line| 
    // remove the eventual comment after the ';'
    line.rsplit_once(';')
        .map(|(l, _)| l)
        .unwrap_or(line)
        // split by whitespace
        .split([' ', '\t'])
        .filter(|s| !s.is_empty())
        .map(|s| Token::lex(s,  unsafe {
            // Safety: `s` is always a subslice of `input`, and `input` is alive as long as the iterator is alive
            subslice_pos(input, s)
        }))
        .chain(Some(Ok(Token::lex("\n", unsafe {
            // Safety: `line` is always a subslice of `input`, and `input` is alive as long as the iterator is alive
            subslice_pos(input, line)
        } + line.len()).unwrap())))
    ).collect_oks_or_errs()
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

    use super::{lex};

    #[test]
    fn empty() {
        let source = "";
        let tokens = lex(source).unwrap();
        assert!(tokens.is_empty())
    }

    #[test]
    fn add() {
        let source = "add 1 2 3\n";
        let tokens = lex(source).unwrap();
        assert_eq!(tokens, ["add","1","2","3","\n"]);
    }

    #[test]
    fn newline_added() {
        let source = "add 1 2 3";
        let tokens = lex(source).unwrap();
        assert_eq!(tokens, ["add","1","2","3","\n"]);
    }
}