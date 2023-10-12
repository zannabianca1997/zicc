use std::num::{IntErrorKind, ParseIntError};

use itertools::Itertools;
use logos::Logos;
use thiserror::Error;
use vm::VMInt;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Identifier<'s> {
    Unnamed(usize),
    Named(&'s str),
    Start,
    End,
    UnitStart,
    UnitEnd,
}

#[derive(Debug, Clone, PartialEq, Eq, Default, Error)]
pub enum LexError {
    #[default]
    #[error("Generic tokenizer error")]
    Unknow,
    #[error("Unnamed label too big to fit in usize")]
    UnnamedTooLong,
    #[error("Char literal can contain only a single char")]
    MultipleCharInCharLiteral,
    #[error("Escaped char error")]
    EscapeError {
        #[source]
        err: EscapeError,
        token_offset: usize,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Error)]
pub enum EscapeError {
    #[error("Unknow escape sequence")]
    UnknowEscapeSequence,
    #[error("Escaped value should be an hexadecimal integer")]
    NonHexEscape(
        #[from]
        #[source]
        ParseIntError,
    ),
}

fn parse_unnamed<'s>(
    lex: &mut logos::Lexer<'s, Token<'s>>,
) -> Result<Identifier<'static>, LexError> {
    match lex.slice().strip_prefix('$').unwrap().parse() {
        Ok(n) => Ok(Identifier::Unnamed(n)),
        Err(err) if *err.kind() == IntErrorKind::PosOverflow => Err(LexError::UnnamedTooLong),
        Err(_) => unreachable!("The regex should avoid other error types"),
    }
}
fn parse_vmint<'s>(lex: &mut logos::Lexer<'s, Token<'s>>) -> Result<VMInt, LexError> {
    match lex.slice().parse() {
        Ok(n) => Ok(n),
        Err(err) if *err.kind() == IntErrorKind::PosOverflow => Err(LexError::UnnamedTooLong),
        Err(_) => unreachable!("The regex should avoid other error types"),
    }
}
fn parse_char<'s>(lex: &mut logos::Lexer<'s, Token<'s>>) -> Result<VMInt, LexError> {
    let content = lex
        .slice()
        .strip_prefix('\'')
        .unwrap()
        .strip_suffix('\'')
        .unwrap();
    let ch = match content.strip_prefix('\\') {
        None => {
            let Some((ch,)) = content.chars().collect_tuple() else {
                return Err(LexError::MultipleCharInCharLiteral);
            };
            (ch as u32).into()
        }
        Some(escape) => {
            let (ch, "") = parse_escape(escape).map_err(|err| LexError::EscapeError {
                err,
                token_offset: "\"\\".len(),
            })?
            else {
                return Err(LexError::MultipleCharInCharLiteral);
            };
            ch
        }
    };
    Ok(ch)
}
fn parse_escape(s: &str) -> Result<(VMInt, &str), EscapeError> {
    let mut chars = s.chars();
    Ok((
        (match chars
            .next()
            .expect("No empty escape should be passed to this function")
        {
            'a' => '\u{07}',
            'b' => '\u{08}',
            'v' => '\u{0B}',
            'f' => '\u{0C}',
            'n' => '\u{0A}',
            'r' => '\u{0D}',
            't' => '\u{09}',
            'e' => '\u{1B}',
            'E' => '\u{1B}',
            '\\' => '\u{5C}',
            '\'' => '\u{27}',
            '"' => '\u{22}',
            '$' => '\u{24}',
            '`' => '\u{60}',
            'x' => {
                let Some((code, rem)) = chars
                    .as_str()
                    .strip_prefix('{')
                    .and_then(|rem| rem.split_once('}'))
                else {
                    return Err(EscapeError::UnknowEscapeSequence);
                };
                let ch = VMInt::from_str_radix(code, 16)?;

                return Ok((ch, rem));
            }
            _ => return Err(EscapeError::UnknowEscapeSequence),
        } as u32)
            .into(),
        chars.as_str(),
    ))
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StringLit<'s> {
    content: &'s str,
}
impl Iterator for StringLit<'_> {
    type Item = VMInt;

    fn next(&mut self) -> Option<Self::Item> {
        // cut the first char
        let mut chars = self.content.chars();
        let ch = chars.next()?;
        self.content = chars.as_str();

        if ch == '\\' {
            // parse the escape and cut it out
            let (ch, rem) =
                parse_escape(self.content).expect("Only valid escape should be in content");
            self.content = rem;
            Some(ch)
        } else {
            Some((ch as u32).into())
        }
    }
}
fn parse_string_lit<'s>(lex: &mut logos::Lexer<'s, Token<'s>>) -> Result<StringLit<'s>, LexError> {
    let content = lex
        .slice()
        .strip_prefix('"')
        .unwrap()
        .strip_suffix('"')
        .unwrap();
    // check that the string is valid escaped
    {
        let mut rem = content;
        while !rem.is_empty() {
            // cut the first char
            let mut chars = rem.chars();
            let ch = chars.next().unwrap();
            rem = chars.as_str();

            if ch == '\\' {
                // check the escape and cut it out
                rem = parse_escape(rem)
                    .map_err(|err| LexError::EscapeError {
                        err,
                        token_offset: '"'.len_utf8() + content.len() - rem.len(),
                    })?
                    .1
            }
        }
    }
    Ok(StringLit { content })
}

#[derive(Logos, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[logos(skip r"(?:[\s&&[^\n]]+|\\[\s&&[^\n]]*\n|//[^\n]*|/\*[^*]*\*+(?:[^*/][^*]*\*+)*/)+")]
#[logos(error=LexError)]
pub enum Token<'s> {
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", |lex| Identifier::Named(lex.slice()))]
    #[regex(r"\$[0-9]+", parse_unnamed)]
    #[token("$start", |_| Identifier::Start)]
    #[token("$end", |_| Identifier::End)]
    #[token("$unit_start", |_| Identifier::UnitStart)]
    #[token("$unit_end", |_| Identifier::UnitEnd)]
    Ident(Identifier<'s>),

    #[regex(r"[0-9]+", parse_vmint)]
    #[regex(r"'(?:[^\\']|\\.)*'", parse_char)]
    Number(VMInt),

    #[regex(r#""(?:[^\\"]|\\.)*'""#, parse_string_lit)]
    StringLit(StringLit<'s>),

    // separators
    #[token("\n")]
    #[token(";")]
    Newline,
    #[token(",")]
    Comma,
    #[token(":")]
    Colon,

    // parameters modes
    #[token("@")]
    At,
    #[token("#")]
    Pound,

    // delimiters
    #[token("(")]
    OpenPar,
    #[token(")")]
    ClosePar,

    // Operators
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Mul,
    #[token("/")]
    Div,
    #[token("%")]
    Mod,
}
