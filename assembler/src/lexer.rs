use std::{
    fmt::{Debug, Display},
    num::{IntErrorKind, ParseIntError},
    ops::Range,
};

use itertools::Itertools;
use logos::{Logos, SpannedIter};
use thiserror::Error;
use vm::VMInt;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Identifier<'s> {
    Unnamed(usize),
    Named(&'s str),
}
impl Display for Identifier<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Identifier::Unnamed(n) => write!(f, "${n}"),
            Identifier::Named(n) => write!(f, "{n}"),
        }
    }
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum SpecialIdentifier {
    Start,
    End,
    UnitStart,
    UnitEnd,
}

#[derive(Debug, Clone, PartialEq, Eq, Error, Default)]
pub enum LexError {
    #[error("Unknow token")]
    #[default]
    Unknow,
    #[error("Unnamed label too big to fit in usize")]
    UnnamedTooLong,
    #[error("Char literal can contain only a single char")]
    MultipleCharInCharLiteral,
    #[error(transparent)]
    EscapeError(EscapeError),
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
    #[error("\\x escape should be followed by a braced hexadecimal")]
    MissingValue,
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
            let (ch, "") = parse_escape(escape).map_err(LexError::EscapeError)? else {
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
                    return Err(EscapeError::MissingValue);
                };
                let ch = VMInt::from_str_radix(code, 16)
                    .map_err(|err| EscapeError::NonHexEscape(err))?;

                return Ok((ch, rem));
            }
            ch => return Err(EscapeError::UnknowEscapeSequence),
        } as u32)
            .into(),
        chars.as_str(),
    ))
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StringLit<'s> {
    content: &'s str,
}
impl<'s> StringLit<'s> {
    fn iter(&self) -> StringLitIter<'s> {
        StringLitIter {
            content: self.content,
        }
    }
}
impl<'s> IntoIterator for StringLit<'s> {
    type Item = VMInt;

    type IntoIter = StringLitIter<'s>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

#[derive(Debug, Clone)]
pub struct StringLitIter<'s> {
    content: &'s str,
}
impl Iterator for StringLitIter<'_> {
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
                rem = parse_escape(rem).map_err(LexError::EscapeError)?.1
            }
        }
    }
    Ok(StringLit { content })
}

#[derive(Logos, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[logos(skip r"(?:[\s&&[^\n]]+|\\[\s&&[^\n]]*\n|//[^\n]*|/\*[^*]*\*+(?:[^*/][^*]*\*+)*/)+")]
#[logos(error=LexError)]
pub enum Token<'s> {
    #[regex(r"(?i)ints", |lex| Identifier::Named(lex.slice()))]
    Ints(Identifier<'s>),

    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", |lex| Identifier::Named(lex.slice()))]
    #[regex(r"\$[0-9]+", parse_unnamed)]
    Identifier(Identifier<'s>),

    #[token("$start", |_| SpecialIdentifier::Start)]
    #[token("$end", |_| SpecialIdentifier::End)]
    #[token("$unit_start", |_| SpecialIdentifier::UnitStart)]
    #[token("$unit_end", |_| SpecialIdentifier::UnitEnd)]
    SpecialIdentifier(SpecialIdentifier),

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

impl Display for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        <Self as Debug>::fmt(&self, f)
    }
}

pub struct Lexer<'s> {
    tokens: SpannedIter<'s, Token<'s>>,
}
impl<'s> Lexer<'s> {
    pub fn new(source: &'s str) -> Lexer<'s> {
        Lexer {
            tokens: Token::lexer(source).spanned(),
        }
    }
}
impl<'s> Iterator for Lexer<'s> {
    type Item = Result<(usize, Token<'s>, usize), LexError>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.tokens.next() {
            Some((Ok(token), Range { start, end })) => Some(Ok((start, token, end))),
            Some((Err(err), Range { start, end })) => Some(Err(err)),
            None => None,
        }
    }
}
