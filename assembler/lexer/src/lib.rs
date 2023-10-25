use std::{
    fmt::{Debug, Display},
    hash::Hash,
    num::{IntErrorKind, ParseIntError},
    ops::Range,
};

use itertools::Itertools;
use keyword_expand_macro::expand_keywords;
use logos::{Logos, SpannedIter};
use thiserror::Error;

use errors::Spanned;
use vm::VMInt;

#[derive(Debug, Clone, Copy)]
pub enum Identifier<'s> {
    Unnamed(usize, (usize, usize)),
    Named(&'s str, usize),
}
impl Display for Identifier<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Identifier::Unnamed(n, ..) => write!(f, "${n}"),
            Identifier::Named(n, ..) => write!(f, "{n}"),
        }
    }
}
impl Spanned for Identifier<'_> {
    fn span(&self) -> Range<usize> {
        match self {
            Identifier::Unnamed(_, (start, end)) => *start..*end,
            Identifier::Named(s, start) => *start..(start + s.len()),
        }
    }
}
impl PartialEq for Identifier<'_> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Unnamed(l0, ..), Self::Unnamed(r0, ..)) => l0 == r0,
            (Self::Named(l0, ..), Self::Named(r0, ..)) => l0 == r0,
            _ => false,
        }
    }
}
impl Eq for Identifier<'_> {}
impl PartialOrd for Identifier<'_> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Self::Unnamed(l0, ..), Self::Unnamed(r0, ..)) => l0.partial_cmp(r0),
            (Self::Named(l0, ..), Self::Named(r0, ..)) => l0.partial_cmp(r0),
            (Identifier::Unnamed(_, _), Identifier::Named(_, _)) => {
                Some(std::cmp::Ordering::Greater)
            }
            (Identifier::Named(_, _), Identifier::Unnamed(_, _)) => Some(std::cmp::Ordering::Less),
        }
    }
}
impl Ord for Identifier<'_> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match (self, other) {
            (Self::Unnamed(l0, ..), Self::Unnamed(r0, ..)) => l0.cmp(r0),
            (Self::Named(l0, ..), Self::Named(r0, ..)) => l0.cmp(r0),
            (Identifier::Unnamed(_, _), Identifier::Named(_, _)) => std::cmp::Ordering::Greater,
            (Identifier::Named(_, _), Identifier::Unnamed(_, _)) => std::cmp::Ordering::Less,
        }
    }
}
impl Hash for Identifier<'_> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        core::mem::discriminant(self).hash(state);
        match self {
            Identifier::Unnamed(n, ..) => n.hash(state),
            Identifier::Named(n, ..) => n.hash(state),
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

#[derive(Debug, Clone, PartialEq, Eq, Error)]
pub enum LexError {
    #[error("Unknow token")]
    Unknow { span: Range<usize> },
    #[error("Unnamed label too big to fit in usize")]
    UnnamedTooLong { span: Range<usize> },
    #[error("Char literal can contain only a single char")]
    MultipleCharInCharLiteral { span: Range<usize> },
    #[error("Error in escaping string")]
    EscapeError {
        #[source]
        err: EscapeError,
        span: Range<usize>,
    },
}
impl LexError {
    fn spanned_at(mut self, new_span: Range<usize>) -> LexError {
        match &mut self {
            LexError::EscapeError { span, .. }
            | LexError::Unknow { span }
            | LexError::UnnamedTooLong { span }
            | LexError::MultipleCharInCharLiteral { span } => *span = new_span,
        }
        self
    }
}
impl Default for LexError {
    fn default() -> Self {
        Self::Unknow {
            span: Default::default(),
        }
    }
}
impl Spanned for LexError {
    fn span(&self) -> Range<usize> {
        match self {
            LexError::Unknow { span }
            | LexError::UnnamedTooLong { span }
            | LexError::MultipleCharInCharLiteral { span } => span.clone(),
            LexError::EscapeError { err, span } => {
                let mut inner = err.span();
                inner.start += span.start;
                inner.end += span.start;
                inner
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Error)]
pub enum EscapeError {
    #[error("Unknow escape sequence")]
    UnknowEscapeSequence { span: Range<usize> },
    #[error("Escaped value should be an hexadecimal integer")]
    NonHexEscape {
        #[source]
        err: ParseIntError,
        span: Range<usize>,
    },
    #[error("\\x escape should be followed by a braced hexadecimal")]
    MissingValue { span: Range<usize> },
}
impl Spanned for EscapeError {
    fn span(&self) -> Range<usize> {
        match self {
            EscapeError::UnknowEscapeSequence { span }
            | EscapeError::NonHexEscape { span, .. }
            | EscapeError::MissingValue { span } => span.clone(),
        }
    }
}

fn parse_unnamed<'s>(
    lex: &mut logos::Lexer<'s, Token<'s>>,
) -> Result<Identifier<'static>, LexError> {
    match lex.slice().strip_prefix('$').unwrap().parse() {
        Ok(n) => Ok(Identifier::Unnamed(n, (lex.span().start, lex.span().end))),
        Err(err) if *err.kind() == IntErrorKind::PosOverflow => Err(LexError::UnnamedTooLong {
            span: Default::default(),
        }),
        Err(_) => unreachable!("The regex should avoid other error types"),
    }
}
fn parse_vmint<'s>(lex: &mut logos::Lexer<'s, Token<'s>>) -> Result<VMInt, LexError> {
    match lex.slice().parse() {
        Ok(n) => Ok(n),
        Err(err) if *err.kind() == IntErrorKind::PosOverflow => Err(LexError::UnnamedTooLong {
            span: Default::default(),
        }),
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
                return Err(LexError::MultipleCharInCharLiteral {
                    span: Default::default(),
                });
            };
            (ch as u32).into()
        }
        Some(escape) => {
            let (ch, "") = parse_escape(escape).map_err(|err| LexError::EscapeError {
                err,
                span: Default::default(),
            })?
            else {
                return Err(LexError::MultipleCharInCharLiteral {
                    span: Default::default(),
                });
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
            ch @ ('x' | 'X' | 'd' | 'D' | 'o' | 'O' | 'B') => {
                let Some((code, rem)) = chars
                    .as_str()
                    .strip_prefix('{')
                    .and_then(|rem| rem.split_once('}'))
                else {
                    return Err(EscapeError::MissingValue {
                        span: 0..ch.len_utf8(),
                    });
                };
                let ch = VMInt::from_str_radix(
                    code,
                    match ch {
                        'x' | 'X' => 16,
                        'd' | 'D' => 10,
                        'o' | 'O' => 8,
                        'B' => 2,
                        _ => unreachable!(),
                    },
                )
                .map_err(|err| {
                    let code_start = ch.len_utf8() + '{'.len_utf8();
                    EscapeError::NonHexEscape {
                        err,
                        span: code_start..code_start + code.len(),
                    }
                })?;

                return Ok((ch, rem));
            }
            ch => {
                return Err(EscapeError::UnknowEscapeSequence {
                    span: 0..ch.len_utf8(),
                })
            }
        } as u32)
            .into(),
        chars.as_str(),
    ))
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StringLit<'s> {
    pub content: &'s str,
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
                rem = parse_escape(rem)
                    .map_err(|err| LexError::EscapeError {
                        err,
                        span: Default::default(),
                    })?
                    .1
            }
        }
    }
    Ok(StringLit { content })
}

expand_keywords! {
#[derive(Logos, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[logos(skip r"(?:[\s&&[^\n]]+|\\[\s&&[^\n]]*\n|//[^\n]*|/\*[^*]*\*+(?:[^*/][^*]*\*+)*/)+")]
#[logos(error=LexError)]
pub enum Token<'s> {
    $(
        #[regex($kwd_regex, |lex| Identifier::Named(lex.slice(), lex.span().start))]
        $Keyword(Identifier<'s>),
    )kwds

    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", |lex| Identifier::Named(lex.slice(), lex.span().start))]
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

    #[regex(r#""(?:[^\\"]+|\\.)*""#, parse_string_lit)]
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
    Times,
    #[token("/")]
    Div,
    #[token("%")]
    Mod,
}
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
            Some((Err(err), span)) => Some(Err(err.spanned_at(span))),
            None => None,
        }
    }
}

#[cfg(test)]
#[test_sources::test_sources]
fn lex_sources(source: &str) {
    let lexer = Lexer::new(source);
    for token in lexer {
        match token {
            Ok((start, token, end)) => println!("{start}..{end}\t=> {token:?}"),
            Err(err) => panic!("Lex error: {err}"),
        }
    }
}
