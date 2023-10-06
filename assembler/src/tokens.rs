use std::hash::Hash;

use errors::{Accumulator, SourceError, Spanned};
use itertools::Itertools;
use not_empty::{NonEmptySlice, NonEmptyVec};
use thiserror::Error;

use crate::{
    ast::{Parse, ParseAll, ParseError},
    parse_all_from_parse,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Token<'s> {
    Identifier(Identifier<'s>),
    Number(Number),
    Punctuator(Punctuator),
}
impl Token<'_> {
    pub fn len(&self) -> usize {
        match self {
            Token::Number(n) => n.len(),
            Token::Punctuator(p) => p.len(),
            Token::Identifier(i) => i.len(),
        }
    }
}
impl Spanned for Token<'_> {
    fn span(&self) -> std::ops::Range<usize> {
        match self {
            Token::Number(n) => n.span(),
            Token::Punctuator(p) => p.span(),
            Token::Identifier(i) => i.span(),
        }
    }
}
impl<'s> Parse<'s> for Token<'s> {
    fn parse<'t>(
        tokens: TokensSlice<'s, 't>,
        errors: &mut Accumulator<impl From<ParseError>>,
    ) -> Option<(Self, TokensSlice<'s, 't>)> {
        if let Some((t, rest)) = tokens.split_first() {
            Some((*t, rest))
        } else {
            errors.push(ParseError::ExpectedToken("Token", tokens.span().start));
            None
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Identifier<'s> {
    pub value: &'s str,
    pos: usize,
}
impl<'s> Identifier<'s> {
    pub fn as_str(&self) -> &'s str {
        self.value
    }
    pub fn len(&self) -> usize {
        self.value.len()
    }
}
impl Spanned for Identifier<'_> {
    fn span(&self) -> std::ops::Range<usize> {
        self.pos..(self.pos + self.len())
    }
}
impl<'s> Parse<'s> for Identifier<'s> {
    fn parse<'t>(
        tokens: TokensSlice<'s, 't>,
        errors: &mut Accumulator<impl From<ParseError>>,
    ) -> Option<(Self, TokensSlice<'s, 't>)> {
        if let Some((Token::Identifier(id), rest)) = tokens.split_first() {
            Some((*id, rest))
        } else {
            errors.push(ParseError::ExpectedToken("Identifier", tokens.span().start));
            None
        }
    }
}
parse_all_from_parse!(Identifier<'s>);
impl PartialEq for Identifier<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}
impl Eq for Identifier<'_> {}
impl PartialOrd for Identifier<'_> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.value.partial_cmp(other.value)
    }
}
impl Ord for Identifier<'_> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.value.cmp(other.value)
    }
}
impl Hash for Identifier<'_> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.value.hash(state);
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Number {
    pub value: u64,
    pos: usize,
}
impl Number {
    pub const fn len(&self) -> usize {
        let mut value = self.value;
        let mut count = 0;

        while value > 0 {
            value = value / 10;
            count += 1;
        }

        count
    }
}
impl Spanned for Number {
    fn span(&self) -> std::ops::Range<usize> {
        self.pos..(self.pos + self.len())
    }
}
impl<'s> Parse<'s> for Number {
    fn parse<'t>(
        tokens: TokensSlice<'s, 't>,
        errors: &mut Accumulator<impl From<ParseError>>,
    ) -> Option<(Self, TokensSlice<'s, 't>)> {
        if let Some((Token::Number(num), rest)) = tokens.split_first() {
            Some((*num, rest))
        } else {
            errors.push(ParseError::ExpectedToken("Number", tokens.span().start));
            None
        }
    }
}
parse_all_from_parse!(Number);
impl PartialEq for Number {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}
impl Eq for Number {}
impl PartialOrd for Number {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.value.partial_cmp(&other.value)
    }
}
impl Ord for Number {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.value.cmp(&other.value)
    }
}
impl Hash for Number {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.value.hash(state);
    }
}

macro_rules! puctuators {
    (
        $(
            $char:literal => $name:ident,
        )*
    ) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
        pub enum Punctuator {
            $($name($name),)*
        }
        impl Punctuator {
            pub fn len(&self)->usize {
                match self {
                    $(Punctuator::$name(v) => v.len(),)*
                }
            }
            pub fn char(&self)->char {
                match self {
                    $(Punctuator::$name(v) => v.char(),)*
                }
            }
            fn try_from_parts(ch:char, pos:usize)->Option<Self> {
                match ch {
                    $($char => Some(Punctuator::$name($name{pos})),)*
                    _=>None
                }
            }
        }
        impl Spanned for Punctuator {
            fn span(&self)->std::ops::Range<usize> {
                match self {
                    $(Punctuator::$name(v) => v.span(),)*
                }
            }
        }
        impl<'s> Parse<'s> for Punctuator {
            fn parse<'t>(
                tokens: TokensSlice<'s, 't>,
                errors: &mut Accumulator<impl From<ParseError>>,
            ) -> Option<(Self, TokensSlice<'s, 't>)> {
                if let Some((Token::Punctuator(punct), rest)) = tokens.split_first() {
                    Some((*punct, rest))
                } else {
                    errors.push(ParseError::ExpectedToken("Punctuator", tokens.span().start));
                    None
                }
            }
        }
        parse_all_from_parse!(Punctuator);
        impl From<Punctuator> for char {
            fn from(value:Punctuator) -> char {
                value.char()
            }
        }
        $(
            #[derive(Debug, Clone, Copy)]
            pub struct $name {
                pos: usize
            }
            impl $name {
                pub const fn len(&self)->usize {
                    $char.len_utf8()
                }
                pub const fn char(&self)->char {
                    $char
                }
            }
            impl Spanned for $name {
                fn span(&self) -> std::ops::Range<usize> {
                    self.pos..(self.pos + self.len())
                }
            }
            impl<'s> Parse<'s> for $name {
                fn parse<'t>(
                    tokens: TokensSlice<'s, 't>,
                    errors: &mut Accumulator<impl From<ParseError>>,
                ) -> Option<(Self, TokensSlice<'s, 't>)> {
                    if let Some((Token::Punctuator(Punctuator::$name(v)), rest)) = tokens.split_first() {
                        Some((*v, rest))
                    } else {
                        errors.push(ParseError::ExpectedToken(stringify!($name), tokens.span().start));
                        None
                    }
                }
            }
            parse_all_from_parse!($name);
            impl PartialEq for $name {
                fn eq(&self, _other: &Self) -> bool {
                    true
                }
            }
            impl Eq for $name {}
            impl PartialOrd for $name {
                fn partial_cmp(&self, _other: &Self) -> Option<std::cmp::Ordering> {
                    Some(std::cmp::Ordering::Equal)
                }
            }
            impl Ord for $name {
                fn cmp(&self, _other: &Self) -> std::cmp::Ordering {
                    std::cmp::Ordering::Equal
                }
            }
            impl Hash for $name {
                fn hash<H: std::hash::Hasher>(&self, _state: &mut H) {}
            }
        )*
    };
}

puctuators! {
    '+' => Plus,
    '-' => Minus,
    '*' => Mul,
    '/' => Div,
    '%' => Mod,
    ':' => Colon,
    ',' => Comma,
    '#' => Pound,
    '@' => At,
    '(' => ParOpen,
    ')' => ParClose,
    '\n' => Newline,
}

#[derive(Debug, Clone)]
pub enum Tokens<'s> {
    Tokens(NonEmptyVec<Token<'s>>),
    Empty { pos: usize },
}

impl<'s> Tokens<'s> {
    pub fn as_slice(&self) -> TokensSlice<'s, '_> {
        match self {
            Tokens::Tokens(tokens) => TokensSlice::Tokens(tokens.as_slice()),
            Tokens::Empty { pos } => TokensSlice::Empty { pos: *pos },
        }
    }
}
impl Spanned for Tokens<'_> {
    fn span(&self) -> std::ops::Range<usize> {
        self.as_slice().span()
    }
}

impl PartialEq for Tokens<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.as_slice().eq(&other.as_slice())
    }
}
impl Eq for Tokens<'_> {}
impl PartialOrd for Tokens<'_> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.as_slice().partial_cmp(&other.as_slice())
    }
}
impl Ord for Tokens<'_> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.as_slice().cmp(&other.as_slice())
    }
}
impl Hash for Tokens<'_> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.as_slice().hash(state)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum TokensSlice<'s, 't> {
    Tokens(&'t NonEmptySlice<Token<'s>>),
    Empty { pos: usize },
}

impl<'s, 't> TokensSlice<'s, 't> {
    fn from_slice_or_pos(slice: &'t [Token<'s>], pos: impl FnOnce() -> usize) -> Self {
        slice
            .try_into()
            .map(TokensSlice::Tokens)
            .unwrap_or_else(|_| TokensSlice::Empty { pos: pos() })
    }

    pub fn tokens(&self) -> &'t [Token<'s>] {
        match self {
            TokensSlice::Tokens(tokens) => tokens,
            TokensSlice::Empty { .. } => &[],
        }
    }

    pub fn split_first(&self) -> Option<(&'t Token<'s>, TokensSlice<'s, 't>)> {
        match self {
            TokensSlice::Tokens(tokens) => {
                let (t, rest) = tokens.split_first();
                Some((t, Self::from_slice_or_pos(rest, || t.span().end)))
            }
            TokensSlice::Empty { .. } => None,
        }
    }
    pub(crate) fn split_last(&self) -> Option<(&'t Token<'s>, TokensSlice<'s, 't>)> {
        match self {
            TokensSlice::Tokens(tokens) => {
                let (t, rest) = tokens.split_last();
                Some((t, Self::from_slice_or_pos(rest, || t.span().start)))
            }
            TokensSlice::Empty { .. } => None,
        }
    }

    pub fn split_at(&self, mid: usize) -> (TokensSlice<'s, 't>, TokensSlice<'s, 't>) {
        match self {
            TokensSlice::Tokens(tokens) => {
                let (a, b) = tokens.split_at(mid);
                (
                    Self::from_slice_or_pos(a, || tokens.first().span().start),
                    Self::from_slice_or_pos(b, || tokens.last().span().end),
                )
            }
            TokensSlice::Empty { pos } => todo!(),
        }
    }

    /// Returns `true` if the tokens slice is [`Empty`].
    ///
    /// [`Empty`]: TokensSlice::Empty
    #[must_use]
    pub fn is_empty(&self) -> bool {
        matches!(self, Self::Empty { .. })
    }

    pub fn split_newlines(&self) -> impl Iterator<Item = TokensSlice<'s, 't>> {
        self.tokens()
            .split(|t| matches!(t, Token::Punctuator(Punctuator::Newline(_))))
            .flat_map(|l| l.try_into().map(TokensSlice::Tokens).ok())
    }
}
impl Spanned for TokensSlice<'_, '_> {
    fn span(&self) -> std::ops::Range<usize> {
        match self {
            TokensSlice::Tokens(tokens) => tokens.first().span().start..tokens.last().span().end,
            TokensSlice::Empty { pos } => *pos..*pos,
        }
    }
}

impl PartialEq for TokensSlice<'_, '_> {
    fn eq(&self, other: &Self) -> bool {
        self.tokens().eq(other.tokens())
    }
}
impl Eq for TokensSlice<'_, '_> {}
impl PartialOrd for TokensSlice<'_, '_> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.tokens().partial_cmp(other.tokens())
    }
}
impl Ord for TokensSlice<'_, '_> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.tokens().cmp(other.tokens())
    }
}
impl Hash for TokensSlice<'_, '_> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.tokens().hash(state);
    }
}

#[derive(Debug, Clone, Copy, Error)]
pub enum TokenizeError {
    #[error("Unknow character")]
    UnknowChar(char, usize),
    #[error("Integer literal is too long to fit")]
    IntegerLiteralTooLong(usize, usize),
    #[error("Escape without newline")]
    EscapeWithoutNewline(usize),
}

impl Spanned for TokenizeError {
    fn span(&self) -> std::ops::Range<usize> {
        match self {
            TokenizeError::UnknowChar(ch, pos) => *pos..(pos + ch.len_utf8()),
            TokenizeError::IntegerLiteralTooLong(start, end) => *start..*end,
            TokenizeError::EscapeWithoutNewline(pos) => *pos..(pos + '\\'.len_utf8()),
        }
    }
}

impl SourceError for TokenizeError {
    fn severity(&self) -> errors::Severity {
        errors::Severity::Error
    }
}

pub fn tokenize<'s>(
    source: &'s str,
    errors: &mut Accumulator<impl From<TokenizeError>>,
) -> Tokens<'s> {
    let mut tokens = vec![];
    let mut chars = source.char_indices();
    while !chars.as_str().is_empty() {
        // remove all non-newline whitespace
        for _ in chars.peeking_take_while(|(_, ch)| ch.is_whitespace() && *ch != '\n') {}

        let Some((start_idx, start_ch)) = chars.next() else {
            break;
        };
        match start_ch {
            'a'..='z' | 'A'..='Z' | '_' => {
                // consume identifier
                for _ in chars.peeking_take_while(
                    |(_, ch)| matches!(ch, 'a'..='z' | 'A'..='Z' | '0'..='9' | '_'),
                ) {}
                let end_idx = chars.offset();
                tokens.push(Token::Identifier(Identifier {
                    value: &source[start_idx..end_idx],
                    pos: start_idx,
                }))
            }
            '0'..='9' => {
                // consume number
                for _ in chars.peeking_take_while(|(_, ch)| matches!(ch, '0'..='9')) {}
                let end_idx = chars.offset();
                match source[start_idx..end_idx].parse() {
                    Ok(value) => tokens.push(Token::Number(Number {
                        value,
                        pos: start_idx,
                    })),
                    Err(_) => errors.push(TokenizeError::IntegerLiteralTooLong(start_idx, end_idx)),
                }
            }
            ';' => {
                // consume all until newline
                for _ in chars.peeking_take_while(|(_, ch)| *ch != '\n') {}
            }
            '\\' => {
                // consume all non newline whitespace
                for _ in chars.peeking_take_while(|(_, ch)| ch.is_whitespace() && *ch != '\n') {}
                if chars.clone().next().is_some_and(|(_, ch)| ch == '\n') {
                    // consume newline
                    chars.next().unwrap();
                    // do not add any token
                } else {
                    errors.push(TokenizeError::EscapeWithoutNewline(start_idx))
                }
            }
            _ => {
                if let Some(punct) = Punctuator::try_from_parts(start_ch, start_idx) {
                    // skip starting newline and duplicate ones
                    if !(matches!(punct, Punctuator::Newline(_))
                        && matches!(
                            tokens.last(),
                            None | Some(Token::Punctuator(Punctuator::Newline(_)))
                        ))
                    {
                        tokens.push(Token::Punctuator(punct))
                    }
                } else {
                    errors.push(TokenizeError::UnknowChar(start_ch, start_idx))
                }
            }
        }
    }
    // skip ending newlines
    {
        let ending_newlines = tokens
            .iter()
            .rev()
            .take_while(|t| matches!(t, Token::Punctuator(Punctuator::Newline(_))))
            .count();
        tokens.truncate(tokens.len() - ending_newlines)
    }
    match NonEmptyVec::try_from(tokens) {
        Ok(tokens) => Tokens::Tokens(tokens),
        Err(_) => Tokens::Empty { pos: 0 },
    }
}
