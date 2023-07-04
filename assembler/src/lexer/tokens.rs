use std::fmt::Display;

use crate::parser::{self, Parse};

use super::{Span, Spanned};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Token<'i> {
    Number(Number),
    Identifier(Identifier<'i>),
    Punctuator(Punctuator),
}

impl Token<'_> {
    /// Parse a single token
    ///
    /// * `s` - The string to parse
    /// * `start` - start index of the string
    #[must_use]
    #[inline]
    pub(super) fn lex(s: &str, start: usize) -> Result<Token<'_>, super::Error> {
        if s.len() == 1 {
            if let Some(punct) = Punctuator::try_from_u8(s.as_bytes()[0], start) {
                return Ok(Token::Punctuator(punct));
            }
        }
        if matches!(s.as_bytes()[0], b'0'..=b'9') {
            if let Ok(n) = s.parse() {
                return Ok(Token::Number(Number(n, start)));
            }
        }
        if matches!(s.as_bytes()[0], b'a'..=b'z' | b'A'..=b'Z' | b'_') {
            if s.bytes()
                .all(|b| matches!(b, b'a'..=b'z' | b'A'..=b'Z'|b'0'..=b'9' | b'_'))
            {
                return Ok(Token::Identifier(Identifier(s, start)));
            }
        }
        Err(super::Error::UnknownToken(Span::new(
            start,
            start + s.len(),
        )))
    }
}
impl Spanned for Token<'_> {
    fn span(&self) -> Span {
        match self {
            Token::Number(n) => n.span(),
            Token::Identifier(i) => i.span(),
            Token::Punctuator(p) => p.span(),
        }
    }
}
impl Display for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Number(n) => write!(f, "{n}"),
            Token::Identifier(i) => write!(f, "{i}"),
            Token::Punctuator(p) => write!(f, "{p}"),
        }
    }
}
impl PartialEq<&str> for Token<'_> {
    fn eq(&self, other: &&str) -> bool {
        match self {
            Token::Number(n) => n == other,
            Token::Identifier(i) => i == other,
            Token::Punctuator(p) => p == other,
        }
    }
}
impl<'i> Parse<'i> for Token<'i> {
    fn parse<'a>(
        tokens: &'a [Token<'i>],
    ) -> Result<(Self, &'a [Token<'i>]), Vec<crate::parser::Error>>
    where
        Self: Sized,
    {
        match tokens.split_first() {
            Some((t, ts)) => Ok((*t, ts)),
            None => Err(vec![parser::Error::Expected("token", tokens.span())]),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Identifier<'i>(&'i str, usize);
impl<'i> Identifier<'i> {
    pub(crate) fn as_str(&self) -> &'i str {
        self.0
    }
}

impl Spanned for Identifier<'_> {
    fn span(&self) -> Span {
        Span::new(self.1, self.1 + self.0.len())
    }
}
impl Display for Identifier<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}
impl PartialEq for Identifier<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}
impl Eq for Identifier<'_> {}
impl PartialEq<&str> for Identifier<'_> {
    fn eq(&self, other: &&str) -> bool {
        self.0 == *other
    }
}
impl PartialOrd for Identifier<'_> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.0.partial_cmp(&other.0)
    }
}
impl Ord for Identifier<'_> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.cmp(&other.0)
    }
}

impl<'i> Parse<'i> for Identifier<'i> {
    fn parse<'a>(
        tokens: &'a [Token<'i>],
    ) -> Result<(Self, &'a [Token<'i>]), Vec<crate::parser::Error>>
    where
        Self: Sized,
    {
        match tokens.split_first() {
            Some((Token::Identifier(ident), ts)) => Ok((*ident, ts)),
            Some((t, _)) => Err(vec![parser::Error::Expected("identifier", t.span())]),
            None => Err(vec![parser::Error::Expected("identifier", tokens.span())]),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Number(u64, usize);
impl Spanned for Number {
    fn span(&self) -> Span {
        Span::new(self.1, self.1 + self.0.to_string().len())
    }
}
impl Display for Number {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}
impl PartialEq for Number {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}
impl Eq for Number {}
impl PartialEq<&str> for Number {
    fn eq(&self, other: &&str) -> bool {
        self.0.to_string() == *other
    }
}
impl PartialEq<u64> for Number {
    fn eq(&self, other: &u64) -> bool {
        self.0 == *other
    }
}
impl PartialOrd for Number {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.0.partial_cmp(&other.0)
    }
}
impl Ord for Number {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.cmp(&other.0)
    }
}
impl<'i> Parse<'i> for Number {
    fn parse<'a>(
        tokens: &'a [Token<'i>],
    ) -> Result<(Self, &'a [Token<'i>]), Vec<crate::parser::Error>>
    where
        Self: Sized,
    {
        match tokens.split_first() {
            Some((Token::Number(num), ts)) => Ok((*num, ts)),
            Some((t, _)) => Err(vec![parser::Error::Expected("number", t.span())]),
            None => Err(vec![parser::Error::Expected("number", tokens.span())]),
        }
    }
}

macro_rules! puctuators {
    ( pub enum $name: ident {
        $($var: ident = $val: literal ,)*
    }) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq)]
        #[cfg_attr(test, derive(::strum::EnumIter))]
        #[repr(u8)]
        pub enum $name {
            $($var($var) ,)*
        }

        impl From<$name> for u8 {
            #[must_use]
            #[inline]
            fn from(value: $name)-> u8 {
                match value {
                    $($name :: $var ( _ ) => $val ,)*
                }
            }
        }
        impl From<&$name> for u8 {
            #[must_use]
            #[inline]
            fn from(value: &$name)-> u8 {
                match value {
                    $($name :: $var ( _ ) => $val ,)*
                }
            }
        }
        impl  $name {
            #[must_use]
            #[inline]
            fn try_from_u8(value: u8, pos:usize)-> Option<$name> {
                match value {
                    $($val => Some($name :: $var ( $var (pos) )) ,)*
                    _ => None,
                }
            }
        }
        impl Display for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    $($name :: $var ( p ) => write!(f, "{p}"),)*
                }
            }
        }
        impl Spanned for $name {
            fn span(&self) -> Span {
                match self {
                    $($name :: $var ( p ) => p.span(),)*
                }
            }
        }

        impl PartialEq<&str> for $name {
            fn eq(&self, other: &&str) -> bool {
                other.len() == 1 && other.as_bytes()[0] == self.into()
            }
        }

        impl<'i> Parse<'i> for $name {
            fn parse<'a>(
                tokens: &'a [Token<'i>],
            ) -> Result<(Self, &'a [Token<'i>]), Vec<crate::parser::Error>>
            where
                Self: Sized,
            {
                match tokens.split_first() {
                    Some((Token::Punctuator(p), ts)) => Ok((*p, ts)),
                    Some((t, _)) => Err(vec![parser::Error::Expected("punctuator", t.span())]),
                    None => Err(vec![parser::Error::Expected("punctuator", tokens.span())]),
                }
            }
        }

        $(
            #[derive(Debug, Clone, Copy)]
            #[cfg_attr(test, derive(Default))]
            pub struct $var(usize);
            impl Display for $var {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    write!(f,"{}", $val as char)
                }
            }

            impl PartialEq for $var {
                fn eq(&self, _: &Self) -> bool { true }
            }
            impl Eq for $var {}

            impl Spanned for $var {
                fn span(&self) -> Span {
                    Span::new(self.0, self.0 + 1)
                }
            }

            impl<'i> Parse<'i> for $var {
                fn parse<'a>(
                    tokens: &'a [Token<'i>],
                ) -> Result<(Self, &'a [Token<'i>]), Vec<crate::parser::Error>>
                where
                    Self: Sized,
                {
                    match tokens.split_first() {
                        Some((Token::Punctuator($name::$var(p)), ts)) => Ok((*p, ts)),
                        Some((t, _)) => Err(vec![parser::Error::Expected(stringify!($var), t.span())]),
                        None => Err(vec![parser::Error::Expected(stringify!($var), tokens.span())]),
                    }
                }
            }


        )*
    };
}

puctuators! {
    pub enum Punctuator {
        Comma = b',',
        Colon = b':',
        Plus = b'+',
        Minus = b'-',
        Star = b'*',
        Slash = b'/',
        Hashtag = b'#',
        At = b'@',
        ParenthesesOpen = b'(',
        ParenthesesClose = b')',
        BracketOpen = b'[',
        BracketClose = b']',
        BraceOpen = b'{',
        BraceClose = b'}',
        Newline = b'\n',
    }
}

#[cfg(test)]
mod tests {
    mod token {
        mod parse {
            use std::assert_matches::assert_matches;

            use strum::IntoEnumIterator;

            use crate::lexer::{tokens::Punctuator, Span, Spanned, Token};

            #[test]
            fn identifier() {
                for s in ["hello", "HELO", "_1", "Hey1", "_"] {
                    let t = Token::lex(s, 3).unwrap();
                    assert_matches!(t, Token::Identifier(..));
                    assert_eq!(t.to_string(), s);
                    assert_eq!(t.span(), Span::new(3, 3 + s.len()))
                }
            }

            #[test]
            fn number() {
                let t = Token::lex("123", 3).unwrap();
                assert_matches!(t, Token::Number(..));
                assert_eq!(t.to_string(), "123");
                assert_eq!(t.span(), Span::new(3, 3 + "123".len()))
            }

            #[test]
            fn punctuators() {
                for p in Punctuator::iter() {
                    let c = [p.into()];
                    let s = std::str::from_utf8(&c).unwrap();
                    let t = Token::lex(s, 3).unwrap();
                    assert_matches!(t, Token::Punctuator(punct) if punct == p);
                    assert_eq!(t.to_string(), s);
                    assert_eq!(t.span(), Span::new(3, 3 + 1))
                }
            }
        }
    }
}
