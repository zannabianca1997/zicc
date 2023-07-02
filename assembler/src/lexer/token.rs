use std::fmt::Display;

use super::{Span, Spanned};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Token<'i> {
    Number(u64, usize),
    Identifier(&'i str, usize),
    Punctuator(Punctuator, usize),
}

impl Token<'_> {
    /// Parse a single token
    ///
    /// * `s` - The string to parse
    /// * `start` - start index of the string
    #[must_use]
    #[inline]
    pub(super) fn parse(s: &str, start: usize) -> Result<Token<'_>, super::Error> {
        if s.len() == 1 {
            if let Ok(punct) = Punctuator::try_from(s.as_bytes()[0]) {
                return Ok(Token::Punctuator(punct, start));
            }
        }
        if matches!(s.as_bytes()[0], b'0'..=b'9') {
            if let Ok(n) = s.parse() {
                return Ok(Token::Number(n, start));
            }
        }
        if matches!(s.as_bytes()[0], b'a'..=b'z' | b'A'..=b'Z' | b'_') {
            if s.bytes()
                .all(|b| matches!(b, b'a'..=b'z' | b'A'..=b'Z'|b'0'..=b'9' | b'_'))
            {
                return Ok(Token::Identifier(s, start));
            }
        }
        Err(super::Error::UnknownToken(Span::new(
            start,
            start + s.len(),
        )))
    }
}

impl Spanned for Token<'_> {
    #[must_use]
    #[inline]
    fn span(&self) -> Span {
        match self {
            Token::Number(n, pos) => {
                let mut len = 0;
                let mut n = *n;
                while n > 0 {
                    len += 1;
                    n /= 10;
                }
                Span::new(*pos, pos + len)
            }
            Token::Identifier(s, pos) => Span::new(*pos, pos + s.len()),
            Token::Punctuator(_, pos) => Span::new(*pos, pos + 1),
        }
    }
}

impl Display for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Number(n, _) => write!(f, "{n}"),
            Token::Identifier(s, _) => write!(f, "{s}"),
            Token::Punctuator(p, _) => write!(f, "{p}"),
        }
    }
}
impl PartialEq<&str> for Token<'_> {
    fn eq(&self, other: &&str) -> bool {
        if self.span().len() == other.len() {
            match self {
                Token::Number(n, _) => n.to_string() == *other,
                Token::Identifier(s, _) => s == other,
                Token::Punctuator(p, _) => other.as_bytes()[0] == *p as u8,
            }
        } else {
            false
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
            $($var = $val ,)*
        }

        impl From<$name> for u8 {
            #[must_use]
            #[inline]
            fn from(value: $name)-> u8 {
                value as u8
            }
        }
        impl TryFrom<u8> for $name {
            type Error = u8;
            #[must_use]
            #[inline]
            fn try_from(value: u8)-> Result<$name, Self::Error> {
                match value {
                    $($val => Ok($name :: $var) ,)*
                    _ => Err(value),
                }
            }
        }
        impl Display for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    $($name :: $var => write!(f, "{}", $val as char),)*
                }
            }
        }
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

            use crate::lexer::{Punctuator, Span, Spanned, Token};

            #[test]
            fn identifier() {
                for s in ["hello", "HELO", "_1", "Hey1", "_"] {
                    let t = Token::parse(s, 3).unwrap();
                    assert_matches!(t, Token::Identifier(..));
                    assert_eq!(t.to_string(), s);
                    assert_eq!(t.span(), Span::new(3, 3 + s.len()))
                }
            }

            #[test]
            fn number() {
                let t = Token::parse("123", 3).unwrap();
                assert_matches!(t, Token::Number(..));
                assert_eq!(t.to_string(), "123");
                assert_eq!(t.span(), Span::new(3, 3 + "123".len()))
            }

            #[test]
            fn punctuators() {
                for p in Punctuator::iter() {
                    let c = [p as u8];
                    let s = std::str::from_utf8(&c).unwrap();
                    let t = Token::parse(s, 3).unwrap();
                    assert_matches!(t, Token::Punctuator(punct,_) if punct == p);
                    assert_eq!(t.to_string(), s);
                    assert_eq!(t.span(), Span::new(3, 3 + 1))
                }
            }
        }
    }
}
