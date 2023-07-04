

use thiserror::Error;
use utils::collect_oks_or_errs::CollectOksOrFlatErrs;

use crate::lexer::{self, Span, Spanned, Token};

#[derive(Debug, Error)]
pub enum Error {
    #[error(transparent)]
    Lex(#[from] super::lexer::Error),
    #[error("Expected {0}")]
    Expected(&'static str, Span),
    #[error("Unexpected token(s)")]
    Unexpected(Span),
    #[error("Unknow directive")]
    UnknowDirective(Span),
}
impl Spanned for Error {
    fn span(&self) -> crate::lexer::Span {
        match self {
            Error::Lex(err) => err.span(),
            Error::Unexpected(span) | Error::Expected(_, span) | Error::UnknowDirective(span) => {
                *span
            }
        }
    }
}

/// Something that can be parsed
pub(crate) trait Parse<'i> {
    fn parse<'a>(tokens: &'a [Token<'i>]) -> Result<(Self, &'a [Token<'i>]), Vec<Error>>
    where
        Self: Sized;

    fn parse_all(tokens: &[Token<'i>]) -> Result<Self, Vec<Error>>
    where
        Self: Sized,
    {
        let (parsed, remains) = Self::parse(tokens)?;
        if !remains.is_empty() {
            return Err(vec![Error::Unexpected(remains.span())]);
        }
        Ok(parsed)
    }
    fn parse_splitted<C, F>(tokens: &[Token<'i>], pred: F) -> Result<C, Vec<Error>>
    where
        Self: Sized,
        C: Default + Extend<Self>,
        F: FnMut(&Token<'i>) -> bool,
    {
        tokens
            .split(pred)
            .map(Self::parse_all)
            .collect_oks_or_flat_errs()
    }
}

macro_rules! parse_tuple_impl {
    (
        $(
        ( $($t:ident,)* ),
        )*
    ) => {
        $(
            impl<'i, $($t,)*> Parse<'i> for ($($t,)*)
            where
                $($t : Parse<'i>,)*
            {
                #[allow(non_snake_case)]
                fn parse<'a>(tokens: &'a [Token<'i>]) -> Result<(Self, &'a [Token<'i>]), Vec<Error>>
                where
                    Self: Sized,
                {
                    $(let ($t, tokens) = $t::parse(tokens)?;)*
                    Ok((($($t,)*), tokens))
                }
            }
        )*
    };
}
parse_tuple_impl! {(), (A,), (A,B,),(A,B,C,),(A,B,C,D,),(A,B,C,D,E,),}

impl<'i, T> Parse<'i> for Vec<T>
where
    T: Parse<'i>,
{
    fn parse<'a>(mut tokens: &'a [Token<'i>]) -> Result<(Self, &'a [Token<'i>]), Vec<Error>>
    where
        Self: Sized,
    {
        let mut collected = vec![];
        while let Ok((p, t)) = T::parse(tokens) {
            collected.push(p);
            tokens = t;
        }
        Ok((collected, tokens))
    }

    fn parse_all(mut tokens: &[Token<'i>]) -> Result<Self, Vec<Error>>
    where
        Self: Sized,
    {
        let mut collected = vec![];
        while !tokens.is_empty() {
            let (p, t) = T::parse(tokens)?;
            collected.push(p);
            tokens = t;
        }
        Ok(collected)
    }
}

impl<'i, T> Parse<'i> for Option<T>
where
    T: Parse<'i>,
{
    fn parse<'a>(tokens: &'a [Token<'i>]) -> Result<(Self, &'a [Token<'i>]), Vec<Error>>
    where
        Self: Sized,
    {
        match T::parse(tokens) {
            Ok((t, tokens)) => Ok((Some(t), tokens)),
            Err(_) => Ok((None, tokens)),
        }
    }

    fn parse_all(tokens: &[Token<'i>]) -> Result<Self, Vec<Error>>
    where
        Self: Sized,
    {
        match T::parse(tokens) {
            Ok((t, tokens)) if tokens.len() == 0 => Ok(Some(t)),
            Ok((_, tokens)) => Err(vec![Error::Unexpected(tokens.span())]),
            Err(errs) => Err(errs),
        }
    }
}

mod ast;

pub fn parse<'i>(tokens: &[Token<'i>]) -> Result<ast::File<'i>, Vec<Error>> {
    ast::File::parse_all(tokens)
}

#[cfg(test)]
mod tests {
    use std::assert_matches::assert_matches;

    use crate::lexer::lex;

    use super::{
        ast::{File, Ints, Line, Statement},
        parse,
    };

    #[test]
    fn ints() {
        let input = "ints 3 4 2";
        let tokens = lex(input).unwrap();
        let File { lines } = parse(&tokens).unwrap();
        assert_eq!(lines.len(), 1);
        assert_matches!(
            &lines[0],
            Line {
                labels,
                statement: Some(Statement::Ints(Ints { values}))
            }
            if labels.len() == 0
            && values.as_slice() == &[3,4,2]
        );
    }
}
