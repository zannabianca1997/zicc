use std::{collections::BTreeSet};

use crate::lexer::{Spanned};

use super::lexer::tokens::*;

use super::{Error, Parse};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct File<'i> {
    pub lines: Vec<Line<'i>>,
}
impl<'i> Parse<'i> for File<'i> {
    fn parse<'a>(tokens: &'a [Token<'i>]) -> Result<(Self, &'a [Token<'i>]), Vec<super::Error>> {
        Ok((
            Self {
                lines: Line::parse_splitted(tokens, |t| {
                    matches!(t, Token::Punctuator(Punctuator::Newline(_)))
                })?,
            },
            &[],
        ))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Line<'i> {
    pub labels: BTreeSet<Identifier<'i>>,
    pub statement: Option<Statement>,
}
impl<'i> Parse<'i> for Line<'i> {
    fn parse<'a>(tokens: &'a [Token<'i>]) -> Result<(Self, &'a [Token<'i>]), Vec<super::Error>> {
        let (labels, tokens) = <Vec<(Identifier, Colon)>>::parse(tokens)?;
        let labels = labels.into_iter().map(|(l, _)| l).collect();
        Ok((
            Self {
                labels,
                statement: <Option<Statement>>::parse_all(tokens)?,
            },
            &[],
        ))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Statement {
    Ints(Ints),
}
impl<'i> Parse<'i> for Statement {
    fn parse<'a>(tokens: &'a [Token<'i>]) -> Result<(Self, &'a [Token<'i>]), Vec<Error>>
    where
        Self: Sized,
    {
        let (ident, tokens) = <Identifier<'_>>::parse(tokens)?;
        match ident.as_str() {
            "ints" => {
                let ints = Ints::parse_all(tokens)?;
                Ok((Self::Ints(ints), &[]))
            }
            _ => Err(vec![Error::Expected("command", ident.span())]),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Ints {
    pub values: Vec<Number>,
}

impl<'i> Parse<'i> for Ints {
    fn parse<'a>(tokens: &'a [Token<'i>]) -> Result<(Self, &'a [Token<'i>]), Vec<Error>>
    where
        Self: Sized,
    {
        let (values, tokens) = Parse::parse(tokens)?;
        Ok((Self { values }, tokens))
    }
}
