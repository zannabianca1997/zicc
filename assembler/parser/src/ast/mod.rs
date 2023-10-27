use std::{
    collections::BTreeSet,
    fmt::Display,
    mem,
    ops::{AddAssign, Range},
};

use arrayvec::ArrayVec;
use either::Either::{self};
use itertools::Itertools;
use lalrpop_util::{lalrpop_mod, ErrorRecovery};
use map_in_place::MapBoxInPlace;
use thiserror::Error;

use errors::{Accumulator, Spanned};
use lexer::{Identifier, LexError, Lexer, SpecialIdentifier, StringLit, Token};
use vm::VMInt;

type AstErrorRecovery<'s> = ErrorRecovery<usize, Token<'s>, ParseErrorContent>;
lalrpop_mod!(grammar);

#[derive(Debug, Error)]
pub enum ParseErrorContent {
    #[error(transparent)]
    Lex(#[from] LexError),
    #[error("Immediate mode is invalid in a write param")]
    ImmediateInWrite { span: Range<usize> },
    #[error("Labels are valid only on parameters that corrispond to a single memory location")]
    LabelsOnUnlabelled { span: Range<usize> },
}
impl Spanned for ParseErrorContent {
    fn span(&self) -> Range<usize> {
        match self {
            ParseErrorContent::Lex(err) => err.span(),
            ParseErrorContent::ImmediateInWrite { span } => span.clone(),
            ParseErrorContent::LabelsOnUnlabelled { span } => span.clone(),
        }
    }
}

#[derive(Debug, Error)]
#[error(transparent)]
pub struct ParseError(#[from] lalrpop_util::ParseError<usize, String, ParseErrorContent>);
impl From<lalrpop_util::ParseError<usize, Token<'_>, ParseErrorContent>> for ParseError {
    fn from(value: lalrpop_util::ParseError<usize, Token<'_>, ParseErrorContent>) -> Self {
        Self(value.map_token(|t| t.to_string()))
    }
}
impl From<lalrpop_util::ErrorRecovery<usize, Token<'_>, ParseErrorContent>> for ParseError {
    fn from(value: lalrpop_util::ErrorRecovery<usize, Token<'_>, ParseErrorContent>) -> Self {
        value.error.into()
    }
}

impl Spanned for ParseError {
    fn span(&self) -> Range<usize> {
        match self {
            ParseError(lalrpop_util::ParseError::InvalidToken { location })
            | ParseError(lalrpop_util::ParseError::UnrecognizedEof { location, .. }) => {
                *location..*location
            }
            ParseError(lalrpop_util::ParseError::UnrecognizedToken {
                token: (start, _, end),
                ..
            })
            | ParseError(lalrpop_util::ParseError::ExtraToken {
                token: (start, _, end),
            }) => *start..*end,
            ParseError(lalrpop_util::ParseError::User { error }) => error.span(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct File<'s, Error = !> {
    pub statements: Vec<Labelled<'s, Option<Statement<'s, Error>>>>,
}

impl<'s> File<'s> {
    pub fn parse(
        source: &'s str,
        errors: &mut impl Accumulator<Error = ParseError>,
    ) -> Option<File<'s>> {
        errors
            .handle(
                grammar::FileParser::new()
                    .parse(Lexer::new(source).map(|t| t.map_err(Into::into)))
                    .map_err(ParseError::from),
            )?
            .extract_errs(errors)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Labelled<'s, T> {
    pub labels: BTreeSet<LabelDef<'s>>,
    pub content: T,
}

impl<'s, T> Labelled<'s, T> {
    pub fn is_labelled(&self) -> bool {
        !self.labels.is_empty()
    }

    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> Labelled<'s, U> {
        Labelled {
            labels: self.labels,
            content: f(self.content),
        }
    }

    pub fn map_in_place(&mut self, f: impl FnOnce(&mut T)) {
        f(&mut self.content)
    }
}
impl<'s, T> Labelled<'s, Option<T>> {
    pub fn transpose(self) -> Option<Labelled<'s, T>> {
        match self {
            Labelled {
                labels,
                content: Some(content),
            } => Some(Labelled { labels, content }),
            Labelled {
                labels: _,
                content: None,
            } => None,
        }
    }
}

impl<'s, T: std::ops::Deref> std::ops::Deref for Labelled<'s, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.content
    }
}

impl<'s, T> From<T> for Labelled<'s, T> {
    fn from(value: T) -> Self {
        Self {
            labels: BTreeSet::new(),
            content: value,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LabelDef<'s> {
    pub label: Identifier<'s>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum LabelRef<'s, Error = !> {
    Identifier(Identifier<'s>),
    SpecialIdentifier(SpecialIdentifier),
    Error(Error),
}
impl Display for LabelRef<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LabelRef::Identifier(id) => <Identifier as Display>::fmt(id, f),
            LabelRef::SpecialIdentifier(si) => <SpecialIdentifier as Display>::fmt(si, f),
            LabelRef::Error(e) => *e,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Statement<'s, Error = !> {
    Ints(IntsStm<'s, Error>),
    Instruction(Instruction<'s, Error>),
    Inc(IncStm<'s, Error>),
    Dec(DecStm<'s, Error>),
    Jmp(JmpStm<'s, Error>),
    Mov(MovStm<'s, Error>),
    Zeros(ZerosStm<'s, Error>),
    Push(PushStm<'s, Error>),
    Pop(PopStm<'s, Error>),
    Call(CallStm<'s, Error>),
    Ret(RetStm),
    Export(ExportStm<'s>),
    Entry(EntryStm<'s>),
    Error(Error),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Instruction<'s, Error = !> {
    Add(
        ReadParam<'s, Error>,
        ReadParam<'s, Error>,
        WriteParam<'s, Error>,
    ),
    Mul(
        ReadParam<'s, Error>,
        ReadParam<'s, Error>,
        WriteParam<'s, Error>,
    ),
    In(WriteParam<'s, Error>),
    Out(ReadParam<'s, Error>),
    Jnz(ReadParam<'s, Error>, ReadParam<'s, Error>),
    Jz(ReadParam<'s, Error>, ReadParam<'s, Error>),
    Slt(
        ReadParam<'s, Error>,
        ReadParam<'s, Error>,
        WriteParam<'s, Error>,
    ),
    Seq(
        ReadParam<'s, Error>,
        ReadParam<'s, Error>,
        WriteParam<'s, Error>,
    ),
    Incb(ReadParam<'s, Error>),
    Halt,
    Error(Error),
}
impl<'s> Instruction<'s> {
    pub fn opcode(&self) -> VMInt {
        match self {
            Instruction::Add(_, _, _) => 1,
            Instruction::Mul(_, _, _) => 2,
            Instruction::In(_) => 3,
            Instruction::Out(_) => 4,
            Instruction::Jnz(_, _) => 5,
            Instruction::Jz(_, _) => 6,
            Instruction::Slt(_, _, _) => 7,
            Instruction::Seq(_, _, _) => 8,
            Instruction::Incb(_) => 9,
            Instruction::Halt => 99,
            Instruction::Error(e) => *e,
        }
    }

    pub fn into_param_values(self) -> ArrayVec<Labelled<'s, Box<Expression<'s>>>, 3> {
        match self {
            Instruction::Add(a, b, c)
            | Instruction::Mul(a, b, c)
            | Instruction::Slt(a, b, c)
            | Instruction::Seq(a, b, c) => {
                ArrayVec::from([a.into_value(), b.into_value(), c.into_value()])
            }
            Instruction::In(a) => ArrayVec::try_from([a.into_value()].as_slice()).unwrap(),
            Instruction::Out(a) | Instruction::Incb(a) => {
                ArrayVec::try_from([a.into_value()].as_slice()).unwrap()
            }
            Instruction::Jz(a, b) | Instruction::Jnz(a, b) => {
                ArrayVec::try_from([a.into_value(), b.into_value()].as_slice()).unwrap()
            }
            Instruction::Halt => ArrayVec::new(),
            Instruction::Error(e) => e,
        }
    }

    pub fn param_modes(&self) -> ArrayVec<VMInt, 3> {
        match self {
            Instruction::Add(a, b, c)
            | Instruction::Mul(a, b, c)
            | Instruction::Slt(a, b, c)
            | Instruction::Seq(a, b, c) => ArrayVec::from([a.mode(), b.mode(), c.mode()]),
            Instruction::In(a) => ArrayVec::try_from([a.mode()].as_slice()).unwrap(),
            Instruction::Out(a) | Instruction::Incb(a) => {
                ArrayVec::try_from([a.mode()].as_slice()).unwrap()
            }
            Instruction::Jz(a, b) | Instruction::Jnz(a, b) => {
                ArrayVec::try_from([a.mode(), b.mode()].as_slice()).unwrap()
            }
            Instruction::Halt => ArrayVec::new(),
            Instruction::Error(e) => *e,
        }
    }
}

pub use statements::*;
mod statements;

pub use params::*;
mod params;

pub use expression::*;
mod expression;

pub use ast_nodes::AstNode;
mod ast_nodes;
