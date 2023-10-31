use std::{
    collections::BTreeSet,
    fmt::Display,
    mem,
    ops::{AddAssign, Range},
};

use arrayvec::ArrayVec;
use bincode::{BorrowDecode, Decode, Encode};
use itertools::Itertools;
use map_in_place::MapBoxInPlace;
use serde::{Deserialize, Serialize};
use thiserror::Error;

#[cfg(feature = "parse")]
use lalrpop_util::{lalrpop_mod, ErrorRecovery};

use errors::{Accumulator, Spanned};
use lexer::{Identifier, SpecialIdentifier, StringLit};

#[cfg(feature = "parse")]
use lexer::{LexError, Lexer, Token};

use vm::VMInt;

#[cfg(feature = "parse")]
lalrpop_mod!(grammar);

#[derive(Debug, Error)]
pub enum ParseErrorContent {
    #[cfg(feature = "parse")]
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
            #[cfg(feature = "parse")]
            ParseErrorContent::Lex(err) => err.span(),
            ParseErrorContent::ImmediateInWrite { span } => span.clone(),
            ParseErrorContent::LabelsOnUnlabelled { span } => span.clone(),
        }
    }
}

#[cfg(feature = "parse")]
type AstErrorRecovery<'s> = ErrorRecovery<usize, Token<'s>, ParseErrorContent>;

#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize, Decode, Encode,
)]
pub enum Infallible {}
impl From<Infallible> for ! {
    fn from(_: Infallible) -> Self {
        unreachable!()
    }
}

#[cfg(feature = "parse")]
#[derive(Debug, Error)]
#[error(transparent)]
pub struct ParseError(#[from] lalrpop_util::ParseError<usize, String, ParseErrorContent>);
#[cfg(feature = "parse")]
impl From<lalrpop_util::ParseError<usize, Token<'_>, ParseErrorContent>> for ParseError {
    fn from(value: lalrpop_util::ParseError<usize, Token<'_>, ParseErrorContent>) -> Self {
        Self(value.map_token(|t| t.to_string()))
    }
}
#[cfg(feature = "parse")]
impl From<lalrpop_util::ErrorRecovery<usize, Token<'_>, ParseErrorContent>> for ParseError {
    fn from(value: lalrpop_util::ErrorRecovery<usize, Token<'_>, ParseErrorContent>) -> Self {
        value.error.into()
    }
}
#[cfg(feature = "parse")]
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

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, BorrowDecode, Serialize, Deserialize)]
pub struct File<'s, Error = Infallible> {
    #[serde(borrow)]
    pub statements: Vec<Labelled<'s, Option<Statement<'s, Error>>>>,
}

impl<'s> File<'s> {
    #[cfg(feature = "parse")]
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

impl<'s, Err> ::bincode::Encode for File<'s, Err>
where
    Err: Encode,
{
    fn encode<E: bincode::enc::Encoder>(
        &self,
        encoder: &mut E,
    ) -> Result<(), bincode::error::EncodeError> {
        utils::encode_vec(&self.statements, encoder)?; // we have to avoid direct encoding cause the implementation of `encode` for `Vec` require `'static`
        Ok(())
    }
}

#[derive(
    Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Encode, BorrowDecode, Serialize, Deserialize,
)]
pub struct Labelled<'s, T> {
    #[serde(borrow)]
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

#[derive(
    Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Encode, BorrowDecode, Serialize, Deserialize,
)]
/**
    Definition of a label

    This is not `Clone` as a ward against duplicating code defining the same label
*/
pub struct LabelDef<'s> {
    #[serde(borrow)]
    pub label: Identifier<'s>,
}

#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Encode,
    BorrowDecode,
    Serialize,
    Deserialize,
)]
pub enum LabelRef<'s, Error = Infallible> {
    #[serde(borrow)]
    Identifier(Identifier<'s>),
    SpecialIdentifier(SpecialIdentifier),
    Error(Error),
}
impl Display for LabelRef<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LabelRef::Identifier(id) => <Identifier as Display>::fmt(id, f),
            LabelRef::SpecialIdentifier(si) => <SpecialIdentifier as Display>::fmt(si, f),
            LabelRef::Error(e) => <!>::from(*e),
        }
    }
}
impl<'s> From<Identifier<'s>> for LabelRef<'s> {
    fn from(value: Identifier<'s>) -> Self {
        Self::Identifier(value)
    }
}
impl From<SpecialIdentifier> for LabelRef<'_> {
    fn from(value: SpecialIdentifier) -> Self {
        Self::SpecialIdentifier(value)
    }
}

#[derive(
    Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Encode, BorrowDecode, Serialize, Deserialize,
)]
pub enum Statement<'s, Error = Infallible> {
    Ints(#[serde(borrow)] IntsStm<'s, Error>),
    Zeros(#[serde(borrow)] ZerosStm<'s, Error>),
    Instruction(#[serde(borrow)] Instruction<'s, Error>),
    Inc(#[serde(borrow)] IncStm<'s, Error>),
    Dec(#[serde(borrow)] DecStm<'s, Error>),
    Jmp(#[serde(borrow)] JmpStm<'s, Error>),
    Mov(#[serde(borrow)] MovStm<'s, Error>),
    Load(#[serde(borrow)] LoadStm<'s, Error>),
    Store(#[serde(borrow)] StoreStm<'s, Error>),
    Call(#[serde(borrow)] CallStm<'s, Error>),
    Ret(RetStm),
    Export(#[serde(borrow)] ExportStm<'s>),
    Entry(#[serde(borrow)] EntryStm<'s>),
    Error(Error),
}

#[derive(
    Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Encode, BorrowDecode, Serialize, Deserialize,
)]
pub enum Instruction<'s, Error = Infallible> {
    Add(
        #[serde(borrow)] ReadParam<'s, Error>,
        #[serde(borrow)] ReadParam<'s, Error>,
        #[serde(borrow)] WriteParam<'s, Error>,
    ),
    Mul(
        #[serde(borrow)] ReadParam<'s, Error>,
        #[serde(borrow)] ReadParam<'s, Error>,
        #[serde(borrow)] WriteParam<'s, Error>,
    ),
    In(#[serde(borrow)] WriteParam<'s, Error>),
    Out(#[serde(borrow)] ReadParam<'s, Error>),
    Jnz(
        #[serde(borrow)] ReadParam<'s, Error>,
        #[serde(borrow)] ReadParam<'s, Error>,
    ),
    Jz(
        #[serde(borrow)] ReadParam<'s, Error>,
        #[serde(borrow)] ReadParam<'s, Error>,
    ),
    Slt(
        #[serde(borrow)] ReadParam<'s, Error>,
        #[serde(borrow)] ReadParam<'s, Error>,
        #[serde(borrow)] WriteParam<'s, Error>,
    ),
    Seq(
        #[serde(borrow)] ReadParam<'s, Error>,
        #[serde(borrow)] ReadParam<'s, Error>,
        #[serde(borrow)] WriteParam<'s, Error>,
    ),
    Incb(#[serde(borrow)] ReadParam<'s, Error>),
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
            Instruction::Error(e) => <!>::from(*e),
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
            Instruction::In(a) => {
                let mut v = ArrayVec::new();
                v.push(a.into_value());
                v
            }
            Instruction::Out(a) | Instruction::Incb(a) => {
                let mut v = ArrayVec::new();
                v.push(a.into_value());
                v
            }
            Instruction::Jz(a, b) | Instruction::Jnz(a, b) => {
                let mut v = ArrayVec::new();
                v.push(a.into_value());
                v.push(b.into_value());
                v
            }
            Instruction::Halt => ArrayVec::new(),
            Instruction::Error(e) => <!>::from(e),
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
            Instruction::Error(e) => <!>::from(*e),
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
