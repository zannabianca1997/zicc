use std::{
    collections::BTreeSet,
    mem,
    ops::{AddAssign, Range},
};

use arrayvec::ArrayVec;
use either::Either::{self, Left, Right};
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
pub struct IntsStm<'s, Error = !> {
    pub values: Vec<Labelled<'s, Either<Box<Expression<'s, Error>>, StringLit<'s>>>>,
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

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct IncStm<'s, Error = !>(pub UnlabelledWriteParam<'s, Error>);
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct DecStm<'s, Error = !>(pub UnlabelledWriteParam<'s, Error>);
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct JmpStm<'s, Error = !>(pub ReadParam<'s, Error>);
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ZerosStm<'s, Error = !>(pub Box<Expression<'s, Error>>);

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum MovStm<'s, Error = !> {
    /// Moves a single memory cell
    Single(ReadParam<'s, Error>, WriteParam<'s, Error>),
    /// Moves multiple consecutive memory cells
    Multiple(
        UnlabelledNonImmediateReadParam<'s, Error>,
        UnlabelledWriteParam<'s, Error>,
        Box<Expression<'s, Error>>,
    ),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum PushStm<'s, Error = !> {
    /// Push a single memory cell
    Single(ReadParam<'s, Error>),
    /// Pushes multiple consecutive memory cells
    Multiple(
        UnlabelledNonImmediateReadParam<'s, Error>,
        Box<Expression<'s, Error>>,
    ),
}
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum PopStm<'s, Error = !> {
    /// Pop a single memory cell
    Single(WriteParam<'s, Error>),
    /// Pop multiple consecutive memory cells
    Multiple(UnlabelledWriteParam<'s, Error>, Box<Expression<'s, Error>>),
}
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct CallStm<'s, Error = !>(pub ReadParam<'s, Error>);
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RetStm;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ExportStm<'s> {
    pub exported: Vec<Identifier<'s>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct EntryStm<'s> {
    pub entry: Identifier<'s>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ReadParam<'s, Error = !> {
    Absolute(AbsoluteParam<'s, Error>),
    Immediate(ImmediateParam<'s, Error>),
    Relative(RelativeParam<'s, Error>),
    Error(Error),
}
impl<'s> ReadParam<'s> {
    pub fn mode(&self) -> VMInt {
        match self {
            ReadParam::Absolute(_) => 0,
            ReadParam::Immediate(_) => 1,
            ReadParam::Relative(_) => 2,
            ReadParam::Error(e) => *e,
        }
    }
    pub fn as_value_mut(&mut self) -> &mut Labelled<'s, Box<Expression<'s>>> {
        match self {
            ReadParam::Absolute(AbsoluteParam { value })
            | ReadParam::Immediate(ImmediateParam { value })
            | ReadParam::Relative(RelativeParam { value }) => value,
            ReadParam::Error(e) => *e,
        }
    }
    fn into_value(self) -> Labelled<'s, Box<Expression<'s>>> {
        match self {
            ReadParam::Absolute(AbsoluteParam { value })
            | ReadParam::Immediate(ImmediateParam { value })
            | ReadParam::Relative(RelativeParam { value }) => value,
            ReadParam::Error(e) => e,
        }
    }
}
impl<'s, E> From<WriteParam<'s, E>> for ReadParam<'s, E> {
    fn from(value: WriteParam<'s, E>) -> Self {
        match value {
            WriteParam::Absolute(a) => ReadParam::Absolute(a),
            WriteParam::Relative(r) => ReadParam::Relative(r),
            WriteParam::Error(e) => ReadParam::Error(e),
        }
    }
}
impl<'s, E> From<UnlabelledReadParam<'s, E>> for ReadParam<'s, E> {
    fn from(value: UnlabelledReadParam<'s, E>) -> Self {
        match value {
            UnlabelledReadParam::Absolute(a) => ReadParam::Absolute(a.into()),
            UnlabelledReadParam::Immediate(i) => ReadParam::Immediate(i.into()),
            UnlabelledReadParam::Relative(r) => ReadParam::Relative(r.into()),
            UnlabelledReadParam::Error(e) => ReadParam::Error(e),
        }
    }
}
impl<'s, E> From<UnlabelledWriteParam<'s, E>> for ReadParam<'s, E> {
    fn from(value: UnlabelledWriteParam<'s, E>) -> Self {
        match value {
            UnlabelledWriteParam::Absolute(a) => ReadParam::Absolute(a.into()),
            UnlabelledWriteParam::Relative(r) => ReadParam::Relative(r.into()),
            UnlabelledWriteParam::Error(e) => ReadParam::Error(e),
        }
    }
}
impl<'s, E> From<Box<Expression<'s, E>>> for ReadParam<'s, E> {
    fn from(value: Box<Expression<'s, E>>) -> Self {
        Self::Absolute(AbsoluteParam {
            value: Labelled {
                labels: BTreeSet::new(),
                content: value,
            },
        })
    }
}

pub type NonImmediateReadParam<'s, Error = !> = WriteParam<'s, Error>;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum WriteParam<'s, Error = !> {
    Absolute(AbsoluteParam<'s, Error>),
    Relative(RelativeParam<'s, Error>),
    Error(Error),
}

impl<'s, E> WriteParam<'s, E> {
    pub fn from_read(p: ReadParam<'s, E>) -> Option<Self> {
        match p {
            ReadParam::Absolute(a) => Some(Self::Absolute(a)),
            ReadParam::Immediate(_) => None,
            ReadParam::Relative(r) => Some(Self::Relative(r)),
            ReadParam::Error(e) => Some(Self::Error(e)),
        }
    }
}
impl<'s> WriteParam<'s> {
    pub fn mode(&self) -> VMInt {
        match self {
            WriteParam::Absolute(_) => 0,
            WriteParam::Relative(_) => 2,
            WriteParam::Error(e) => *e,
        }
    }
    fn into_value(self) -> Labelled<'s, Box<Expression<'s>>> {
        match self {
            WriteParam::Absolute(AbsoluteParam { value })
            | WriteParam::Relative(RelativeParam { value }) => value,
            WriteParam::Error(e) => e,
        }
    }

    pub fn as_value_mut(&mut self) -> &mut Labelled<'s, Box<Expression<'s>>> {
        match self {
            WriteParam::Absolute(AbsoluteParam { value })
            | WriteParam::Relative(RelativeParam { value }) => value,
            WriteParam::Error(e) => *e,
        }
    }
}
impl<'s, E> From<UnlabelledWriteParam<'s, E>> for WriteParam<'s, E> {
    fn from(value: UnlabelledWriteParam<'s, E>) -> Self {
        match value {
            UnlabelledWriteParam::Absolute(a) => WriteParam::Absolute(a.into()),
            UnlabelledWriteParam::Relative(r) => WriteParam::Relative(r.into()),
            UnlabelledWriteParam::Error(e) => WriteParam::Error(e),
        }
    }
}
impl<'s, E> From<Box<Expression<'s, E>>> for WriteParam<'s, E> {
    fn from(value: Box<Expression<'s, E>>) -> Self {
        Self::Absolute(AbsoluteParam {
            value: Labelled {
                labels: BTreeSet::new(),
                content: value,
            },
        })
    }
}
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ImmediateParam<'s, Error = !> {
    pub value: Labelled<'s, Box<Expression<'s, Error>>>,
}
impl<'s, E> From<UnlabelledImmediateParam<'s, E>> for ImmediateParam<'s, E> {
    fn from(value: UnlabelledImmediateParam<'s, E>) -> Self {
        Self {
            value: Labelled {
                labels: BTreeSet::new(),
                content: value.value,
            },
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct AbsoluteParam<'s, Error = !> {
    pub value: Labelled<'s, Box<Expression<'s, Error>>>,
}
impl<'s, E> From<UnlabelledAbsoluteParam<'s, E>> for AbsoluteParam<'s, E> {
    fn from(value: UnlabelledAbsoluteParam<'s, E>) -> Self {
        Self {
            value: Labelled {
                labels: BTreeSet::new(),
                content: value.value,
            },
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RelativeParam<'s, Error = !> {
    pub value: Labelled<'s, Box<Expression<'s, Error>>>,
}
impl<'s, E> From<UnlabelledRelativeParam<'s, E>> for RelativeParam<'s, E> {
    fn from(value: UnlabelledRelativeParam<'s, E>) -> Self {
        Self {
            value: Labelled {
                labels: BTreeSet::new(),
                content: value.value,
            },
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum UnlabelledReadParam<'s, Error = !> {
    Absolute(UnlabelledAbsoluteParam<'s, Error>),
    Immediate(UnlabelledImmediateParam<'s, Error>),
    Relative(UnlabelledRelativeParam<'s, Error>),
    Error(Error),
}
impl<'s> UnlabelledReadParam<'s> {
    pub fn mode(&self) -> VMInt {
        match self {
            UnlabelledReadParam::Absolute(_) => 0,
            UnlabelledReadParam::Immediate(_) => 1,
            UnlabelledReadParam::Relative(_) => 2,
            UnlabelledReadParam::Error(e) => *e,
        }
    }
}
impl<'s, E> From<UnlabelledWriteParam<'s, E>> for UnlabelledReadParam<'s, E> {
    fn from(value: UnlabelledWriteParam<'s, E>) -> Self {
        match value {
            UnlabelledWriteParam::Absolute(a) => UnlabelledReadParam::Absolute(a),
            UnlabelledWriteParam::Relative(r) => UnlabelledReadParam::Relative(r),
            UnlabelledWriteParam::Error(e) => UnlabelledReadParam::Error(e),
        }
    }
}

pub type UnlabelledNonImmediateReadParam<'s, Error = !> = UnlabelledWriteParam<'s, Error>;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum UnlabelledWriteParam<'s, Error = !> {
    Absolute(UnlabelledAbsoluteParam<'s, Error>),
    Relative(UnlabelledRelativeParam<'s, Error>),
    Error(Error),
}

impl<'s, E> UnlabelledWriteParam<'s, E> {
    pub fn map_value(
        self,
        f: impl FnOnce(Box<Expression<'s, E>>) -> Box<Expression<'s, E>>,
    ) -> Self {
        match self {
            UnlabelledWriteParam::Absolute(a) => UnlabelledWriteParam::Absolute(a.map_value(f)),
            UnlabelledWriteParam::Relative(r) => UnlabelledWriteParam::Relative(r.map_value(f)),
            UnlabelledWriteParam::Error(e) => UnlabelledWriteParam::Error(e),
        }
    }
    pub fn from_read(p: UnlabelledReadParam<'s, E>) -> Option<Self> {
        match p {
            UnlabelledReadParam::Absolute(a) => Some(Self::Absolute(a)),
            UnlabelledReadParam::Immediate(_) => None,
            UnlabelledReadParam::Relative(r) => Some(Self::Relative(r)),
            UnlabelledReadParam::Error(e) => Some(Self::Error(e)),
        }
    }
}
impl<'s> UnlabelledWriteParam<'s> {
    pub fn mode(&self) -> VMInt {
        match self {
            UnlabelledWriteParam::Absolute(_) => 0,
            UnlabelledWriteParam::Relative(_) => 2,
            UnlabelledWriteParam::Error(e) => *e,
        }
    }
}

impl<'s, E> TryFrom<WriteParam<'s, E>> for UnlabelledWriteParam<'s, E> {
    type Error = ParseErrorContent;

    fn try_from(
        value: WriteParam<'s, E>,
    ) -> Result<Self, <Self as TryFrom<WriteParam<'s, E>>>::Error> {
        Ok(match value {
            WriteParam::Absolute(a) => Self::Absolute(a.try_into()?),
            WriteParam::Relative(r) => Self::Relative(r.try_into()?),
            WriteParam::Error(e) => Self::Error(e),
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct UnlabelledImmediateParam<'s, Error = !> {
    pub value: Box<Expression<'s, Error>>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct UnlabelledAbsoluteParam<'s, Error = !> {
    pub value: Box<Expression<'s, Error>>,
}
impl<'s, E> UnlabelledAbsoluteParam<'s, E> {
    pub fn map_value(
        self,
        f: impl FnOnce(Box<Expression<'s, E>>) -> Box<Expression<'s, E>>,
    ) -> Self {
        Self {
            value: f(self.value),
        }
    }
}
impl<'s, E> TryFrom<AbsoluteParam<'s, E>> for UnlabelledAbsoluteParam<'s, E> {
    type Error = ParseErrorContent;

    fn try_from(
        value: AbsoluteParam<'s, E>,
    ) -> Result<Self, <Self as TryFrom<AbsoluteParam<'s, E>>>::Error> {
        let AbsoluteParam {
            value: Labelled { labels, content },
        } = value;
        if labels.is_empty() {
            Ok(Self { value: content })
        } else {
            Err(ParseErrorContent::LabelsOnUnlabelled {
                span: labels
                    .into_iter()
                    .map(|LabelDef { label }| label.span())
                    .reduce(|s1, s2| (s1.start.min(s2.start))..(s1.end.min(s2.end)))
                    .unwrap(),
            })
        }
    }
}
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct UnlabelledRelativeParam<'s, Error = !> {
    pub value: Box<Expression<'s, Error>>,
}
impl<'s, E> TryFrom<RelativeParam<'s, E>> for UnlabelledRelativeParam<'s, E> {
    type Error = ParseErrorContent;

    fn try_from(
        value: RelativeParam<'s, E>,
    ) -> Result<Self, <Self as TryFrom<RelativeParam<'s, E>>>::Error> {
        let RelativeParam {
            value: Labelled { labels, content },
        } = value;
        if labels.is_empty() {
            Ok(Self { value: content })
        } else {
            Err(ParseErrorContent::LabelsOnUnlabelled {
                span: labels
                    .into_iter()
                    .map(|LabelDef { label }| label.span())
                    .reduce(|s1, s2| (s1.start.min(s2.start))..(s1.end.min(s2.end)))
                    .unwrap(),
            })
        }
    }
}
impl<'s, E> UnlabelledRelativeParam<'s, E> {
    pub fn map_value(
        self,
        f: impl FnOnce(Box<Expression<'s, E>>) -> Box<Expression<'s, E>>,
    ) -> Self {
        Self {
            value: f(self.value),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Expression<'s, Error = !> {
    Sum(Box<Expression<'s, Error>>, Box<Expression<'s, Error>>),
    Sub(Box<Expression<'s, Error>>, Box<Expression<'s, Error>>),
    Mul(Box<Expression<'s, Error>>, Box<Expression<'s, Error>>),
    Div(Box<Expression<'s, Error>>, Box<Expression<'s, Error>>),
    Mod(Box<Expression<'s, Error>>, Box<Expression<'s, Error>>),
    Neg(Box<Expression<'s, Error>>),
    Num(VMInt),
    Ref(LabelRef<'s, Error>),
    Error(Error),
}
impl<'s> Expression<'s> {
    pub fn replace<E>(
        self: Box<Self>,
        solver: &mut impl FnMut(&LabelRef<'s>) -> Result<Option<<Self as AstNode<!>>::ErrMapped<E>>, E>,
    ) -> Box<<Self as AstNode<!>>::ErrMapped<E>> {
        Box::new(match *self {
            Expression::Sum(a, b) => Expression::Sum(a.replace(solver), b.replace(solver)),
            Expression::Sub(a, b) => Expression::Sub(a.replace(solver), b.replace(solver)),
            Expression::Mul(a, b) => Expression::Mul(a.replace(solver), b.replace(solver)),
            Expression::Div(a, b) => Expression::Div(a.replace(solver), b.replace(solver)),
            Expression::Mod(a, b) => Expression::Mod(a.replace(solver), b.replace(solver)),
            Expression::Neg(a) => Expression::Neg(a.replace(solver)),
            Expression::Num(a) => Expression::Num(a),
            Expression::Ref(a) => match solver(&a) {
                Ok(Some(replacement)) => replacement,
                Ok(None) => Expression::Ref(a.map_err(&mut |e| e)),
                Err(e) => Expression::Error(e),
            },
            Expression::Error(a) => a,
        })
    }
}
impl<'s> AddAssign<VMInt> for Expression<'s> {
    fn add_assign(&mut self, rhs: VMInt) {
        let taken = mem::replace(self, Expression::Num(0xdeadbeef));
        *self = Expression::Sum(Box::new(taken), Box::new(Expression::Num(rhs)))
    }
}

pub trait AstNode<E> {
    fn constant_folding(self) -> Self;

    type ErrMapped<EE>: AstNode<EE>;
    fn extract_errs(
        self,
        accumulator: &mut impl Accumulator<Error = impl From<E>>,
    ) -> Option<Self::ErrMapped<!>>;

    fn map_err<EE>(self, f: &mut impl FnMut(E) -> EE) -> Self::ErrMapped<EE>;

    fn max_unnamed_label(&self) -> Option<usize>;
}

impl<'s, E> AstNode<E> for File<'s, E> {
    fn constant_folding(self) -> Self {
        Self {
            statements: self
                .statements
                .into_iter()
                .map(AstNode::constant_folding)
                .coalesce(|a, b| match (a, b) {
                    (
                        Labelled {
                            labels: a_lbls,
                            content: None,
                        },
                        Labelled {
                            labels: b_lbls,
                            content,
                        },
                    ) => Ok(Labelled {
                        labels: a_lbls.into_iter().chain(b_lbls).collect(),
                        content,
                    }),
                    (a, b) => Err((a, b)),
                })
                .collect(),
        }
    }

    type ErrMapped<EE> = File<'s, EE>;

    fn extract_errs(
        self,
        accumulator: &mut impl Accumulator<Error = impl From<E>>,
    ) -> Option<Self::ErrMapped<!>> {
        Some(File {
            statements: self.statements.extract_errs(accumulator)?,
        })
    }

    fn max_unnamed_label(&self) -> Option<usize> {
        self.statements
            .iter()
            .flat_map(AstNode::max_unnamed_label)
            .max()
    }

    fn map_err<EE>(self, f: &mut impl FnMut(E) -> EE) -> Self::ErrMapped<EE> {
        File {
            statements: self.statements.into_iter().map(|s| s.map_err(f)).collect(),
        }
    }
}

impl<'s, E, T: AstNode<E>> AstNode<E> for Labelled<'s, T> {
    fn constant_folding(self) -> Self {
        Self {
            labels: self.labels,
            content: self.content.constant_folding(),
        }
    }

    type ErrMapped<EE> = Labelled<'s, T::ErrMapped<EE>>;

    fn extract_errs(
        self,
        accumulator: &mut impl Accumulator<Error = impl From<E>>,
    ) -> Option<Self::ErrMapped<!>> {
        Some(Labelled {
            labels: self.labels,
            content: self.content.extract_errs(accumulator)?,
        })
    }

    fn max_unnamed_label(&self) -> Option<usize> {
        self.labels
            .iter()
            .flat_map(|ldef| match ldef.label {
                Identifier::Unnamed(n, _) => Some(n),
                Identifier::Named(_, _) => None,
            })
            .chain(self.content.max_unnamed_label())
            .max()
    }

    fn map_err<EE>(self, f: &mut impl FnMut(E) -> EE) -> Self::ErrMapped<EE> {
        Labelled {
            labels: self.labels,
            content: self.content.map_err(f),
        }
    }
}

impl<E, T: AstNode<E>> AstNode<E> for Option<T> {
    fn constant_folding(self) -> Self {
        self.map(AstNode::constant_folding)
    }

    type ErrMapped<EE> = Option<T::ErrMapped<EE>>;

    fn extract_errs(
        self,
        accumulator: &mut impl Accumulator<Error = impl From<E>>,
    ) -> Option<Self::ErrMapped<!>> {
        match self {
            Some(t) => t.extract_errs(accumulator).map(Some),
            None => Some(None),
        }
    }

    fn max_unnamed_label(&self) -> Option<usize> {
        self.as_ref().and_then(AstNode::max_unnamed_label)
    }

    fn map_err<EE>(self, f: &mut impl FnMut(E) -> EE) -> Self::ErrMapped<EE> {
        self.map(|c| c.map_err(f))
    }
}

impl<E, T: AstNode<E>> AstNode<E> for Box<T> {
    fn constant_folding(self) -> Self {
        Box::map_in_place(self, AstNode::constant_folding)
    }

    type ErrMapped<EE> = Box<T::ErrMapped<EE>>;

    fn extract_errs(
        self,
        accumulator: &mut impl Accumulator<Error = impl From<E>>,
    ) -> Option<Self::ErrMapped<!>> {
        // we have to take it on the stack cause it will change size
        let inner = Box::into_inner(self);
        inner.extract_errs(accumulator).map(Box::new)
    }

    fn max_unnamed_label(&self) -> Option<usize> {
        Box::as_ref(self).max_unnamed_label()
    }

    fn map_err<EE>(self, f: &mut impl FnMut(E) -> EE) -> Self::ErrMapped<EE> {
        // we have to take it on the stack cause it will change size
        let inner = Box::into_inner(self);
        Box::new(inner.map_err(f))
    }
}

impl<E, T: AstNode<E>> AstNode<E> for Vec<T> {
    fn constant_folding(self) -> Self {
        self.into_iter().map(AstNode::constant_folding).collect()
    }

    type ErrMapped<EE> = Vec<T::ErrMapped<EE>>;

    fn extract_errs(
        self,
        accumulator: &mut impl Accumulator<Error = impl From<E>>,
    ) -> Option<Self::ErrMapped<!>> {
        let mut error_present = false;
        let res = self
            .into_iter()
            .flat_map(|i| match i.extract_errs(accumulator) {
                Some(t) => Some(t),
                None => {
                    error_present = true;
                    None
                }
            })
            .collect_vec();
        (!error_present).then_some(res)
    }

    fn max_unnamed_label(&self) -> Option<usize> {
        self.iter().flat_map(AstNode::max_unnamed_label).max()
    }

    fn map_err<EE>(self, f: &mut impl FnMut(E) -> EE) -> Self::ErrMapped<EE> {
        self.into_iter().map(|i| i.map_err(f)).collect()
    }
}

impl<E, L: AstNode<E>, R: AstNode<E>> AstNode<E> for Either<L, R> {
    fn constant_folding(self) -> Self {
        self.map_either(AstNode::constant_folding, AstNode::constant_folding)
    }

    type ErrMapped<EE> = Either<L::ErrMapped<EE>, R::ErrMapped<EE>>;

    fn extract_errs(
        self,
        accumulator: &mut impl Accumulator<Error = impl From<E>>,
    ) -> Option<Self::ErrMapped<!>> {
        match self {
            Either::Left(l) => l.extract_errs(accumulator).map(Left),
            Either::Right(r) => r.extract_errs(accumulator).map(Right),
        }
    }

    fn max_unnamed_label(&self) -> Option<usize> {
        self.as_ref()
            .map_either(AstNode::max_unnamed_label, AstNode::max_unnamed_label)
            .into_inner()
    }

    fn map_err<EE>(self, f: &mut impl FnMut(E) -> EE) -> Self::ErrMapped<EE> {
        match self {
            Left(l) => Left(l.map_err(f)),
            Right(r) => Right(r.map_err(f)),
        }
    }
}

impl<E> AstNode<E> for StringLit<'_> {
    fn constant_folding(self) -> Self {
        self
    }

    type ErrMapped<EE> = Self;

    fn extract_errs(
        self,
        _accumulator: &mut impl Accumulator<Error = impl From<E>>,
    ) -> Option<Self::ErrMapped<!>> {
        Some(self)
    }

    fn max_unnamed_label(&self) -> Option<usize> {
        None
    }

    fn map_err<EE>(self, _f: &mut impl FnMut(E) -> EE) -> Self::ErrMapped<EE> {
        self
    }
}

impl<'s, E> AstNode<E> for LabelRef<'s, E> {
    fn constant_folding(self) -> Self {
        self
    }

    type ErrMapped<EE> = LabelRef<'s, EE>;

    fn extract_errs(
        self,
        accumulator: &mut impl Accumulator<Error = impl From<E>>,
    ) -> Option<Self::ErrMapped<!>> {
        match self {
            LabelRef::Identifier(ident) => Some(LabelRef::Identifier(ident)),
            LabelRef::SpecialIdentifier(sident) => Some(LabelRef::SpecialIdentifier(sident)),
            LabelRef::Error(e) => {
                accumulator.push(e);
                None
            }
        }
    }

    fn max_unnamed_label(&self) -> Option<usize> {
        if let LabelRef::Identifier(Identifier::Unnamed(n, ..)) = self {
            Some(*n)
        } else {
            None
        }
    }

    fn map_err<EE>(self, f: &mut impl FnMut(E) -> EE) -> Self::ErrMapped<EE> {
        match self {
            LabelRef::Identifier(i) => LabelRef::Identifier(i),
            LabelRef::SpecialIdentifier(s) => LabelRef::SpecialIdentifier(s),
            LabelRef::Error(e) => LabelRef::Error(f(e)),
        }
    }
}

macro_rules! ast_node_for_statement {
    (
        $($variant:ident)*
    ) => {
        impl<'s, E> AstNode<E> for Statement<'s, E> {
            fn constant_folding(self) -> Self {
                match self {
                    $(
                    Statement::$variant(stm) => Self::$variant(AstNode::<E>::constant_folding(stm)),
                    )*
                    Statement::Error(e) => Statement::Error(e)
                }
            }

            type ErrMapped<EE> = Statement<'s, EE>;

            fn extract_errs(
                self,
                accumulator: &mut impl Accumulator<Error = impl From<E>>,
            ) -> Option<Self::ErrMapped<!>> {
                match self {
                    $(
                    Statement::$variant(stm) => Some(Statement::$variant(AstNode::<E>::extract_errs(stm, accumulator)?)),
                    )*
                    Statement::Error(e) => {
                        accumulator.push(e);
                        None
                    }
                }
            }

            fn map_err<EE>(self, f: &mut impl FnMut(E) -> EE) -> Self::ErrMapped<EE> {
                match self {
                    $(
                    Statement::$variant(stm) => Statement::$variant(AstNode::<E>::map_err(stm, f)),
                    )*
                    Statement::Error(e) => Statement::Error(f(e))
                }
            }

            fn max_unnamed_label(&self) -> Option<usize> {
                match self {
                    $(
                    Statement::$variant(stm) => AstNode::<E>::max_unnamed_label(stm),
                    )*
                    Statement::Error(_) => None
                }
            }

        }
    };
}
ast_node_for_statement! {
    Ints Instruction Inc Dec Jmp Mov Zeros Push Pop Call Ret Export Entry
}

impl<'s, E> AstNode<E> for IntsStm<'s, E> {
    fn constant_folding(self) -> Self {
        Self {
            values: self.values.constant_folding(),
        }
    }

    type ErrMapped<EE> = IntsStm<'s, EE>;

    fn extract_errs(
        self,
        accumulator: &mut impl Accumulator<Error = impl From<E>>,
    ) -> Option<Self::ErrMapped<!>> {
        Some(IntsStm {
            values: self.values.extract_errs(accumulator)?,
        })
    }

    fn max_unnamed_label(&self) -> Option<usize> {
        self.values
            .iter()
            .flat_map(AstNode::max_unnamed_label)
            .max()
    }

    fn map_err<EE>(self, f: &mut impl FnMut(E) -> EE) -> Self::ErrMapped<EE> {
        IntsStm {
            values: self.values.map_err(f),
        }
    }
}

impl<'s, E> AstNode<E> for Instruction<'s, E> {
    fn constant_folding(self) -> Self {
        match self {
            Instruction::Add(a, b, c) => Instruction::Add(
                a.constant_folding(),
                b.constant_folding(),
                c.constant_folding(),
            ),
            Instruction::Mul(a, b, c) => Instruction::Mul(
                a.constant_folding(),
                b.constant_folding(),
                c.constant_folding(),
            ),
            Instruction::In(a) => Instruction::In(a.constant_folding()),
            Instruction::Out(a) => Instruction::Out(a.constant_folding()),
            Instruction::Jz(a, b) => Instruction::Jz(a.constant_folding(), b.constant_folding()),
            Instruction::Jnz(a, b) => Instruction::Jnz(a.constant_folding(), b.constant_folding()),
            Instruction::Slt(a, b, c) => Instruction::Slt(
                a.constant_folding(),
                b.constant_folding(),
                c.constant_folding(),
            ),
            Instruction::Seq(a, b, c) => Instruction::Seq(
                a.constant_folding(),
                b.constant_folding(),
                c.constant_folding(),
            ),
            Instruction::Incb(a) => Instruction::Incb(a.constant_folding()),
            Instruction::Halt => Instruction::Halt,
            Instruction::Error(e) => Instruction::Error(e),
        }
    }

    type ErrMapped<EE> = Instruction<'s, EE>;

    fn extract_errs(
        self,
        accumulator: &mut impl Accumulator<Error = impl From<E>>,
    ) -> Option<Self::ErrMapped<!>> {
        match self {
            Instruction::Add(a, b, c) => Some({
                let a = a.extract_errs(accumulator);
                let b = b.extract_errs(accumulator);
                let c = c.extract_errs(accumulator);
                Instruction::Add(a?, b?, c?)
            }),
            Instruction::Mul(a, b, c) => Some({
                let a = a.extract_errs(accumulator);
                let b = b.extract_errs(accumulator);
                let c = c.extract_errs(accumulator);
                Instruction::Mul(a?, b?, c?)
            }),
            Instruction::In(a) => Some(Instruction::In(a.extract_errs(accumulator)?)),
            Instruction::Out(a) => Some(Instruction::Out(a.extract_errs(accumulator)?)),
            Instruction::Jz(a, b) => Some({
                let a = a.extract_errs(accumulator);
                let b = b.extract_errs(accumulator);
                Instruction::Jz(a?, b?)
            }),
            Instruction::Jnz(a, b) => Some({
                let a = a.extract_errs(accumulator);
                let b = b.extract_errs(accumulator);
                Instruction::Jnz(a?, b?)
            }),
            Instruction::Slt(a, b, c) => Some({
                let a = a.extract_errs(accumulator);
                let b = b.extract_errs(accumulator);
                let c = c.extract_errs(accumulator);
                Instruction::Slt(a?, b?, c?)
            }),
            Instruction::Seq(a, b, c) => Some({
                let a = a.extract_errs(accumulator);
                let b = b.extract_errs(accumulator);
                let c = c.extract_errs(accumulator);
                Instruction::Seq(a?, b?, c?)
            }),
            Instruction::Incb(a) => Some(Instruction::Incb(a.extract_errs(accumulator)?)),
            Instruction::Halt => Some(Instruction::Halt),
            Instruction::Error(e) => {
                accumulator.push(e);
                None
            }
        }
    }

    fn map_err<EE>(self, f: &mut impl FnMut(E) -> EE) -> Self::ErrMapped<EE> {
        match self {
            Instruction::Add(a, b, c) => {
                let a = a.map_err(f);
                let b = b.map_err(f);
                let c = c.map_err(f);
                Instruction::Add(a, b, c)
            }
            Instruction::Mul(a, b, c) => {
                let a = a.map_err(f);
                let b = b.map_err(f);
                let c = c.map_err(f);
                Instruction::Mul(a, b, c)
            }
            Instruction::In(a) => Instruction::In(a.map_err(f)),
            Instruction::Out(a) => Instruction::Out(a.map_err(f)),
            Instruction::Jz(a, b) => {
                let a = a.map_err(f);
                let b = b.map_err(f);
                Instruction::Jz(a, b)
            }
            Instruction::Jnz(a, b) => {
                let a = a.map_err(f);
                let b = b.map_err(f);
                Instruction::Jnz(a, b)
            }
            Instruction::Slt(a, b, c) => {
                let a = a.map_err(f);
                let b = b.map_err(f);
                let c = c.map_err(f);
                Instruction::Slt(a, b, c)
            }
            Instruction::Seq(a, b, c) => {
                let a = a.map_err(f);
                let b = b.map_err(f);
                let c = c.map_err(f);
                Instruction::Seq(a, b, c)
            }
            Instruction::Incb(a) => Instruction::Incb(a.map_err(f)),
            Instruction::Halt => Instruction::Halt,
            Instruction::Error(e) => Instruction::Error(f(e)),
        }
    }

    fn max_unnamed_label(&self) -> Option<usize> {
        match self {
            Instruction::Add(a, b, c)
            | Instruction::Mul(a, b, c)
            | Instruction::Slt(a, b, c)
            | Instruction::Seq(a, b, c) => [
                a.max_unnamed_label(),
                b.max_unnamed_label(),
                c.max_unnamed_label(),
            ]
            .into_iter()
            .flatten()
            .max(),
            Instruction::In(a) => a.max_unnamed_label(),
            Instruction::Out(a) => a.max_unnamed_label(),
            Instruction::Jz(a, b) | Instruction::Jnz(a, b) => {
                [a.max_unnamed_label(), b.max_unnamed_label()]
                    .into_iter()
                    .flatten()
                    .max()
            }
            Instruction::Incb(a) => a.max_unnamed_label(),
            Instruction::Halt => None,
            Instruction::Error(_) => None,
        }
    }
}

impl<'s, E> AstNode<E> for IncStm<'s, E> {
    fn constant_folding(self) -> Self {
        Self(self.0.constant_folding())
    }

    type ErrMapped<EE> = IncStm<'s, EE>;

    fn extract_errs(
        self,
        accumulator: &mut impl Accumulator<Error = impl From<E>>,
    ) -> Option<Self::ErrMapped<!>> {
        Some(IncStm(self.0.extract_errs(accumulator)?))
    }

    fn max_unnamed_label(&self) -> Option<usize> {
        self.0.max_unnamed_label()
    }

    fn map_err<EE>(self, f: &mut impl FnMut(E) -> EE) -> Self::ErrMapped<EE> {
        IncStm(self.0.map_err(f))
    }
}
impl<'s, E> AstNode<E> for DecStm<'s, E> {
    fn constant_folding(self) -> Self {
        Self(self.0.constant_folding())
    }

    type ErrMapped<EE> = DecStm<'s, EE>;

    fn extract_errs(
        self,
        accumulator: &mut impl Accumulator<Error = impl From<E>>,
    ) -> Option<Self::ErrMapped<!>> {
        Some(DecStm(self.0.extract_errs(accumulator)?))
    }

    fn max_unnamed_label(&self) -> Option<usize> {
        self.0.max_unnamed_label()
    }

    fn map_err<EE>(self, f: &mut impl FnMut(E) -> EE) -> Self::ErrMapped<EE> {
        DecStm(self.0.map_err(f))
    }
}
impl<'s, E> AstNode<E> for JmpStm<'s, E> {
    fn constant_folding(self) -> Self {
        Self(self.0.constant_folding())
    }

    type ErrMapped<EE> = JmpStm<'s, EE>;

    fn extract_errs(
        self,
        accumulator: &mut impl Accumulator<Error = impl From<E>>,
    ) -> Option<Self::ErrMapped<!>> {
        Some(JmpStm(self.0.extract_errs(accumulator)?))
    }

    fn max_unnamed_label(&self) -> Option<usize> {
        self.0.max_unnamed_label()
    }

    fn map_err<EE>(self, f: &mut impl FnMut(E) -> EE) -> Self::ErrMapped<EE> {
        JmpStm(self.0.map_err(f))
    }
}
impl<'s, E> AstNode<E> for MovStm<'s, E> {
    fn constant_folding(self) -> Self {
        match self {
            MovStm::Single(a, b) => Self::Single(a.constant_folding(), b.constant_folding()),
            MovStm::Multiple(a, b, n) => Self::Multiple(
                a.constant_folding(),
                b.constant_folding(),
                n.constant_folding(),
            ),
        }
    }

    type ErrMapped<EE> = MovStm<'s, EE>;

    fn extract_errs(
        self,
        accumulator: &mut impl Accumulator<Error = impl From<E>>,
    ) -> Option<Self::ErrMapped<!>> {
        match self {
            MovStm::Single(a, b) => Some(MovStm::Single(
                a.extract_errs(accumulator)?,
                b.extract_errs(accumulator)?,
            )),
            MovStm::Multiple(a, b, n) => Some(MovStm::Multiple(
                a.extract_errs(accumulator)?,
                b.extract_errs(accumulator)?,
                n.extract_errs(accumulator)?,
            )),
        }
    }

    fn max_unnamed_label(&self) -> Option<usize> {
        match self {
            MovStm::Single(a, b) => [a.max_unnamed_label(), b.max_unnamed_label()]
                .into_iter()
                .flatten()
                .max(),
            MovStm::Multiple(a, b, c) => [
                a.max_unnamed_label(),
                b.max_unnamed_label(),
                c.max_unnamed_label(),
            ]
            .into_iter()
            .flatten()
            .max(),
        }
    }

    fn map_err<EE>(self, f: &mut impl FnMut(E) -> EE) -> Self::ErrMapped<EE> {
        match self {
            MovStm::Single(a, b) => MovStm::Single(a.map_err(f), b.map_err(f)),
            MovStm::Multiple(a, b, c) => MovStm::Multiple(a.map_err(f), b.map_err(f), c.map_err(f)),
        }
    }
}
impl<'s, E> AstNode<E> for ZerosStm<'s, E> {
    fn constant_folding(self) -> Self {
        Self(self.0.constant_folding())
    }

    type ErrMapped<EE> = ZerosStm<'s, EE>;

    fn extract_errs(
        self,
        accumulator: &mut impl Accumulator<Error = impl From<E>>,
    ) -> Option<Self::ErrMapped<!>> {
        Some(ZerosStm(self.0.extract_errs(accumulator)?))
    }

    fn max_unnamed_label(&self) -> Option<usize> {
        self.0.max_unnamed_label()
    }

    fn map_err<EE>(self, f: &mut impl FnMut(E) -> EE) -> Self::ErrMapped<EE> {
        ZerosStm(self.0.map_err(f))
    }
}
impl<'s, E> AstNode<E> for PushStm<'s, E> {
    fn constant_folding(self) -> Self {
        match self {
            PushStm::Single(a) => Self::Single(a.constant_folding()),
            PushStm::Multiple(a, n) => Self::Multiple(a.constant_folding(), n.constant_folding()),
        }
    }

    type ErrMapped<EE> = PushStm<'s, EE>;

    fn extract_errs(
        self,
        accumulator: &mut impl Accumulator<Error = impl From<E>>,
    ) -> Option<Self::ErrMapped<!>> {
        Some(match self {
            PushStm::Single(a) => PushStm::Single(a.extract_errs(accumulator)?),
            PushStm::Multiple(a, n) => {
                PushStm::Multiple(a.extract_errs(accumulator)?, n.extract_errs(accumulator)?)
            }
        })
    }

    fn max_unnamed_label(&self) -> Option<usize> {
        match self {
            Self::Single(a) => a.max_unnamed_label(),
            Self::Multiple(a, b) => [a.max_unnamed_label(), b.max_unnamed_label()]
                .into_iter()
                .flatten()
                .max(),
        }
    }

    fn map_err<EE>(self, f: &mut impl FnMut(E) -> EE) -> Self::ErrMapped<EE> {
        match self {
            PushStm::Single(a) => PushStm::Single(a.map_err(f)),
            PushStm::Multiple(a, b) => PushStm::Multiple(a.map_err(f), b.map_err(f)),
        }
    }
}
impl<'s, E> AstNode<E> for PopStm<'s, E> {
    fn constant_folding(self) -> Self {
        match self {
            PopStm::Single(a) => Self::Single(a.constant_folding()),
            PopStm::Multiple(a, n) => Self::Multiple(a.constant_folding(), n.constant_folding()),
        }
    }

    type ErrMapped<EE> = PopStm<'s, EE>;

    fn extract_errs(
        self,
        accumulator: &mut impl Accumulator<Error = impl From<E>>,
    ) -> Option<Self::ErrMapped<!>> {
        Some(match self {
            PopStm::Single(a) => PopStm::Single(a.extract_errs(accumulator)?),
            PopStm::Multiple(a, n) => {
                PopStm::Multiple(a.extract_errs(accumulator)?, n.extract_errs(accumulator)?)
            }
        })
    }

    fn max_unnamed_label(&self) -> Option<usize> {
        match self {
            Self::Single(a) => a.max_unnamed_label(),
            Self::Multiple(a, b) => [a.max_unnamed_label(), b.max_unnamed_label()]
                .into_iter()
                .flatten()
                .max(),
        }
    }

    fn map_err<EE>(self, f: &mut impl FnMut(E) -> EE) -> Self::ErrMapped<EE> {
        match self {
            PopStm::Single(a) => PopStm::Single(a.map_err(f)),
            PopStm::Multiple(a, b) => PopStm::Multiple(a.map_err(f), b.map_err(f)),
        }
    }
}
impl<'s, E> AstNode<E> for CallStm<'s, E> {
    fn constant_folding(self) -> Self {
        Self(self.0.constant_folding())
    }

    type ErrMapped<EE> = CallStm<'s, EE>;

    fn extract_errs(
        self,
        accumulator: &mut impl Accumulator<Error = impl From<E>>,
    ) -> Option<Self::ErrMapped<!>> {
        Some(CallStm(self.0.extract_errs(accumulator)?))
    }

    fn max_unnamed_label(&self) -> Option<usize> {
        self.0.max_unnamed_label()
    }

    fn map_err<EE>(self, f: &mut impl FnMut(E) -> EE) -> Self::ErrMapped<EE> {
        CallStm(self.0.map_err(f))
    }
}
impl<E> AstNode<E> for RetStm {
    fn constant_folding(self) -> Self {
        self
    }

    type ErrMapped<EE> = Self;

    fn extract_errs(
        self,
        _accumulator: &mut impl Accumulator<Error = impl From<E>>,
    ) -> Option<Self::ErrMapped<!>> {
        Some(self)
    }

    fn max_unnamed_label(&self) -> Option<usize> {
        None
    }

    fn map_err<EE>(self, _f: &mut impl FnMut(E) -> EE) -> Self::ErrMapped<EE> {
        self
    }
}
impl<'s, E> AstNode<E> for ExportStm<'s> {
    fn constant_folding(self) -> Self {
        self
    }

    type ErrMapped<EE> = Self;

    fn extract_errs(
        self,
        _accumulator: &mut impl Accumulator<Error = impl From<E>>,
    ) -> Option<Self::ErrMapped<!>> {
        Some(self)
    }

    fn map_err<EE>(self, _f: &mut impl FnMut(E) -> EE) -> Self::ErrMapped<EE> {
        self
    }

    fn max_unnamed_label(&self) -> Option<usize> {
        self.exported
            .iter()
            .filter_map(|l| {
                if let Identifier::Unnamed(n, ..) = l {
                    Some(*n)
                } else {
                    None
                }
            })
            .max()
    }
}
impl<'s, E> AstNode<E> for EntryStm<'s> {
    fn constant_folding(self) -> Self {
        self
    }

    type ErrMapped<EE> = Self;

    fn extract_errs(
        self,
        _accumulator: &mut impl Accumulator<Error = impl From<E>>,
    ) -> Option<Self::ErrMapped<!>> {
        Some(self)
    }

    fn map_err<EE>(self, _f: &mut impl FnMut(E) -> EE) -> Self::ErrMapped<EE> {
        self
    }

    fn max_unnamed_label(&self) -> Option<usize> {
        if let Identifier::Unnamed(n, ..) = &self.entry {
            Some(*n)
        } else {
            None
        }
    }
}

impl<'s, E> AstNode<E> for ReadParam<'s, E> {
    fn constant_folding(self) -> Self {
        match self {
            ReadParam::Absolute(a) => ReadParam::Absolute(a.constant_folding()),
            ReadParam::Immediate(i) => ReadParam::Immediate(i.constant_folding()),
            ReadParam::Relative(r) => ReadParam::Relative(r.constant_folding()),
            ReadParam::Error(e) => ReadParam::Error(e),
        }
    }

    type ErrMapped<EE> = ReadParam<'s, EE>;

    fn extract_errs(
        self,
        accumulator: &mut impl Accumulator<Error = impl From<E>>,
    ) -> Option<Self::ErrMapped<!>> {
        match self {
            ReadParam::Absolute(a) => Some(ReadParam::Absolute(a.extract_errs(accumulator)?)),
            ReadParam::Immediate(i) => Some(ReadParam::Immediate(i.extract_errs(accumulator)?)),
            ReadParam::Relative(r) => Some(ReadParam::Relative(r.extract_errs(accumulator)?)),
            ReadParam::Error(e) => {
                accumulator.push(e);
                None
            }
        }
    }

    fn max_unnamed_label(&self) -> Option<usize> {
        match self {
            Self::Absolute(a) => a.max_unnamed_label(),
            Self::Immediate(i) => i.max_unnamed_label(),
            Self::Relative(r) => r.max_unnamed_label(),
            Self::Error(_) => None,
        }
    }

    fn map_err<EE>(self, f: &mut impl FnMut(E) -> EE) -> Self::ErrMapped<EE> {
        match self {
            ReadParam::Absolute(a) => ReadParam::Absolute(a.map_err(f)),
            ReadParam::Immediate(i) => ReadParam::Immediate(i.map_err(f)),
            ReadParam::Relative(r) => ReadParam::Relative(r.map_err(f)),
            ReadParam::Error(e) => ReadParam::Error(f(e)),
        }
    }
}

impl<'s, E> AstNode<E> for WriteParam<'s, E> {
    fn constant_folding(self) -> Self {
        match self {
            WriteParam::Absolute(a) => WriteParam::Absolute(a.constant_folding()),
            WriteParam::Relative(r) => WriteParam::Relative(r.constant_folding()),
            WriteParam::Error(e) => WriteParam::Error(e),
        }
    }

    type ErrMapped<EE> = WriteParam<'s, EE>;

    fn extract_errs(
        self,
        accumulator: &mut impl Accumulator<Error = impl From<E>>,
    ) -> Option<Self::ErrMapped<!>> {
        match self {
            WriteParam::Absolute(a) => Some(WriteParam::Absolute(a.extract_errs(accumulator)?)),
            WriteParam::Relative(r) => Some(WriteParam::Relative(r.extract_errs(accumulator)?)),
            WriteParam::Error(e) => {
                accumulator.push(e);
                None
            }
        }
    }

    fn max_unnamed_label(&self) -> Option<usize> {
        match self {
            Self::Absolute(a) => a.max_unnamed_label(),
            Self::Relative(r) => r.max_unnamed_label(),
            Self::Error(_) => None,
        }
    }

    fn map_err<EE>(self, f: &mut impl FnMut(E) -> EE) -> Self::ErrMapped<EE> {
        match self {
            WriteParam::Absolute(a) => WriteParam::Absolute(a.map_err(f)),
            WriteParam::Relative(r) => WriteParam::Relative(r.map_err(f)),
            WriteParam::Error(e) => WriteParam::Error(f(e)),
        }
    }
}

impl<'s, E> AstNode<E> for AbsoluteParam<'s, E> {
    fn constant_folding(self) -> Self {
        Self {
            value: self.value.constant_folding(),
        }
    }

    type ErrMapped<EE> = AbsoluteParam<'s, EE>;

    fn extract_errs(
        self,
        accumulator: &mut impl Accumulator<Error = impl From<E>>,
    ) -> Option<Self::ErrMapped<!>> {
        Some(AbsoluteParam {
            value: self.value.extract_errs(accumulator)?,
        })
    }

    fn max_unnamed_label(&self) -> Option<usize> {
        self.value.max_unnamed_label()
    }

    fn map_err<EE>(self, f: &mut impl FnMut(E) -> EE) -> Self::ErrMapped<EE> {
        AbsoluteParam {
            value: self.value.map_err(f),
        }
    }
}
impl<'s, E> AstNode<E> for ImmediateParam<'s, E> {
    fn constant_folding(self) -> Self {
        Self {
            value: self.value.constant_folding(),
        }
    }

    type ErrMapped<EE> = ImmediateParam<'s, EE>;

    fn extract_errs(
        self,
        accumulator: &mut impl Accumulator<Error = impl From<E>>,
    ) -> Option<Self::ErrMapped<!>> {
        Some(ImmediateParam {
            value: self.value.extract_errs(accumulator)?,
        })
    }

    fn max_unnamed_label(&self) -> Option<usize> {
        self.value.max_unnamed_label()
    }

    fn map_err<EE>(self, f: &mut impl FnMut(E) -> EE) -> Self::ErrMapped<EE> {
        ImmediateParam {
            value: self.value.map_err(f),
        }
    }
}
impl<'s, E> AstNode<E> for RelativeParam<'s, E> {
    fn constant_folding(self) -> Self {
        Self {
            value: self.value.constant_folding(),
        }
    }

    type ErrMapped<EE> = RelativeParam<'s, EE>;

    fn extract_errs(
        self,
        accumulator: &mut impl Accumulator<Error = impl From<E>>,
    ) -> Option<Self::ErrMapped<!>> {
        Some(RelativeParam {
            value: self.value.extract_errs(accumulator)?,
        })
    }

    fn max_unnamed_label(&self) -> Option<usize> {
        self.value.max_unnamed_label()
    }

    fn map_err<EE>(self, f: &mut impl FnMut(E) -> EE) -> Self::ErrMapped<EE> {
        RelativeParam {
            value: self.value.map_err(f),
        }
    }
}

impl<'s, E> AstNode<E> for UnlabelledReadParam<'s, E> {
    fn constant_folding(self) -> Self {
        match self {
            UnlabelledReadParam::Absolute(a) => UnlabelledReadParam::Absolute(a.constant_folding()),
            UnlabelledReadParam::Immediate(i) => {
                UnlabelledReadParam::Immediate(i.constant_folding())
            }
            UnlabelledReadParam::Relative(r) => UnlabelledReadParam::Relative(r.constant_folding()),
            UnlabelledReadParam::Error(e) => UnlabelledReadParam::Error(e),
        }
    }

    type ErrMapped<EE> = UnlabelledReadParam<'s, EE>;

    fn extract_errs(
        self,
        accumulator: &mut impl Accumulator<Error = impl From<E>>,
    ) -> Option<Self::ErrMapped<!>> {
        match self {
            UnlabelledReadParam::Absolute(a) => {
                Some(UnlabelledReadParam::Absolute(a.extract_errs(accumulator)?))
            }
            UnlabelledReadParam::Immediate(i) => {
                Some(UnlabelledReadParam::Immediate(i.extract_errs(accumulator)?))
            }
            UnlabelledReadParam::Relative(r) => {
                Some(UnlabelledReadParam::Relative(r.extract_errs(accumulator)?))
            }
            UnlabelledReadParam::Error(e) => {
                accumulator.push(e);
                None
            }
        }
    }

    fn max_unnamed_label(&self) -> Option<usize> {
        match self {
            Self::Absolute(a) => a.max_unnamed_label(),
            Self::Immediate(i) => i.max_unnamed_label(),
            Self::Relative(r) => r.max_unnamed_label(),
            Self::Error(_) => None,
        }
    }
    fn map_err<EE>(self, f: &mut impl FnMut(E) -> EE) -> Self::ErrMapped<EE> {
        match self {
            UnlabelledReadParam::Absolute(a) => UnlabelledReadParam::Absolute(a.map_err(f)),
            UnlabelledReadParam::Immediate(i) => UnlabelledReadParam::Immediate(i.map_err(f)),
            UnlabelledReadParam::Relative(r) => UnlabelledReadParam::Relative(r.map_err(f)),
            UnlabelledReadParam::Error(e) => UnlabelledReadParam::Error(f(e)),
        }
    }
}

impl<'s, E> AstNode<E> for UnlabelledWriteParam<'s, E> {
    fn constant_folding(self) -> Self {
        match self {
            UnlabelledWriteParam::Absolute(a) => {
                UnlabelledWriteParam::Absolute(a.constant_folding())
            }
            UnlabelledWriteParam::Relative(r) => {
                UnlabelledWriteParam::Relative(r.constant_folding())
            }
            UnlabelledWriteParam::Error(e) => UnlabelledWriteParam::Error(e),
        }
    }

    type ErrMapped<EE> = UnlabelledWriteParam<'s, EE>;

    fn extract_errs(
        self,
        accumulator: &mut impl Accumulator<Error = impl From<E>>,
    ) -> Option<Self::ErrMapped<!>> {
        match self {
            UnlabelledWriteParam::Absolute(a) => {
                Some(UnlabelledWriteParam::Absolute(a.extract_errs(accumulator)?))
            }
            UnlabelledWriteParam::Relative(r) => {
                Some(UnlabelledWriteParam::Relative(r.extract_errs(accumulator)?))
            }
            UnlabelledWriteParam::Error(e) => {
                accumulator.push(e);
                None
            }
        }
    }
    fn max_unnamed_label(&self) -> Option<usize> {
        match self {
            Self::Absolute(a) => a.max_unnamed_label(),
            Self::Relative(r) => r.max_unnamed_label(),
            Self::Error(_) => None,
        }
    }
    fn map_err<EE>(self, f: &mut impl FnMut(E) -> EE) -> Self::ErrMapped<EE> {
        match self {
            UnlabelledWriteParam::Absolute(a) => UnlabelledWriteParam::Absolute(a.map_err(f)),
            UnlabelledWriteParam::Relative(r) => UnlabelledWriteParam::Relative(r.map_err(f)),
            UnlabelledWriteParam::Error(e) => UnlabelledWriteParam::Error(f(e)),
        }
    }
}

impl<'s, E> AstNode<E> for UnlabelledAbsoluteParam<'s, E> {
    fn constant_folding(self) -> Self {
        Self {
            value: self.value.constant_folding(),
        }
    }

    type ErrMapped<EE> = UnlabelledAbsoluteParam<'s, EE>;

    fn extract_errs(
        self,
        accumulator: &mut impl Accumulator<Error = impl From<E>>,
    ) -> Option<Self::ErrMapped<!>> {
        Some(UnlabelledAbsoluteParam {
            value: self.value.extract_errs(accumulator)?,
        })
    }

    fn max_unnamed_label(&self) -> Option<usize> {
        self.value.max_unnamed_label()
    }

    fn map_err<EE>(self, f: &mut impl FnMut(E) -> EE) -> Self::ErrMapped<EE> {
        UnlabelledAbsoluteParam {
            value: self.value.map_err(f),
        }
    }
}
impl<'s, E> AstNode<E> for UnlabelledImmediateParam<'s, E> {
    fn constant_folding(self) -> Self {
        Self {
            value: self.value.constant_folding(),
        }
    }

    type ErrMapped<EE> = UnlabelledImmediateParam<'s, EE>;

    fn extract_errs(
        self,
        accumulator: &mut impl Accumulator<Error = impl From<E>>,
    ) -> Option<Self::ErrMapped<!>> {
        Some(UnlabelledImmediateParam {
            value: self.value.extract_errs(accumulator)?,
        })
    }

    fn max_unnamed_label(&self) -> Option<usize> {
        self.value.max_unnamed_label()
    }

    fn map_err<EE>(self, f: &mut impl FnMut(E) -> EE) -> Self::ErrMapped<EE> {
        UnlabelledImmediateParam {
            value: self.value.map_err(f),
        }
    }
}
impl<'s, E> AstNode<E> for UnlabelledRelativeParam<'s, E> {
    fn constant_folding(self) -> Self {
        Self {
            value: self.value.constant_folding(),
        }
    }

    type ErrMapped<EE> = UnlabelledRelativeParam<'s, EE>;

    fn extract_errs(
        self,
        accumulator: &mut impl Accumulator<Error = impl From<E>>,
    ) -> Option<Self::ErrMapped<!>> {
        Some(UnlabelledRelativeParam {
            value: self.value.extract_errs(accumulator)?,
        })
    }

    fn max_unnamed_label(&self) -> Option<usize> {
        self.value.max_unnamed_label()
    }

    fn map_err<EE>(self, f: &mut impl FnMut(E) -> EE) -> Self::ErrMapped<EE> {
        UnlabelledRelativeParam {
            value: self.value.map_err(f),
        }
    }
}

impl<'s, E> AstNode<E> for Expression<'s, E> {
    fn constant_folding(self) -> Self {
        match self {
            Expression::Sum(a, b) => match (a.constant_folding(), b.constant_folding()) {
                (box Expression::Num(a), box Expression::Num(b)) => Expression::Num(a + b),
                (box Expression::Num(0), a) | (a, box Expression::Num(0)) => *a,
                (a, b) => Expression::Sum(a, b),
            },
            Expression::Sub(a, b) => match (a.constant_folding(), b.constant_folding()) {
                (box Expression::Num(a), box Expression::Num(b)) => Expression::Num(a - b),
                (a, box Expression::Num(0)) => *a,
                (box Expression::Num(0), box Expression::Neg(a)) => *a,
                (a, box Expression::Neg(b)) => Expression::Sum(a, b),
                (box Expression::Num(0), a) => Expression::Neg(a),
                (a, b) => Expression::Sub(a, b),
            },
            Expression::Mul(a, b) => match (a.constant_folding(), b.constant_folding()) {
                (box Expression::Num(a), box Expression::Num(b)) => Expression::Num(a * b),
                (box Expression::Num(0), _) | (_, box Expression::Num(0)) => Expression::Num(0),
                (box Expression::Num(1), a) | (a, box Expression::Num(1)) => *a,
                (a, b) => Expression::Mul(a, b),
            },
            Expression::Div(a, b) => match (a.constant_folding(), b.constant_folding()) {
                (box Expression::Num(a), box Expression::Num(b)) => Expression::Num(a / b),
                (box Expression::Num(0), _) => Expression::Num(0),
                (a, box Expression::Num(1)) => *a,
                (a, b) => Expression::Div(a, b),
            },
            Expression::Mod(a, b) => match (a.constant_folding(), b.constant_folding()) {
                (box Expression::Num(a), box Expression::Num(b)) => Expression::Num(a % b),
                (box Expression::Num(0), _) | (_, box Expression::Num(1)) => Expression::Num(0),
                (a, b) => Expression::Mod(a, b),
            },
            Expression::Neg(a) => match a.constant_folding() {
                box Expression::Num(a) => Expression::Num(-a),
                box Expression::Neg(a) => Box::into_inner(a),
                a => Expression::Neg(a),
            },
            Expression::Num(n) => Expression::Num(n),
            Expression::Ref(r) => Expression::Ref(r.constant_folding()),
            Expression::Error(e) => Expression::Error(e),
        }
    }

    type ErrMapped<EE> = Expression<'s, EE>;

    fn extract_errs(
        self,
        accumulator: &mut impl Accumulator<Error = impl From<E>>,
    ) -> Option<Self::ErrMapped<!>> {
        match self {
            Expression::Sum(a, b) => Some({
                let a = a.extract_errs(accumulator);
                let b = b.extract_errs(accumulator);
                Expression::Sum(a?, b?)
            }),
            Expression::Sub(a, b) => Some({
                let a = a.extract_errs(accumulator);
                let b = b.extract_errs(accumulator);
                Expression::Sub(a?, b?)
            }),
            Expression::Mul(a, b) => Some({
                let a = a.extract_errs(accumulator);
                let b = b.extract_errs(accumulator);
                Expression::Mul(a?, b?)
            }),
            Expression::Div(a, b) => Some({
                let a = a.extract_errs(accumulator);
                let b = b.extract_errs(accumulator);
                Expression::Div(a?, b?)
            }),
            Expression::Mod(a, b) => Some({
                let a = a.extract_errs(accumulator);
                let b = b.extract_errs(accumulator);
                Expression::Mod(a?, b?)
            }),
            Expression::Neg(a) => Some(Expression::Neg(a.extract_errs(accumulator)?)),
            Expression::Num(a) => Some(Expression::Num(a)),
            Expression::Ref(a) => Some(Expression::Ref(a.extract_errs(accumulator)?)),
            Expression::Error(e) => {
                accumulator.push(e);
                None
            }
        }
    }

    fn max_unnamed_label(&self) -> Option<usize> {
        match self {
            Expression::Sum(a, b)
            | Expression::Sub(a, b)
            | Expression::Mul(a, b)
            | Expression::Div(a, b)
            | Expression::Mod(a, b) => [a.max_unnamed_label(), b.max_unnamed_label()]
                .into_iter()
                .flatten()
                .max(),
            Expression::Neg(a) => a.max_unnamed_label(),
            Expression::Num(_) => None,
            Expression::Ref(LabelRef::Identifier(Identifier::Unnamed(n, ..))) => Some(*n),
            Expression::Ref(_) => None,
            Expression::Error(_) => None,
        }
    }

    fn map_err<EE>(self, f: &mut impl FnMut(E) -> EE) -> Self::ErrMapped<EE> {
        match self {
            Expression::Sum(a, b) => {
                let a = a.map_err(f);
                let b = b.map_err(f);
                Expression::Sum(a, b)
            }
            Expression::Sub(a, b) => {
                let a = a.map_err(f);
                let b = b.map_err(f);
                Expression::Sub(a, b)
            }
            Expression::Mul(a, b) => {
                let a = a.map_err(f);
                let b = b.map_err(f);
                Expression::Mul(a, b)
            }
            Expression::Div(a, b) => {
                let a = a.map_err(f);
                let b = b.map_err(f);
                Expression::Div(a, b)
            }
            Expression::Mod(a, b) => {
                let a = a.map_err(f);
                let b = b.map_err(f);
                Expression::Mod(a, b)
            }
            Expression::Neg(a) => Expression::Neg(a.map_err(f)),
            Expression::Num(a) => Expression::Num(a),
            Expression::Ref(a) => Expression::Ref(a.map_err(f)),
            Expression::Error(e) => Expression::Error(f(e)),
        }
    }
}
