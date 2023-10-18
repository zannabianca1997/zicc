use std::{collections::BTreeSet, ops::Range};

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
}
impl Spanned for ParseErrorContent {
    fn span(&self) -> Range<usize> {
        match self {
            ParseErrorContent::Lex(err) => err.span(),
            ParseErrorContent::ImmediateInWrite { span } => span.clone(),
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
    IntsStm(IntsStm<'s, Error>),
    Instruction(Instruction<'s, Error>),
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
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ImmediateParam<'s, Error = !> {
    value: Labelled<'s, Box<Expression<'s, Error>>>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct AbsoluteParam<'s, Error = !> {
    value: Labelled<'s, Box<Expression<'s, Error>>>,
}
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RelativeParam<'s, Error = !> {
    value: Labelled<'s, Box<Expression<'s, Error>>>,
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
    pub fn calculate(
        &self,
        solver: &mut impl FnMut(&LabelRef<'s>) -> Option<VMInt>,
    ) -> Option<VMInt> {
        Some(match self {
            Expression::Sum(a, b) => a.calculate(solver)? + b.calculate(solver)?,
            Expression::Sub(a, b) => a.calculate(solver)? - b.calculate(solver)?,
            Expression::Mul(a, b) => a.calculate(solver)? * b.calculate(solver)?,
            Expression::Div(a, b) => a.calculate(solver)? / b.calculate(solver)?,
            Expression::Mod(a, b) => a.calculate(solver)? % b.calculate(solver)?,
            Expression::Neg(a) => -a.calculate(solver)?,
            Expression::Num(n) => *n,
            Expression::Ref(refer) => solver(refer)?,
            Expression::Error(e) => *e,
        })
    }

    pub fn replace(&mut self, refer: LabelRef<'s>, value: VMInt) {
        match self {
            Expression::Sum(a, b)
            | Expression::Sub(a, b)
            | Expression::Mul(a, b)
            | Expression::Div(a, b)
            | Expression::Mod(a, b) => {
                a.replace(refer, value);
                b.replace(refer, value)
            }
            Expression::Neg(a) => a.replace(refer, value),
            Expression::Num(_) => (),
            Expression::Ref(r) if r == &refer => *self = Expression::Num(value),
            Expression::Ref(_) => (),
            Expression::Error(e) => *e,
        }
    }
}

pub trait AstNode<E> {
    fn constant_folding(self) -> Self;

    type ErrMapped<EE>: AstNode<EE>;
    fn extract_errs(
        self,
        accumulator: &mut impl Accumulator<Error = impl From<E>>,
    ) -> Option<Self::ErrMapped<!>>;
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
}

impl<'s, E> AstNode<E> for Statement<'s, E> {
    fn constant_folding(self) -> Self {
        match self {
            Statement::IntsStm(ints) => Self::IntsStm(ints.constant_folding()),
            Statement::Instruction(instr) => Statement::Instruction(instr.constant_folding()),
            Statement::Error(e) => Statement::Error(e),
        }
    }

    type ErrMapped<EE> = Statement<'s, EE>;

    fn extract_errs(
        self,
        accumulator: &mut impl Accumulator<Error = impl From<E>>,
    ) -> Option<Self::ErrMapped<!>> {
        match self {
            Statement::IntsStm(ints) => Some(Statement::IntsStm(ints.extract_errs(accumulator)?)),
            Statement::Instruction(instr) => {
                Some(Statement::Instruction(instr.extract_errs(accumulator)?))
            }
            Statement::Error(e) => {
                accumulator.push(e);
                None
            }
        }
    }
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
}

impl<'s, E> AstNode<E> for Expression<'s, E> {
    fn constant_folding(self) -> Self {
        match self {
            Expression::Sum(a, b) => match (a.constant_folding(), b.constant_folding()) {
                (box Expression::Num(a), box Expression::Num(b)) => Expression::Num(a + b),
                (a, b) => Expression::Sum(a, b),
            },
            Expression::Sub(a, b) => match (a.constant_folding(), b.constant_folding()) {
                (box Expression::Num(a), box Expression::Num(b)) => Expression::Num(a - b),
                (a, b) => Expression::Sub(a, b),
            },
            Expression::Mul(a, b) => match (a.constant_folding(), b.constant_folding()) {
                (box Expression::Num(a), box Expression::Num(b)) => Expression::Num(a * b),
                (a, b) => Expression::Mul(a, b),
            },
            Expression::Div(a, b) => match (a.constant_folding(), b.constant_folding()) {
                (box Expression::Num(a), box Expression::Num(b)) => Expression::Num(a / b),
                (a, b) => Expression::Div(a, b),
            },
            Expression::Mod(a, b) => match (a.constant_folding(), b.constant_folding()) {
                (box Expression::Num(a), box Expression::Num(b)) => Expression::Num(a % b),
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
}
