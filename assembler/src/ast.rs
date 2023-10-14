use std::collections::BTreeSet;

use either::Either;
use lalrpop_util::lalrpop_mod;
use thiserror::Error;

use vm::VMInt;

use crate::lexer::{Identifier, LexError, Lexer, SpecialIdentifier, StringLit, Token};

lalrpop_mod!(grammar);

#[derive(Debug, Error)]
pub enum ParseError {
    #[error(transparent)]
    Lex(#[from] LexError),
    #[error(transparent)]
    Parser(lalrpop_util::ParseError<usize, String, !>),
}

impl From<lalrpop_util::ParseError<usize, Token<'_>, ParseError>> for ParseError {
    fn from(value: lalrpop_util::ParseError<usize, Token<'_>, ParseError>) -> Self {
        if let lalrpop_util::ParseError::User { error } = value {
            error
        } else {
            ParseError::Parser(
                value
                    .map_error(|_| unreachable!())
                    .map_token(|t| t.to_string()),
            )
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct File<'s> {
    pub statements: Vec<Labelled<'s, Option<Statement<'s>>>>,
}

impl<'s> File<'s> {
    pub fn parse(source: &'s str) -> Result<Self, ParseError> {
        grammar::FileParser::new()
            .parse(Lexer::new(source).map(|t| t.map_err(Into::into)))
            .map_err(Into::into)
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
pub enum LabelRef<'s> {
    Identifier(Identifier<'s>),
    SpecialIdentifier(SpecialIdentifier),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Statement<'s> {
    IntsStm(IntsStm<'s>),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct IntsStm<'s> {
    pub values: Vec<Labelled<'s, Either<Box<Expression<'s>>, StringLit<'s>>>>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ReadParam<'s> {
    Absolute(AbsoluteParam<'s>),
    Immediate(ImmediateParam<'s>),
    Relative(RelativeParam<'s>),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum WriteParam<'s> {
    Absolute(AbsoluteParam<'s>),
    Relative(RelativeParam<'s>),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ImmediateParam<'s> {
    value: Labelled<'s, Box<Expression<'s>>>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct AbsoluteParam<'s> {
    value: Labelled<'s, Box<Expression<'s>>>,
}
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RelativeParam<'s> {
    value: Labelled<'s, Box<Expression<'s>>>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Expression<'s> {
    Sum(Box<Expression<'s>>, Box<Expression<'s>>),
    Sub(Box<Expression<'s>>, Box<Expression<'s>>),
    Mul(Box<Expression<'s>>, Box<Expression<'s>>),
    Div(Box<Expression<'s>>, Box<Expression<'s>>),
    Mod(Box<Expression<'s>>, Box<Expression<'s>>),
    Neg(Box<Expression<'s>>),
    Num(VMInt),
    Ref(LabelRef<'s>),
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
        })
    }

    pub(crate) fn replace(&mut self, refer: LabelRef<'s>, value: VMInt) {
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
        }
    }
}

pub mod constant_folding {
    use crate::lexer::StringLit;

    use super::{Expression, File, IntsStm, Labelled, Statement};
    use either::Either;
    use itertools::Itertools;
    use map_in_place::MapBoxInPlace;

    pub trait ConstantFolding {
        fn constant_folding(self) -> Self;
    }

    impl ConstantFolding for File<'_> {
        fn constant_folding(self) -> Self {
            Self {
                statements: self
                    .statements
                    .into_iter()
                    .map(ConstantFolding::constant_folding)
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
    }

    impl<T: ConstantFolding> ConstantFolding for Labelled<'_, T> {
        fn constant_folding(self) -> Self {
            Self {
                labels: self.labels,
                content: self.content.constant_folding(),
            }
        }
    }

    impl<T: ConstantFolding> ConstantFolding for Option<T> {
        fn constant_folding(self) -> Self {
            self.map(ConstantFolding::constant_folding)
        }
    }
    impl<T: ConstantFolding> ConstantFolding for Box<T> {
        fn constant_folding(self) -> Self {
            Box::map_in_place(self, ConstantFolding::constant_folding)
        }
    }
    impl<T: ConstantFolding> ConstantFolding for Vec<T> {
        fn constant_folding(self) -> Self {
            self.into_iter()
                .map(ConstantFolding::constant_folding)
                .collect()
        }
    }
    impl<L: ConstantFolding, R: ConstantFolding> ConstantFolding for Either<L, R> {
        fn constant_folding(self) -> Self {
            self.map_either(
                ConstantFolding::constant_folding,
                ConstantFolding::constant_folding,
            )
        }
    }

    impl ConstantFolding for StringLit<'_> {
        fn constant_folding(self) -> Self {
            self
        }
    }

    impl ConstantFolding for Statement<'_> {
        fn constant_folding(self) -> Self {
            match self {
                Statement::IntsStm(ints) => Self::IntsStm(ints.constant_folding()),
            }
        }
    }

    impl ConstantFolding for IntsStm<'_> {
        fn constant_folding(self) -> Self {
            Self {
                values: self.values.constant_folding(),
            }
        }
    }

    impl ConstantFolding for Expression<'_> {
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
                Expression::Ref(r) => Expression::Ref(r),
            }
        }
    }
}
