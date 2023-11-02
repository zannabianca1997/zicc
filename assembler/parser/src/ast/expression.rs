use std::{
    collections::BTreeMap,
    iter::repeat,
    num::NonZeroUsize,
    ops::{Add, Div, Mul, Neg, Rem, Sub},
};

use bincode::{BorrowDecode, Encode};
use serde::{Deserialize, Serialize};
use vm::NonZeroVMInt;

use super::*;

#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Encode, BorrowDecode, Serialize, Deserialize,
)]
pub enum Expression<'s, Error = Infallible> {
    Sum(Box<Expression<'s, Error>>, Box<Expression<'s, Error>>),
    Sub(Box<Expression<'s, Error>>, Box<Expression<'s, Error>>),
    Mul(Box<Expression<'s, Error>>, Box<Expression<'s, Error>>),
    Div(Box<Expression<'s, Error>>, Box<Expression<'s, Error>>),
    Mod(Box<Expression<'s, Error>>, Box<Expression<'s, Error>>),
    Neg(Box<Expression<'s, Error>>),
    Num(VMInt),
    #[serde(borrow)]
    Ref(LabelRef<'s, Error>),
    Error(Error),
}

impl<'s> Expression<'s> {
    pub fn replace<E>(
        self,
        solver: &mut impl FnMut(
            &LabelRef<'s>,
        )
            -> Result<Option<<Self as AstNode<Infallible>>::ErrMapped<E>>, E>,
    ) -> <Self as AstNode<Infallible>>::ErrMapped<E> {
        match self {
            Expression::Sum(a, b) => {
                Expression::Sum(Box::new(a.replace(solver)), Box::new(b.replace(solver)))
            }
            Expression::Sub(a, b) => {
                Expression::Sub(Box::new(a.replace(solver)), Box::new(b.replace(solver)))
            }
            Expression::Mul(a, b) => {
                Expression::Mul(Box::new(a.replace(solver)), Box::new(b.replace(solver)))
            }
            Expression::Div(a, b) => {
                Expression::Div(Box::new(a.replace(solver)), Box::new(b.replace(solver)))
            }
            Expression::Mod(a, b) => {
                Expression::Mod(Box::new(a.replace(solver)), Box::new(b.replace(solver)))
            }
            Expression::Neg(a) => Expression::Neg(Box::new(a.replace(solver))),
            Expression::Num(a) => Expression::Num(a),
            Expression::Ref(a) => match solver(&a) {
                Ok(Some(replacement)) => replacement,
                Ok(None) => Expression::Ref(a.map_err(&mut |e| <!>::from(e))),
                Err(e) => Expression::Error(e),
            },
            Expression::Error(e) => <!>::from(e),
        }
    }
}
impl<'s, E> Expression<'s, E> {
    pub fn simplify(self) -> Self {
        match FoldedExpression::try_from(self) {
            Ok(folded) => folded.into(),
            Err(partially_folded) => partially_folded,
        }
    }

    pub fn recursive_map(self, f: impl Fn(Self) -> Self) -> Self {
        match f(self) {
            Expression::Sum(a, b) => {
                Expression::Sum(Box::map_in_place(a, &f), Box::map_in_place(b, f))
            }
            Expression::Sub(a, b) => {
                Expression::Sub(Box::map_in_place(a, &f), Box::map_in_place(b, f))
            }
            Expression::Mul(a, b) => {
                Expression::Mul(Box::map_in_place(a, &f), Box::map_in_place(b, f))
            }
            Expression::Div(a, b) => {
                Expression::Div(Box::map_in_place(a, &f), Box::map_in_place(b, f))
            }
            Expression::Mod(a, b) => {
                Expression::Mod(Box::map_in_place(a, &f), Box::map_in_place(b, f))
            }
            Expression::Neg(a) => Expression::Neg(Box::map_in_place(a, f)),
            a => a,
        }
    }

    /// Iterate through all the sub_expressions
    pub fn nodes(&self) -> Nodes<'_, 's, E> {
        Nodes(vec![self])
    }
}

pub struct Nodes<'e, 's, E>(Vec<&'e Expression<'s, E>>);
impl<'e, 's, E> Iterator for Nodes<'e, 's, E> {
    type Item = &'e Expression<'s, E>;

    fn next(&mut self) -> Option<Self::Item> {
        let node = self.0.pop()?;
        match node {
            Expression::Sum(a, b)
            | Expression::Sub(a, b)
            | Expression::Mul(a, b)
            | Expression::Div(a, b)
            | Expression::Mod(a, b) => {
                self.0.push(a);
                self.0.push(b);
            }
            Expression::Neg(a) => self.0.push(a),
            _ => (),
        }
        Some(node)
    }
}

impl<'s> AddAssign<VMInt> for Expression<'s> {
    fn add_assign(&mut self, rhs: VMInt) {
        let taken = mem::replace(self, Expression::Num(0xdeadbeef));
        *self = Expression::Sum(Box::new(taken), Box::new(Expression::Num(rhs)))
    }
}
impl<'s, E> Add for Expression<'s, E> {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self::Sum(Box::new(self), Box::new(rhs))
    }
}
impl<'s, E> Sub for Expression<'s, E> {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        Self::Sub(Box::new(self), Box::new(rhs))
    }
}
impl<'s, E> Mul for Expression<'s, E> {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        Self::Mul(Box::new(self), Box::new(rhs))
    }
}
impl<'s, E> Div for Expression<'s, E> {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        Self::Div(Box::new(self), Box::new(rhs))
    }
}
impl<'s, E> Rem for Expression<'s, E> {
    type Output = Self;

    fn rem(self, rhs: Self) -> Self::Output {
        Self::Mod(Box::new(self), Box::new(rhs))
    }
}
impl<'s, E> Neg for Expression<'s, E> {
    type Output = Self;

    fn neg(self) -> Self::Output {
        Self::Neg(Box::new(self))
    }
}
impl<'s, E> From<VMInt> for Box<Expression<'s, E>> {
    fn from(value: VMInt) -> Self {
        Box::new(Expression::Num(value.into()))
    }
}
impl<'s, T> From<T> for Box<Expression<'s>>
where
    LabelRef<'s>: From<T>,
{
    fn from(value: T) -> Self {
        Box::new(Expression::Ref(value.into()))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
struct FoldedExpression<'s> {
    addends: BTreeMap<FoldedAddends<'s>, NonZeroVMInt>,
}
impl<'s> FoldedExpression<'s> {
    fn zero() -> FoldedExpression<'s> {
        Self {
            addends: BTreeMap::new(),
        }
    }
}
impl<'s> From<VMInt> for FoldedExpression<'s> {
    fn from(value: VMInt) -> Self {
        if let Some(value) = NonZeroVMInt::new(value) {
            Self {
                addends: BTreeMap::from([(FoldedAddends::unit(), value)]),
            }
        } else {
            Self::zero()
        }
    }
}
impl<'s> TryFrom<FoldedExpression<'s>> for VMInt {
    type Error = FoldedExpression<'s>;

    fn try_from(value: FoldedExpression<'s>) -> Result<Self, Self::Error> {
        match value.addends.len() {
            0 => Ok(0),
            1 => {
                let (addend, f) = value.addends.iter().next().unwrap();
                if addend.factors.is_empty() {
                    Ok(f.get())
                } else {
                    Err(value)
                }
            }
            _ => Err(value),
        }
    }
}
impl<'s> From<FoldedAtom<'s>> for FoldedExpression<'s> {
    fn from(value: FoldedAtom<'s>) -> Self {
        Self::from(FoldedAddends::from(value))
    }
}
impl<'s> From<FoldedAddends<'s>> for FoldedExpression<'s> {
    fn from(value: FoldedAddends<'s>) -> Self {
        Self {
            addends: BTreeMap::from([(value, NonZeroVMInt::new(1).unwrap())]),
        }
    }
}
impl<'s> Add for FoldedExpression<'s> {
    type Output = Self;

    fn add(mut self, rhs: Self) -> Self::Output {
        for (addend, factor) in rhs.addends {
            match self.addends.entry(addend) {
                std::collections::btree_map::Entry::Vacant(v) => {
                    v.insert(factor);
                }
                std::collections::btree_map::Entry::Occupied(mut o) => {
                    if let Some(t) = NonZeroVMInt::new((*o.get()).get() + factor.get()) {
                        o.insert(t); // updating the value
                    } else {
                        o.remove(); // they cancelled each other out
                    }
                }
            }
        }
        self
    }
}
impl<'s> Sub for FoldedExpression<'s> {
    type Output = Self;

    fn sub(mut self, rhs: Self) -> Self::Output {
        for (addend, factor) in rhs.addends {
            match self.addends.entry(addend) {
                std::collections::btree_map::Entry::Vacant(v) => {
                    v.insert(factor);
                }
                std::collections::btree_map::Entry::Occupied(mut o) => {
                    if let Some(t) = NonZeroVMInt::new((*o.get()).get() - factor.get()) {
                        o.insert(t); // updating the value
                    } else {
                        o.remove(); // they cancelled each other out
                    }
                }
            }
        }
        self
    }
}
impl<'s> Mul for FoldedExpression<'s> {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        let mut total = Self::zero();
        for (add1, fact1) in self.addends {
            for (add2, fact2) in &rhs.addends {
                let add = add1.clone() * add2.clone();
                total = total
                    + Self {
                        addends: BTreeMap::from([(
                            add,
                            NonZeroVMInt::new(fact1.get() * fact2.get())
                                .expect("The product of two nonzero should be nonzero"),
                        )]),
                    }
            }
        }
        total
    }
}

impl<'s> Neg for FoldedExpression<'s> {
    type Output = Self;

    fn neg(mut self) -> Self::Output {
        for v in self.addends.values_mut() {
            *v = -*v
        }
        self
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
struct FoldedAddends<'s> {
    factors: BTreeMap<FoldedAtom<'s>, NonZeroUsize>,
}
impl<'s> FoldedAddends<'s> {
    fn unit() -> FoldedAddends<'s> {
        Self {
            factors: BTreeMap::new(),
        }
    }
}
impl<'s> From<FoldedAtom<'s>> for FoldedAddends<'s> {
    fn from(value: FoldedAtom<'s>) -> Self {
        Self {
            factors: BTreeMap::from([(value, NonZeroUsize::new(1).unwrap())]),
        }
    }
}
impl<'s> Mul for FoldedAddends<'s> {
    type Output = FoldedAddends<'s>;

    fn mul(mut self, rhs: Self) -> Self::Output {
        for (factor, exponent) in rhs.factors {
            match self.factors.entry(factor) {
                std::collections::btree_map::Entry::Vacant(v) => {
                    v.insert(exponent);
                }
                std::collections::btree_map::Entry::Occupied(mut o) => {
                    if let Some(t) = NonZeroUsize::new((*o.get()).get() + exponent.get()) {
                        o.insert(t); // updating the value
                    } else {
                        o.remove(); // they cancelled each other out
                    }
                }
            }
        }
        self
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
enum FoldedAtom<'s> {
    Div(FoldedExpression<'s>, FoldedExpression<'s>),
    Mod(FoldedExpression<'s>, FoldedExpression<'s>),
    Ref(LabelRef<'s>),
}

impl<'s, E> TryFrom<Expression<'s, E>> for FoldedExpression<'s> {
    type Error = Expression<'s, E>;

    fn try_from(value: Expression<'s, E>) -> Result<Self, Self::Error> {
        match value {
            Expression::Sum(a, b) => match (Self::try_from(*a), Self::try_from(*b)) {
                (Ok(a), Ok(b)) => Ok(a + b),
                (Ok(a), Err(b)) => Err(Expression::Sum(Box::new(a.into()), Box::new(b))),
                (Err(a), Ok(b)) => Err(Expression::Sum(Box::new(a), Box::new(b.into()))),
                (Err(a), Err(b)) => Err(Expression::Sum(Box::new(a), Box::new(b))),
            },
            Expression::Sub(a, b) => match (Self::try_from(*a), Self::try_from(*b)) {
                (Ok(a), Ok(b)) => Ok(a - b),
                (Ok(a), Err(b)) => Err(Expression::Sub(Box::new(a.into()), Box::new(b))),
                (Err(a), Ok(b)) => Err(Expression::Sub(Box::new(a), Box::new(b.into()))),
                (Err(a), Err(b)) => Err(Expression::Sub(Box::new(a), Box::new(b))),
            },
            Expression::Mul(a, b) => match (Self::try_from(*a), Self::try_from(*b)) {
                (Ok(a), Ok(b)) => Ok(a * b),
                (Ok(a), Err(b)) => Err(Expression::Mul(Box::new(a.into()), Box::new(b))),
                (Err(a), Ok(b)) => Err(Expression::Mul(Box::new(a), Box::new(b.into()))),
                (Err(a), Err(b)) => Err(Expression::Mul(Box::new(a), Box::new(b))),
            },
            Expression::Div(a, b) => match (Self::try_from(*a), Self::try_from(*b)) {
                (Ok(a), Ok(b)) => Ok({
                    match (TryInto::<VMInt>::try_into(a), TryInto::<VMInt>::try_into(b)) {
                        (Ok(a), Ok(b)) => Self::from(a / b),
                        (Ok(0), Err(_)) => Self::from(0),
                        (Err(a), Ok(1)) => a,
                        (Err(a), Ok(-1)) => -a,

                        (Ok(a), Err(b)) => Self::from(FoldedAtom::Div(a.into(), b)),
                        (Err(a), Ok(b)) => Self::from(FoldedAtom::Div(a, b.into())),
                        (Err(a), Err(b)) => Self::from(FoldedAtom::Div(a, b)),
                    }
                }),
                (Ok(a), Err(b)) => Err(Expression::Div(Box::new(a.into()), Box::new(b))),
                (Err(a), Ok(b)) => Err(Expression::Div(Box::new(a), Box::new(b.into()))),
                (Err(a), Err(b)) => Err(Expression::Div(Box::new(a), Box::new(b))),
            },
            Expression::Mod(a, b) => match (Self::try_from(*a), Self::try_from(*b)) {
                (Ok(a), Ok(b)) => Ok({
                    match (TryInto::<VMInt>::try_into(a), TryInto::<VMInt>::try_into(b)) {
                        (Ok(a), Ok(b)) => Self::from(a % b),
                        (Ok(0), Err(_)) | (Err(_), Ok(1 | -1)) => Self::from(0),

                        (Ok(a), Err(b)) => Self::from(FoldedAtom::Mod(a.into(), b)),
                        (Err(a), Ok(b)) => Self::from(FoldedAtom::Mod(a, b.into())),
                        (Err(a), Err(b)) => Self::from(FoldedAtom::Mod(a, b)),
                    }
                }),
                (Ok(a), Err(b)) => Err(Expression::Mod(Box::new(a.into()), Box::new(b))),
                (Err(a), Ok(b)) => Err(Expression::Mod(Box::new(a), Box::new(b.into()))),
                (Err(a), Err(b)) => Err(Expression::Mod(Box::new(a), Box::new(b))),
            },
            Expression::Neg(a) => match Self::try_from(*a) {
                Ok(a) => Ok(-a),
                Err(a) => Err(Expression::Neg(Box::new(a))),
            },
            Expression::Num(n) => Ok(n.into()),
            Expression::Ref(LabelRef::Identifier(i)) => {
                Ok(FoldedAtom::Ref(LabelRef::Identifier(i)).into())
            }
            Expression::Ref(LabelRef::SpecialIdentifier(s)) => {
                Ok(FoldedAtom::Ref(LabelRef::SpecialIdentifier(s)).into())
            }
            Expression::Ref(LabelRef::Error(e)) => Err(Expression::Ref(LabelRef::Error(e))),
            Expression::Error(e) => Err(Expression::Error(e)),
        }
    }
}
impl<'s, E> From<FoldedExpression<'s>> for Expression<'s, E> {
    fn from(FoldedExpression { addends }: FoldedExpression<'s>) -> Self {
        addends
            .into_iter()
            .map(|(add, f)| match (f.get(), Self::from(add)) {
                (n, Expression::Num(1)) => Expression::Num(n),
                (_, Expression::Num(_)) => unreachable!(),
                (1, a) => a,
                (n, a) => Expression::Mul(Box::new(Expression::Num(n)), Box::new(a)),
            })
            .reduce(|a, b| a + b)
            .unwrap_or(Expression::Num(0))
    }
}
impl<'s, E> From<FoldedAddends<'s>> for Expression<'s, E> {
    fn from(FoldedAddends { factors }: FoldedAddends<'s>) -> Self {
        factors
            .into_iter()
            .flat_map(|(add, f)| repeat(add).map(Self::from).take(f.get()))
            .reduce(|a, b| a * b)
            .unwrap_or(Expression::Num(1))
    }
}
impl<'s, E> From<FoldedAtom<'s>> for Expression<'s, E> {
    fn from(value: FoldedAtom<'s>) -> Self {
        match value {
            FoldedAtom::Div(a, b) => Expression::Div(Box::new(a.into()), Box::new(b.into())),
            FoldedAtom::Mod(a, b) => Expression::Mod(Box::new(a.into()), Box::new(b.into())),
            FoldedAtom::Ref(LabelRef::Identifier(i)) => Expression::Ref(LabelRef::Identifier(i)),
            FoldedAtom::Ref(LabelRef::SpecialIdentifier(s)) => {
                Expression::Ref(LabelRef::SpecialIdentifier(s))
            }
            FoldedAtom::Ref(LabelRef::Error(e)) => <!>::from(e),
        }
    }
}
