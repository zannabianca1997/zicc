use super::*;

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
        self,
        solver: &mut impl FnMut(&LabelRef<'s>) -> Result<Option<<Self as AstNode<!>>::ErrMapped<E>>, E>,
    ) -> <Self as AstNode<!>>::ErrMapped<E> {
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
                Ok(None) => Expression::Ref(a.map_err(&mut |e| e)),
                Err(e) => Expression::Error(e),
            },
            Expression::Error(a) => a,
        }
    }
}
impl<'s> AddAssign<VMInt> for Expression<'s> {
    fn add_assign(&mut self, rhs: VMInt) {
        let taken = mem::replace(self, Expression::Num(0xdeadbeef));
        *self = Expression::Sum(Box::new(taken), Box::new(Expression::Num(rhs)))
    }
}