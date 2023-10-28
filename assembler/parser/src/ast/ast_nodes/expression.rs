use super::*;

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
    ) -> Option<Self::ErrMapped<Infallible>> {
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
