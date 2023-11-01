use super::*;

trait StartsWithMinus {
    fn starts_with_minus(&self) -> bool;
}
impl<T: StartsWithMinus> StartsWithMinus for Box<T> {
    fn starts_with_minus(&self) -> bool {
        Box::as_ref(self).starts_with_minus()
    }
}

impl Unparse for Expression<'_> {
    fn unparse(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Sum(a, b) => {
                a.unparse(f)?;
                f.write_str(" + ")?;
                let parenthesized =
                    matches!(Box::as_ref(b), Expression::Sum(..) | Expression::Sub(..));
                if parenthesized {
                    f.write_char('(')?;
                }
                b.unparse(f)?;
                if parenthesized {
                    f.write_char(')')?;
                }
                Ok(())
            }
            Expression::Sub(a, b) => {
                a.unparse(f)?;
                f.write_str(" - ")?;
                let parenthesized =
                    matches!(Box::as_ref(b), Expression::Sum(..) | Expression::Sub(..));
                if parenthesized {
                    f.write_char('(')?;
                }
                b.unparse(f)?;
                if parenthesized {
                    f.write_char(')')?;
                }
                Ok(())
            }
            Expression::Mul(a, b) => {
                let parenthesized =
                    matches!(Box::as_ref(a), Expression::Sum(..) | Expression::Sub(..));
                if parenthesized {
                    f.write_char('(')?;
                }
                a.unparse(f)?;
                if parenthesized {
                    f.write_char(')')?;
                }
                f.write_str(" * ")?;
                let parenthesized = matches!(
                    Box::as_ref(b),
                    Expression::Sum(..)
                        | Expression::Sub(..)
                        | Expression::Div(..)
                        | Expression::Mod(..)
                );
                if parenthesized {
                    f.write_char('(')?;
                }
                b.unparse(f)?;
                if parenthesized {
                    f.write_char(')')?;
                }
                Ok(())
            }
            Expression::Div(a, b) => {
                let parenthesized =
                    matches!(Box::as_ref(a), Expression::Sum(..) | Expression::Sub(..));
                if parenthesized {
                    f.write_char('(')?;
                }
                a.unparse(f)?;
                if parenthesized {
                    f.write_char(')')?;
                }
                f.write_str(" / ")?;
                let parenthesized = matches!(
                    Box::as_ref(b),
                    Expression::Sum(..)
                        | Expression::Sub(..)
                        | Expression::Div(..)
                        | Expression::Mod(..)
                );
                if parenthesized {
                    f.write_char('(')?;
                }
                b.unparse(f)?;
                if parenthesized {
                    f.write_char(')')?;
                }
                Ok(())
            }
            Expression::Mod(a, b) => {
                let parenthesized =
                    matches!(Box::as_ref(a), Expression::Sum(..) | Expression::Sub(..));
                if parenthesized {
                    f.write_char('(')?;
                }
                a.unparse(f)?;
                if parenthesized {
                    f.write_char(')')?;
                }
                f.write_str(" % ")?;
                let parenthesized = matches!(
                    Box::as_ref(b),
                    Expression::Sum(..)
                        | Expression::Sub(..)
                        | Expression::Div(..)
                        | Expression::Mod(..)
                );
                if parenthesized {
                    f.write_char('(')?;
                }
                b.unparse(f)?;
                if parenthesized {
                    f.write_char(')')?;
                }
                Ok(())
            }
            Expression::Neg(a) => {
                f.write_char('-')?;
                let parenthesized =
                    !matches!(Box::as_ref(a), Expression::Ref(..) | Expression::Num(..));
                if parenthesized {
                    f.write_char('(')?;
                }
                a.unparse(f)?;
                if parenthesized {
                    f.write_char(')')?;
                }
                Ok(())
            }
            Expression::Num(n) => write!(f, "{n}"),
            Expression::Ref(r) => r.unparse(f),
            Expression::Error(e) => <!>::from(*e),
        }
    }
}

impl StartsWithMinus for Expression<'_> {
    fn starts_with_minus(&self) -> bool {
        match self {
            Expression::Sum(a, _)
            | Expression::Sub(a, _)
            | Expression::Mul(a, _)
            | Expression::Div(a, _)
            | Expression::Mod(a, _) => a.starts_with_minus(),
            Expression::Neg(_) => true,
            Expression::Num(n) => *n < 0,
            Expression::Ref(_) => false,
            Expression::Error(e) => <!>::from(*e),
        }
    }
}

impl NeedComma<Box<Expression<'_>>> for Labelled<'_, Box<Expression<'_>>> {
    fn need_comma(&self, before: &Box<Expression>) -> bool {
        if self.is_labelled() {
            false
        } else {
            self.content.need_comma(before)
        }
    }
}
impl NeedComma<Box<Expression<'_>>> for Box<Expression<'_>> {
    fn need_comma(&self, before: &Box<Expression>) -> bool {
        self.starts_with_minus()
    }
}
