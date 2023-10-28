//! Unified metods to navigate the ast

use super::*;

pub trait AstNode<E> {
    fn constant_folding(self) -> Self;

    type ErrMapped<EE>: AstNode<EE>;
    fn extract_errs(
        self,
        accumulator: &mut impl Accumulator<Error = impl From<E>>,
    ) -> Option<Self::ErrMapped<Infallible>>;

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
    ) -> Option<Self::ErrMapped<Infallible>> {
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
    ) -> Option<Self::ErrMapped<Infallible>> {
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
    ) -> Option<Self::ErrMapped<Infallible>> {
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
    ) -> Option<Self::ErrMapped<Infallible>> {
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
    ) -> Option<Self::ErrMapped<Infallible>> {
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

impl<E> AstNode<E> for StringLit<'_> {
    fn constant_folding(self) -> Self {
        self
    }

    type ErrMapped<EE> = Self;

    fn extract_errs(
        self,
        _accumulator: &mut impl Accumulator<Error = impl From<E>>,
    ) -> Option<Self::ErrMapped<Infallible>> {
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
    ) -> Option<Self::ErrMapped<Infallible>> {
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
            ) -> Option<Self::ErrMapped<Infallible>> {
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
    ) -> Option<Self::ErrMapped<Infallible>> {
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

mod expression;
mod params;
mod statements;
