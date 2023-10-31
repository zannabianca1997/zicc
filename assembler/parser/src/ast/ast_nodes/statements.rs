use super::*;

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
    ) -> Option<Self::ErrMapped<Infallible>> {
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

impl<'s, E> AstNode<E> for IntsParam<'s, E> {
    fn constant_folding(self) -> Self {
        match self {
            IntsParam::Int(i) => IntsParam::Int(i.constant_folding()),
            IntsParam::Str(s) => IntsParam::Str(AstNode::<E>::constant_folding(s)),
        }
    }

    type ErrMapped<EE> = IntsParam<'s, EE>;

    fn extract_errs(
        self,
        accumulator: &mut impl Accumulator<Error = impl From<E>>,
    ) -> Option<Self::ErrMapped<Infallible>> {
        match self {
            IntsParam::Int(i) => i.extract_errs(accumulator).map(IntsParam::Int),
            IntsParam::Str(s) => s.extract_errs(accumulator).map(IntsParam::Str),
        }
    }

    fn map_err<EE>(self, f: &mut impl FnMut(E) -> EE) -> Self::ErrMapped<EE> {
        match self {
            IntsParam::Int(i) => IntsParam::Int(i.map_err(f)),
            IntsParam::Str(s) => IntsParam::Str(s.map_err(f)),
        }
    }

    fn max_unnamed_label(&self) -> Option<usize> {
        match self {
            IntsParam::Int(i) => i.max_unnamed_label(),
            IntsParam::Str(s) => AstNode::<E>::max_unnamed_label(s),
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
    ) -> Option<Self::ErrMapped<Infallible>> {
        Some(ZerosStm(self.0.extract_errs(accumulator)?))
    }

    fn max_unnamed_label(&self) -> Option<usize> {
        self.0.max_unnamed_label()
    }

    fn map_err<EE>(self, f: &mut impl FnMut(E) -> EE) -> Self::ErrMapped<EE> {
        ZerosStm(self.0.map_err(f))
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
    ) -> Option<Self::ErrMapped<Infallible>> {
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
    ) -> Option<Self::ErrMapped<Infallible>> {
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
    ) -> Option<Self::ErrMapped<Infallible>> {
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
    ) -> Option<Self::ErrMapped<Infallible>> {
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

impl<'s, E> AstNode<E> for LoadStm<'s, E> {
    fn constant_folding(self) -> Self {
        match self {
            LoadStm::Single { relative, ptr, to } => Self::Single {
                relative,
                ptr: ptr.constant_folding(),
                to: to.constant_folding(),
            },
            LoadStm::Multiple {
                relative,
                ptr,
                to,
                n,
            } => Self::Multiple {
                relative,
                ptr: ptr.constant_folding(),
                to: to.constant_folding(),
                n: n.constant_folding(),
            },
        }
    }

    type ErrMapped<EE> = LoadStm<'s, EE>;

    fn extract_errs(
        self,
        accumulator: &mut impl Accumulator<Error = impl From<E>>,
    ) -> Option<Self::ErrMapped<Infallible>> {
        Some(match self {
            LoadStm::Single { relative, ptr, to } => LoadStm::Single {
                relative,
                ptr: ptr.extract_errs(accumulator)?,
                to: to.extract_errs(accumulator)?,
            },
            LoadStm::Multiple {
                relative,
                ptr,
                to,
                n,
            } => LoadStm::Multiple {
                relative,
                ptr: ptr.extract_errs(accumulator)?,
                to: to.extract_errs(accumulator)?,
                n: n.extract_errs(accumulator)?,
            },
        })
    }

    fn map_err<EE>(self, f: &mut impl FnMut(E) -> EE) -> Self::ErrMapped<EE> {
        match self {
            LoadStm::Single { relative, ptr, to } => LoadStm::Single {
                relative,
                ptr: ptr.map_err(f),
                to: to.map_err(f),
            },
            LoadStm::Multiple {
                relative,
                ptr,
                to,
                n,
            } => LoadStm::Multiple {
                relative,
                ptr: ptr.map_err(f),
                to: to.map_err(f),
                n: n.map_err(f),
            },
        }
    }

    fn max_unnamed_label(&self) -> Option<usize> {
        match self {
            LoadStm::Single { relative, ptr, to } => {
                [ptr.max_unnamed_label(), to.max_unnamed_label()]
                    .into_iter()
                    .flatten()
                    .max()
            }
            LoadStm::Multiple {
                relative,
                ptr,
                to,
                n,
            } => [
                ptr.max_unnamed_label(),
                to.max_unnamed_label(),
                n.max_unnamed_label(),
            ]
            .into_iter()
            .flatten()
            .max(),
        }
    }
}
impl<'s, E> AstNode<E> for StoreStm<'s, E> {
    fn constant_folding(self) -> Self {
        match self {
            StoreStm::Single {
                relative,
                ptr,
                from,
            } => Self::Single {
                relative,
                ptr: ptr.constant_folding(),
                from: from.constant_folding(),
            },
            StoreStm::Multiple {
                relative,
                ptr,
                from,
                n,
            } => Self::Multiple {
                relative,
                ptr: ptr.constant_folding(),
                from: from.constant_folding(),
                n: n.constant_folding(),
            },
        }
    }

    type ErrMapped<EE> = StoreStm<'s, EE>;

    fn extract_errs(
        self,
        accumulator: &mut impl Accumulator<Error = impl From<E>>,
    ) -> Option<Self::ErrMapped<Infallible>> {
        Some(match self {
            StoreStm::Single {
                relative,
                ptr,
                from,
            } => StoreStm::Single {
                relative,
                ptr: ptr.extract_errs(accumulator)?,
                from: from.extract_errs(accumulator)?,
            },
            StoreStm::Multiple {
                relative,
                ptr,
                from,
                n,
            } => StoreStm::Multiple {
                relative,
                ptr: ptr.extract_errs(accumulator)?,
                from: from.extract_errs(accumulator)?,
                n: n.extract_errs(accumulator)?,
            },
        })
    }

    fn map_err<EE>(self, f: &mut impl FnMut(E) -> EE) -> Self::ErrMapped<EE> {
        match self {
            StoreStm::Single {
                relative,
                from,
                ptr,
            } => StoreStm::Single {
                relative,
                from: from.map_err(f),
                ptr: ptr.map_err(f),
            },
            StoreStm::Multiple {
                relative,
                ptr,
                from,
                n,
            } => StoreStm::Multiple {
                relative,
                ptr: ptr.map_err(f),
                from: from.map_err(f),
                n: n.map_err(f),
            },
        }
    }

    fn max_unnamed_label(&self) -> Option<usize> {
        match self {
            StoreStm::Single {
                relative,
                from,
                ptr,
            } => [from.max_unnamed_label(), ptr.max_unnamed_label()]
                .into_iter()
                .flatten()
                .max(),
            StoreStm::Multiple {
                relative,
                from,
                ptr,
                n,
            } => [
                from.max_unnamed_label(),
                ptr.max_unnamed_label(),
                n.max_unnamed_label(),
            ]
            .into_iter()
            .flatten()
            .max(),
        }
    }
}

impl<'s, E> AstNode<E> for CallStm<'s, E> {
    fn constant_folding(self) -> Self {
        Self(self.0.constant_folding(), self.1.constant_folding())
    }

    type ErrMapped<EE> = CallStm<'s, EE>;

    fn extract_errs(
        self,
        accumulator: &mut impl Accumulator<Error = impl From<E>>,
    ) -> Option<Self::ErrMapped<Infallible>> {
        Some(CallStm(
            self.0.extract_errs(accumulator)?,
            self.1.extract_errs(accumulator)?,
        ))
    }

    fn max_unnamed_label(&self) -> Option<usize> {
        [self.0.max_unnamed_label(), self.1.max_unnamed_label()]
            .into_iter()
            .flatten()
            .max()
    }

    fn map_err<EE>(self, f: &mut impl FnMut(E) -> EE) -> Self::ErrMapped<EE> {
        CallStm(self.0.map_err(f), self.1.map_err(f))
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

impl<'s, E> AstNode<E> for ExportStm<'s> {
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
    ) -> Option<Self::ErrMapped<Infallible>> {
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
