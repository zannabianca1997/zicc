use super::*;

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
    ) -> Option<Self::ErrMapped<Infallible>> {
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
    ) -> Option<Self::ErrMapped<Infallible>> {
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
    ) -> Option<Self::ErrMapped<Infallible>> {
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
    ) -> Option<Self::ErrMapped<Infallible>> {
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
    ) -> Option<Self::ErrMapped<Infallible>> {
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
    ) -> Option<Self::ErrMapped<Infallible>> {
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
    ) -> Option<Self::ErrMapped<Infallible>> {
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
    ) -> Option<Self::ErrMapped<Infallible>> {
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
    ) -> Option<Self::ErrMapped<Infallible>> {
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
    ) -> Option<Self::ErrMapped<Infallible>> {
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
