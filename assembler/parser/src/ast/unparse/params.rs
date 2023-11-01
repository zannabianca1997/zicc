use super::*;

impl Unparse for ReadParam<'_> {
    fn unparse(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Absolute(p) => p.unparse(f),
            Self::Immediate(p) => p.unparse(f),
            Self::Relative(p) => p.unparse(f),
            Self::Error(e) => <!>::from(*e),
        }
    }
}
impl Unparse for WriteParam<'_> {
    fn unparse(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Absolute(p) => p.unparse(f),
            Self::Relative(p) => p.unparse(f),
            Self::Error(e) => <!>::from(*e),
        }
    }
}
impl Unparse for AbsoluteParam<'_> {
    fn unparse(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.value.unparse(f)
    }
}
impl Unparse for ImmediateParam<'_> {
    fn unparse(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_char('#')?;
        self.value.unparse(f)
    }
}
impl Unparse for RelativeParam<'_> {
    fn unparse(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_char('@')?;
        self.value.unparse(f)
    }
}

impl Unparse for UnlabelledReadParam<'_> {
    fn unparse(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Absolute(p) => p.unparse(f),
            Self::Immediate(p) => p.unparse(f),
            Self::Relative(p) => p.unparse(f),
            Self::Error(e) => <!>::from(*e),
        }
    }
}
impl Unparse for UnlabelledWriteParam<'_> {
    fn unparse(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Absolute(p) => p.unparse(f),
            Self::Relative(p) => p.unparse(f),
            Self::Error(e) => <!>::from(*e),
        }
    }
}
impl Unparse for UnlabelledAbsoluteParam<'_> {
    fn unparse(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.value.unparse(f)
    }
}
impl Unparse for UnlabelledImmediateParam<'_> {
    fn unparse(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_char('#')?;
        self.value.unparse(f)
    }
}
impl Unparse for UnlabelledRelativeParam<'_> {
    fn unparse(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_char('@')?;
        self.value.unparse(f)
    }
}

impl<T> NeedComma<ReadParam<'_>> for T
where
    T: for<'a> NeedComma<Box<Expression<'a>>>,
{
    fn need_comma(&self, before: &ReadParam<'_>) -> bool {
        self.need_comma(before.as_value())
    }
}
impl<T> NeedComma<WriteParam<'_>> for T
where
    T: for<'a> NeedComma<Box<Expression<'a>>>,
{
    fn need_comma(&self, before: &WriteParam<'_>) -> bool {
        self.need_comma(before.as_value())
    }
}
impl<T> NeedComma<UnlabelledReadParam<'_>> for T
where
    T: for<'a> NeedComma<Box<Expression<'a>>>,
{
    fn need_comma(&self, before: &UnlabelledReadParam<'_>) -> bool {
        self.need_comma(before.as_value())
    }
}
impl<T> NeedComma<UnlabelledWriteParam<'_>> for T
where
    T: for<'a> NeedComma<Box<Expression<'a>>>,
{
    fn need_comma(&self, before: &UnlabelledWriteParam<'_>) -> bool {
        self.need_comma(before.as_value())
    }
}

impl NeedComma<Box<Expression<'_>>> for ReadParam<'_> {
    fn need_comma(&self, before: &Box<Expression>) -> bool {
        if let Self::Absolute(a) = self {
            a.value.need_comma(before)
        } else {
            false
        }
    }
}
impl NeedComma<Box<Expression<'_>>> for WriteParam<'_> {
    fn need_comma(&self, before: &Box<Expression>) -> bool {
        if let Self::Absolute(a) = self {
            a.value.need_comma(before)
        } else {
            false
        }
    }
}
impl NeedComma<Box<Expression<'_>>> for UnlabelledReadParam<'_> {
    fn need_comma(&self, before: &Box<Expression>) -> bool {
        if let Self::Absolute(a) = self {
            a.value.need_comma(before)
        } else {
            false
        }
    }
}
impl NeedComma<Box<Expression<'_>>> for UnlabelledWriteParam<'_> {
    fn need_comma(&self, before: &Box<Expression>) -> bool {
        if let Self::Absolute(a) = self {
            a.value.need_comma(before)
        } else {
            false
        }
    }
}

impl Unparse for IntsParam<'_> {
    fn unparse(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            IntsParam::Int(n) => n.unparse(f),
            IntsParam::Str(s) => s.unparse(f),
        }
    }
}

impl NeedComma<IntsParam<'_>> for Labelled<'_, IntsParam<'_>> {
    fn need_comma(&self, before: &IntsParam<'_>) -> bool {
        if self.is_labelled() {
            false
        } else {
            self.content.need_comma(before)
        }
    }
}
impl NeedComma<IntsParam<'_>> for IntsParam<'_> {
    fn need_comma(&self, before: &IntsParam<'_>) -> bool {
        if let (IntsParam::Int(a), IntsParam::Int(b)) = (self, before) {
            a.need_comma(b)
        } else {
            false
        }
    }
}
