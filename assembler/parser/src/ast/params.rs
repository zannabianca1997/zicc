use super::*;

#[derive(
    Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize, Encode, BorrowDecode,
)]
pub enum ReadParam<'s, Error = Infallible> {
    Absolute(#[serde(borrow)] AbsoluteParam<'s, Error>),
    Immediate(#[serde(borrow)] ImmediateParam<'s, Error>),
    Relative(#[serde(borrow)] RelativeParam<'s, Error>),
    Error(Error),
}
impl<'s> ReadParam<'s> {
    pub fn mode(&self) -> VMInt {
        match self {
            ReadParam::Absolute(_) => 0,
            ReadParam::Immediate(_) => 1,
            ReadParam::Relative(_) => 2,
            ReadParam::Error(e) => <!>::from(*e),
        }
    }
    pub fn as_value_mut(&mut self) -> &mut Labelled<'s, Box<Expression<'s>>> {
        match self {
            ReadParam::Absolute(AbsoluteParam { value })
            | ReadParam::Immediate(ImmediateParam { value })
            | ReadParam::Relative(RelativeParam { value }) => value,
            ReadParam::Error(e) => <!>::from(*e),
        }
    }
    pub fn into_value(self) -> Labelled<'s, Box<Expression<'s>>> {
        match self {
            ReadParam::Absolute(AbsoluteParam { value })
            | ReadParam::Immediate(ImmediateParam { value })
            | ReadParam::Relative(RelativeParam { value }) => value,
            ReadParam::Error(e) => <!>::from(e),
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
impl<'s, E> From<UnlabelledReadParam<'s, E>> for ReadParam<'s, E> {
    fn from(value: UnlabelledReadParam<'s, E>) -> Self {
        match value {
            UnlabelledReadParam::Absolute(a) => ReadParam::Absolute(a.into()),
            UnlabelledReadParam::Immediate(i) => ReadParam::Immediate(i.into()),
            UnlabelledReadParam::Relative(r) => ReadParam::Relative(r.into()),
            UnlabelledReadParam::Error(e) => ReadParam::Error(e),
        }
    }
}
impl<'s, E> From<UnlabelledWriteParam<'s, E>> for ReadParam<'s, E> {
    fn from(value: UnlabelledWriteParam<'s, E>) -> Self {
        match value {
            UnlabelledWriteParam::Absolute(a) => ReadParam::Absolute(a.into()),
            UnlabelledWriteParam::Relative(r) => ReadParam::Relative(r.into()),
            UnlabelledWriteParam::Error(e) => ReadParam::Error(e),
        }
    }
}
impl<'s, E> From<Box<Expression<'s, E>>> for ReadParam<'s, E> {
    fn from(value: Box<Expression<'s, E>>) -> Self {
        Self::Absolute(AbsoluteParam {
            value: Labelled {
                labels: BTreeSet::new(),
                content: value,
            },
        })
    }
}

pub type NonImmediateReadParam<'s, Error = Infallible> = WriteParam<'s, Error>;

#[derive(
    Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize, Encode, BorrowDecode,
)]
pub enum WriteParam<'s, Error = Infallible> {
    Absolute(#[serde(borrow)] AbsoluteParam<'s, Error>),
    Relative(#[serde(borrow)] RelativeParam<'s, Error>),
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
            WriteParam::Error(e) => <!>::from(*e),
        }
    }
    pub fn into_value(self) -> Labelled<'s, Box<Expression<'s>>> {
        match self {
            WriteParam::Absolute(AbsoluteParam { value })
            | WriteParam::Relative(RelativeParam { value }) => value,
            WriteParam::Error(e) => <!>::from(e),
        }
    }

    pub fn as_value_mut(&mut self) -> &mut Labelled<'s, Box<Expression<'s>>> {
        match self {
            WriteParam::Absolute(AbsoluteParam { value })
            | WriteParam::Relative(RelativeParam { value }) => value,
            WriteParam::Error(e) => <!>::from(*e),
        }
    }
}
impl<'s, E> From<UnlabelledWriteParam<'s, E>> for WriteParam<'s, E> {
    fn from(value: UnlabelledWriteParam<'s, E>) -> Self {
        match value {
            UnlabelledWriteParam::Absolute(a) => WriteParam::Absolute(a.into()),
            UnlabelledWriteParam::Relative(r) => WriteParam::Relative(r.into()),
            UnlabelledWriteParam::Error(e) => WriteParam::Error(e),
        }
    }
}
impl<'s, E> From<Box<Expression<'s, E>>> for WriteParam<'s, E> {
    fn from(value: Box<Expression<'s, E>>) -> Self {
        Self::Absolute(AbsoluteParam {
            value: Labelled {
                labels: BTreeSet::new(),
                content: value,
            },
        })
    }
}
#[derive(
    Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize, Encode, BorrowDecode,
)]
pub struct ImmediateParam<'s, Error = Infallible> {
    #[serde(borrow)]
    pub value: Labelled<'s, Box<Expression<'s, Error>>>,
}
impl<'s, E> From<UnlabelledImmediateParam<'s, E>> for ImmediateParam<'s, E> {
    fn from(value: UnlabelledImmediateParam<'s, E>) -> Self {
        Self {
            value: Labelled {
                labels: BTreeSet::new(),
                content: value.value,
            },
        }
    }
}

#[derive(
    Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize, Encode, BorrowDecode,
)]
pub struct AbsoluteParam<'s, Error = Infallible> {
    #[serde(borrow)]
    pub value: Labelled<'s, Box<Expression<'s, Error>>>,
}
impl<'s, E> From<UnlabelledAbsoluteParam<'s, E>> for AbsoluteParam<'s, E> {
    fn from(value: UnlabelledAbsoluteParam<'s, E>) -> Self {
        Self {
            value: Labelled {
                labels: BTreeSet::new(),
                content: value.value,
            },
        }
    }
}

#[derive(
    Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize, Encode, BorrowDecode,
)]
pub struct RelativeParam<'s, Error = Infallible> {
    #[serde(borrow)]
    pub value: Labelled<'s, Box<Expression<'s, Error>>>,
}
impl<'s, E> From<UnlabelledRelativeParam<'s, E>> for RelativeParam<'s, E> {
    fn from(value: UnlabelledRelativeParam<'s, E>) -> Self {
        Self {
            value: Labelled {
                labels: BTreeSet::new(),
                content: value.value,
            },
        }
    }
}

#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize, Encode, BorrowDecode,
)]
pub enum UnlabelledReadParam<'s, Error = Infallible> {
    Absolute(#[serde(borrow)] UnlabelledAbsoluteParam<'s, Error>),
    Immediate(#[serde(borrow)] UnlabelledImmediateParam<'s, Error>),
    Relative(#[serde(borrow)] UnlabelledRelativeParam<'s, Error>),
    Error(Error),
}
impl<'s> UnlabelledReadParam<'s> {
    pub fn mode(&self) -> VMInt {
        match self {
            UnlabelledReadParam::Absolute(_) => 0,
            UnlabelledReadParam::Immediate(_) => 1,
            UnlabelledReadParam::Relative(_) => 2,
            UnlabelledReadParam::Error(e) => <!>::from(*e),
        }
    }
}
impl<'s, E> From<UnlabelledWriteParam<'s, E>> for UnlabelledReadParam<'s, E> {
    fn from(value: UnlabelledWriteParam<'s, E>) -> Self {
        match value {
            UnlabelledWriteParam::Absolute(a) => UnlabelledReadParam::Absolute(a),
            UnlabelledWriteParam::Relative(r) => UnlabelledReadParam::Relative(r),
            UnlabelledWriteParam::Error(e) => UnlabelledReadParam::Error(e),
        }
    }
}

pub type UnlabelledNonImmediateReadParam<'s, Error = Infallible> = UnlabelledWriteParam<'s, Error>;

#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize, Encode, BorrowDecode,
)]
pub enum UnlabelledWriteParam<'s, Error = Infallible> {
    Absolute(#[serde(borrow)] UnlabelledAbsoluteParam<'s, Error>),
    Relative(#[serde(borrow)] UnlabelledRelativeParam<'s, Error>),
    Error(Error),
}

impl<'s, E> UnlabelledWriteParam<'s, E> {
    pub fn map_value(
        self,
        f: impl FnOnce(Box<Expression<'s, E>>) -> Box<Expression<'s, E>>,
    ) -> Self {
        match self {
            UnlabelledWriteParam::Absolute(a) => UnlabelledWriteParam::Absolute(a.map_value(f)),
            UnlabelledWriteParam::Relative(r) => UnlabelledWriteParam::Relative(r.map_value(f)),
            UnlabelledWriteParam::Error(e) => UnlabelledWriteParam::Error(e),
        }
    }
    pub fn from_read(p: UnlabelledReadParam<'s, E>) -> Option<Self> {
        match p {
            UnlabelledReadParam::Absolute(a) => Some(Self::Absolute(a)),
            UnlabelledReadParam::Immediate(_) => None,
            UnlabelledReadParam::Relative(r) => Some(Self::Relative(r)),
            UnlabelledReadParam::Error(e) => Some(Self::Error(e)),
        }
    }
}
impl<'s> UnlabelledWriteParam<'s> {
    pub fn mode(&self) -> VMInt {
        match self {
            UnlabelledWriteParam::Absolute(_) => 0,
            UnlabelledWriteParam::Relative(_) => 2,
            UnlabelledWriteParam::Error(e) => <!>::from(*e),
        }
    }
}

impl<'s, E> TryFrom<WriteParam<'s, E>> for UnlabelledWriteParam<'s, E> {
    type Error = ParseErrorContent;

    fn try_from(
        value: WriteParam<'s, E>,
    ) -> Result<Self, <Self as TryFrom<WriteParam<'s, E>>>::Error> {
        Ok(match value {
            WriteParam::Absolute(a) => Self::Absolute(a.try_into()?),
            WriteParam::Relative(r) => Self::Relative(r.try_into()?),
            WriteParam::Error(e) => Self::Error(e),
        })
    }
}

#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize, Encode, BorrowDecode,
)]
pub struct UnlabelledImmediateParam<'s, Error = Infallible> {
    #[serde(borrow)]
    pub value: Box<Expression<'s, Error>>,
}

#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize, Encode, BorrowDecode,
)]
pub struct UnlabelledAbsoluteParam<'s, Error = Infallible> {
    #[serde(borrow)]
    pub value: Box<Expression<'s, Error>>,
}
impl<'s, E> UnlabelledAbsoluteParam<'s, E> {
    pub fn map_value(
        self,
        f: impl FnOnce(Box<Expression<'s, E>>) -> Box<Expression<'s, E>>,
    ) -> Self {
        Self {
            value: f(self.value),
        }
    }
}
impl<'s, E> TryFrom<AbsoluteParam<'s, E>> for UnlabelledAbsoluteParam<'s, E> {
    type Error = ParseErrorContent;

    fn try_from(
        value: AbsoluteParam<'s, E>,
    ) -> Result<Self, <Self as TryFrom<AbsoluteParam<'s, E>>>::Error> {
        let AbsoluteParam {
            value: Labelled { labels, content },
        } = value;
        if labels.is_empty() {
            Ok(Self { value: content })
        } else {
            Err(ParseErrorContent::LabelsOnUnlabelled {
                span: labels
                    .into_iter()
                    .map(|LabelDef { label }| label.span())
                    .reduce(|s1, s2| (s1.start.min(s2.start))..(s1.end.min(s2.end)))
                    .unwrap(),
            })
        }
    }
}
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize, Encode, BorrowDecode,
)]
pub struct UnlabelledRelativeParam<'s, Error = Infallible> {
    #[serde(borrow)]
    pub value: Box<Expression<'s, Error>>,
}
impl<'s, E> TryFrom<RelativeParam<'s, E>> for UnlabelledRelativeParam<'s, E> {
    type Error = ParseErrorContent;

    fn try_from(
        value: RelativeParam<'s, E>,
    ) -> Result<Self, <Self as TryFrom<RelativeParam<'s, E>>>::Error> {
        let RelativeParam {
            value: Labelled { labels, content },
        } = value;
        if labels.is_empty() {
            Ok(Self { value: content })
        } else {
            Err(ParseErrorContent::LabelsOnUnlabelled {
                span: labels
                    .into_iter()
                    .map(|LabelDef { label }| label.span())
                    .reduce(|s1, s2| (s1.start.min(s2.start))..(s1.end.min(s2.end)))
                    .unwrap(),
            })
        }
    }
}
impl<'s, E> UnlabelledRelativeParam<'s, E> {
    pub fn map_value(
        self,
        f: impl FnOnce(Box<Expression<'s, E>>) -> Box<Expression<'s, E>>,
    ) -> Self {
        Self {
            value: f(self.value),
        }
    }
}
