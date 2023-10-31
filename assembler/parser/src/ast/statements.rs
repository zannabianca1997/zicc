use super::*;

#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize, BorrowDecode,
)]
pub struct IntsStm<'s, Error = Infallible> {
    #[serde(borrow)]
    pub values: Vec<Labelled<'s, IntsParam<'s, Error>>>,
}

#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize, Encode, BorrowDecode,
)]
pub struct ZerosStm<'s, Error = Infallible>(#[serde(borrow)] pub Box<Expression<'s, Error>>);

impl<'s, Error> ::bincode::Encode for IntsStm<'s, Error>
where
    Error: ::bincode::Encode,
{
    fn encode<__E: ::bincode::enc::Encoder>(
        &self,
        encoder: &mut __E,
    ) -> core::result::Result<(), ::bincode::error::EncodeError> {
        ::utils::encode_vec(&self.values, encoder)?;
        Ok(())
    }
}

#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize, Encode, BorrowDecode,
)]
pub enum IntsParam<'s, Error = Infallible> {
    Int(#[serde(borrow)] Box<Expression<'s, Error>>),
    Str(#[serde(borrow)] StringLit<'s>),
}

#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize, Encode, BorrowDecode,
)]
pub struct IncStm<'s, Error = Infallible>(#[serde(borrow)] pub UnlabelledWriteParam<'s, Error>);
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize, Encode, BorrowDecode,
)]
pub struct DecStm<'s, Error = Infallible>(#[serde(borrow)] pub UnlabelledWriteParam<'s, Error>);
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize, Encode, BorrowDecode,
)]
pub struct JmpStm<'s, Error = Infallible>(#[serde(borrow)] pub ReadParam<'s, Error>);

#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize, Encode, BorrowDecode,
)]
pub enum MovStm<'s, Error = Infallible> {
    /// Moves a single memory cell
    Single(
        #[serde(borrow)] ReadParam<'s, Error>,
        #[serde(borrow)] WriteParam<'s, Error>,
    ),
    /// Moves multiple consecutive memory cells
    Multiple(
        #[serde(borrow)] UnlabelledNonImmediateReadParam<'s, Error>,
        #[serde(borrow)] UnlabelledWriteParam<'s, Error>,
        #[serde(borrow)] Box<Expression<'s, Error>>,
    ),
}
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize, Encode, BorrowDecode,
)]
pub enum LoadStm<'s, Error = Infallible> {
    Single {
        relative: bool,
        #[serde(borrow)]
        ptr: ReadParam<'s, Error>,
        #[serde(borrow)]
        to: WriteParam<'s, Error>,
    },
    Multiple {
        relative: bool,
        #[serde(borrow)]
        ptr: UnlabelledReadParam<'s, Error>,
        #[serde(borrow)]
        to: UnlabelledWriteParam<'s, Error>,
        #[serde(borrow)]
        n: Box<Expression<'s, Error>>,
    },
}
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize, Encode, BorrowDecode,
)]
pub enum StoreStm<'s, Error = Infallible> {
    Single {
        relative: bool,
        #[serde(borrow)]
        from: ReadParam<'s, Error>,
        #[serde(borrow)]
        ptr: ReadParam<'s, Error>,
    },
    Multiple {
        relative: bool,
        #[serde(borrow)]
        from: UnlabelledReadParam<'s, Error>,
        #[serde(borrow)]
        ptr: UnlabelledReadParam<'s, Error>,
        #[serde(borrow)]
        n: Box<Expression<'s, Error>>,
    },
}

#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize, Encode, BorrowDecode,
)]
pub struct CallStm<'s, Error = Infallible>(
    #[serde(borrow)] pub ReadParam<'s, Error>,
    #[serde(borrow)] pub Box<Expression<'s, Error>>,
);
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    Encode,
    BorrowDecode,
)]
pub struct RetStm;

#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize, Encode, BorrowDecode,
)]
pub struct ExportStm<'s> {
    #[serde(borrow)]
    pub exported: BTreeSet<Identifier<'s>>,
}

#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    Encode,
    BorrowDecode,
)]
pub struct EntryStm<'s> {
    #[serde(borrow)]
    pub entry: Identifier<'s>,
}
