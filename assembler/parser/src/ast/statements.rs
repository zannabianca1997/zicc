use super::*;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize, BorrowDecode)]
/// A list of integers to insert verbatim in the output
/// ```zicc
/// ints 1, 3, 4
/// ints "hello\0"
/// ```
pub struct IntsStm<'s, Error = Infallible> {
    #[serde(borrow)]
    pub values: Vec<Labelled<'s, IntsParam<'s, Error>>>,
}

#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize, Encode, BorrowDecode,
)]
/// A lenght of zeros to insert
/// ```zicc
/// zeros 4
/// ```
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
/// Increase its parameter by 1
/// ```zicc
/// inc 3
/// ```
pub struct IncStm<'s, Error = Infallible>(#[serde(borrow)] pub UnlabelledWriteParam<'s, Error>);
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize, Encode, BorrowDecode,
)]
/// Decrease its parameter by 1
/// ```zicc
/// dec 3
/// ```
pub struct DecStm<'s, Error = Infallible>(#[serde(borrow)] pub UnlabelledWriteParam<'s, Error>);
#[derive(
    Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize, Encode, BorrowDecode,
)]
/// Unconditional jump
/// ```zicc
/// jmp @3
/// ```
pub struct JmpStm<'s, Error = Infallible>(#[serde(borrow)] pub ReadParam<'s, Error>);

#[derive(
    Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize, Encode, BorrowDecode,
)]
/// Move one or multiple memory locations
/// ```zicc
/// mov @3 a:@9
/// mov @3 @9 4 ; cannot label multiple moves
/// ```
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
    Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize, Encode, BorrowDecode,
)]
/// Load a value from a pointed cell
/// ```zicc
/// load @3 a:@9
/// load @3 @9 4 ; cannot label multiple moves
/// ; if the cell contains a relative address use loadr
/// loadr @3 a:@9
/// loadr @3 @9 4 ; cannot label multiple moves
/// ```
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
        l: Box<Expression<'s, Error>>,
    },
}
impl<E> LoadStm<'_, E> {
    pub fn relative(&self) -> &bool {
        let (Self::Single { relative, .. } | Self::Multiple { relative, .. }) = self;
        relative
    }
    pub fn relative_mut(&mut self) -> &mut bool {
        let (Self::Single { relative, .. } | Self::Multiple { relative, .. }) = self;
        relative
    }
}
#[derive(
    Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize, Encode, BorrowDecode,
)]
/// Store a value to a pointed cell
/// ```zicc
/// store @3 a:@9
/// store @3 @9 4 ; cannot label multiple moves
/// ; if the cell contains a relative address use storer
/// storer @3 a:@9
/// storer @3 @9 4 ; cannot label multiple moves
/// ```
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
        l: Box<Expression<'s, Error>>,
    },
}
impl<E> StoreStm<'_, E> {
    pub fn relative(&self) -> &bool {
        let (Self::Single { relative, .. } | Self::Multiple { relative, .. }) = self;
        relative
    }
    pub fn relative_mut(&mut self) -> &mut bool {
        let (Self::Single { relative, .. } | Self::Multiple { relative, .. }) = self;
        relative
    }
}

#[derive(
    Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize, Encode, BorrowDecode,
)]
/// Call a subroutine
/// ```zicc
/// call #sub_routine, 2
/// ```
/// The second parameter is the length of the stack frame
pub struct CallStm<'s, Error = Infallible>(
    #[serde(borrow)] pub ReadParam<'s, Error>,
    #[serde(borrow)] pub Box<Expression<'s, Error>>,
);
/// Return from a subroutine
/// ```zicc
/// ret
/// ```
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
/// Export identifiers to make them linkable
/// ```zicc
/// export foo, bar
/// ...
/// foo:
///     ...
/// ```
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
/// Declare an entry point
/// ```zicc
/// entry main
/// ...
/// main:
///     ...
/// ```
pub struct EntryStm<'s> {
    #[serde(borrow)]
    pub entry: Identifier<'s>,
}
