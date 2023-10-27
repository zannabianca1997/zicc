use super::*;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct IntsStm<'s, Error = !> {
    pub values: Vec<Labelled<'s, Either<Box<Expression<'s, Error>>, StringLit<'s>>>>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct IncStm<'s, Error = !>(pub UnlabelledWriteParam<'s, Error>);
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct DecStm<'s, Error = !>(pub UnlabelledWriteParam<'s, Error>);
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct JmpStm<'s, Error = !>(pub ReadParam<'s, Error>);
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ZerosStm<'s, Error = !>(pub Box<Expression<'s, Error>>);

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum MovStm<'s, Error = !> {
    /// Moves a single memory cell
    Single(ReadParam<'s, Error>, WriteParam<'s, Error>),
    /// Moves multiple consecutive memory cells
    Multiple(
        UnlabelledNonImmediateReadParam<'s, Error>,
        UnlabelledWriteParam<'s, Error>,
        Box<Expression<'s, Error>>,
    ),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum PushStm<'s, Error = !> {
    /// Push a single memory cell
    Single(ReadParam<'s, Error>),
    /// Pushes multiple consecutive memory cells
    Multiple(
        UnlabelledNonImmediateReadParam<'s, Error>,
        Box<Expression<'s, Error>>,
    ),
}
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum PopStm<'s, Error = !> {
    /// Pop a single memory cell
    Single(WriteParam<'s, Error>),
    /// Pop multiple consecutive memory cells
    Multiple(UnlabelledWriteParam<'s, Error>, Box<Expression<'s, Error>>),
}
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct CallStm<'s, Error = !>(pub ReadParam<'s, Error>);
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RetStm;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ExportStm<'s> {
    pub exported: Vec<Identifier<'s>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct EntryStm<'s> {
    pub entry: Identifier<'s>,
}
