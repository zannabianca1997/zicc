//! Components of an assembly file

use either::Either;

use super::{directive::Directive, instruction::Instruction, label::Labelled};

pub type Line = Labelled<Option<Either<Directive, Instruction>>>;
pub struct RawAssemblyFile(pub(super) Vec<Line>);
