//! Components of an assembly file

use std::fmt::Display;

use either::Either;

use super::{directive::Directive, instruction::Instruction, label::Labelled};

pub type Line = Labelled<Option<Either<Directive, Instruction>>>;

#[derive(Debug, Clone)]
pub struct RawAssemblyFile(pub(super) Vec<Line>);

impl IntoIterator for RawAssemblyFile {
    type Item = Line;

    type IntoIter = <Vec<Line> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}
impl<'a> IntoIterator for &'a RawAssemblyFile {
    type Item = &'a Line;

    type IntoIter = <&'a Vec<Line> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        (&self.0).into_iter()
    }
}

impl Display for RawAssemblyFile {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for line in self {
            let Labelled { inner, lbls } = line;
            for lbl in lbls {
                write!(f, "{lbl}: ")?
            }
            match inner {
                Some(v) => writeln!(f, "{v}")?,
                None => (),
            }
        }
        Ok(())
    }
}
