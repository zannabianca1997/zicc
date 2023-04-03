//! Components of an assembly file

use std::fmt::Display;

use either::Either::{Left, Right};
use thiserror::Error;

use super::{
    directive::{Directive, ExpandError},
    label::Labelled,
    relocatable::{AppendError, ICProgramFragment},
};

pub type Line = Labelled<Option<Directive>>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AssemblyFile(pub(super) Vec<Line>);

#[derive(Debug, Error, PartialEq, Eq)]
pub enum AssembleError {
    #[error(transparent)]
    Expand(#[from] ExpandError),
    #[error(transparent)]
    Append(#[from] AppendError),
}

impl AssemblyFile {
    /// Assemble the file
    pub fn assemble(self) -> Result<ICProgramFragment, AssembleError> {
        // Stack of stuff to assemble, in reverse order
        let mut stack = vec![];
        for line in self.into_iter().rev() {
            let (line, labels) = line.split();
            if let Some(line) = line {
                stack.push(Left(line));
            }
            if labels.is_labelled() {
                stack.push(Left(Directive::Labels(labels)))
            }
        }

        let mut prog = ICProgramFragment::empty();

        while let Some(directive) = stack.pop() {
            match directive {
                Left(directive) => {
                    let expanded = directive.expand()?;
                    stack.extend(expanded.into_iter().rev())
                }
                Right(fragment) => prog = prog.join(fragment)?,
            }
        }

        Ok(prog)
    }
}

impl IntoIterator for AssemblyFile {
    type Item = Line;

    type IntoIter = <Vec<Line> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}
impl<'a> IntoIterator for &'a AssemblyFile {
    type Item = &'a Line;

    type IntoIter = <&'a Vec<Line> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        (&self.0).into_iter()
    }
}

impl Display for AssemblyFile {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for line in self {
            let Labelled { inner, lbls } = line;
            for lbl in lbls {
                write!(f, "{lbl}: ")?
            }
            match inner {
                Some(v) => writeln!(f, "{v}"),
                None => writeln!(f),
            }?
        }
        Ok(())
    }
}
