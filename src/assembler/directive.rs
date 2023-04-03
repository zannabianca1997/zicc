//! Directives
//!
//! Assembly commands that do not translate to single instructions

use std::fmt::Display;

use either::Either::{self, Right};
use thiserror::Error;

use super::{
    instruction::{GenerateInstructionError, Instruction},
    label::Labelled,
    relocatable::{ICProgramFragment, RlValue},
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Directive {
    Instruction(Instruction),
    Labels(Labelled<()>),
    DATA(Vec<Labelled<RlValue>>),
    ZEROS(usize),
}

#[derive(Debug, Error, PartialEq, Eq)]
pub enum ExpandError {
    #[error("Error during instruction generator")]
    GenerateInstruction(
        #[from]
        #[source]
        GenerateInstructionError,
    ),
}

impl Directive {
    /// Expand this directive into code fragment, or other directives
    pub fn expand(self) -> Result<Vec<Either<Self, ICProgramFragment>>, ExpandError> {
        Ok(match self {
            Directive::Instruction(instr) => vec![Right(instr.generate()?)],
            Directive::DATA(_) => todo!(),
            Directive::ZEROS(_) => todo!(),
            Directive::Labels(lbls) => {
                let mut fragment = ICProgramFragment::empty();
                fragment.push_labels(lbls);
                vec![Right(fragment)]
            }
        })
    }
}

impl Display for Directive {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Directive::Instruction(i) => write!(f, "{i}"),
            Directive::Labels(lbls) => {
                for lbl in lbls.lbls.iter() {
                    write!(f, "{lbl}")?
                }
                Ok(())
            }
            Directive::DATA(_) => todo!(),
            Directive::ZEROS(_) => todo!(),
        }
    }
}
