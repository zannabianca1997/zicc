//! Directives
//!
//! Assembly commands that do not translate to single instructions

use std::fmt::Display;

use either::Either::{self, Left, Right};
use thiserror::Error;

use crate::intcode::ICValue;

use super::{
    instruction::{GenerateInstructionError, Instruction, ReadParam},
    label::Labelled,
    relocatable::{ICProgramFragment, RlValue},
    AppendError,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Directive {
    Instruction(Instruction),
    Labels(Labelled<()>),
    DATA(Vec<Labelled<RlValue>>),
    ZEROS(usize),
    JMP(Labelled<ReadParam>),
}

#[derive(Debug, Error, PartialEq, Eq)]
pub enum ExpandError {
    #[error("Error during instruction generator")]
    GenerateInstruction(
        #[from]
        #[source]
        GenerateInstructionError,
    ),
    #[error("Error while joining multiple ")]
    Joining(
        #[from]
        #[source]
        AppendError,
    ),
}

impl Directive {
    /// Expand this directive into code fragment, or other directives
    pub fn expand(self) -> Result<Vec<Either<Self, ICProgramFragment>>, ExpandError> {
        Ok(match self {
            Directive::Instruction(instr) => vec![Right(instr.generate()?)],
            Directive::DATA(values) => vec![Right(values.into_iter().try_fold(
                ICProgramFragment::empty(),
                |mut fragment, value| -> Result<ICProgramFragment, AppendError> {
                    fragment.push(value)?;
                    Ok(fragment)
                },
            )?)],
            Directive::ZEROS(n) => vec![Right(ICProgramFragment::zeros(n))],
            Directive::Labels(lbls) => {
                let mut fragment = ICProgramFragment::empty();
                fragment
                    .push_labels(lbls)
                    .expect("Adding labels to a empty fragmens should not panic");
                vec![Right(fragment)]
            }
            // jmp a => jz #0 a
            Directive::JMP(dest) => {
                vec![Left(Directive::Instruction(Instruction::JZ(
                    ReadParam::Immediate(ICValue(0).into()).into(),
                    dest,
                )))]
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
                    write!(f, "{lbl}:")?
                }
                Ok(())
            }
            Directive::DATA(values) => {
                write!(f, "data")?;
                for value in values {
                    write!(f, " {value}")?
                }
                Ok(())
            }
            Directive::ZEROS(n) => write!(f, "zeros {n}"),
            Directive::JMP(a) => write!(f, "jmp {a}"),
        }
    }
}
