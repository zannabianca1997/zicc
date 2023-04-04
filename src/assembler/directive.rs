//! Directives
//!
//! Assembly commands that do not translate to single instructions

use std::fmt::Display;

use either::Either::{self, Left, Right};
use thiserror::Error;

use crate::{assembler::label::Labellable, intcode::ICValue};

use super::{
    instruction::{GenerateInstructionError, Instruction, ReadParam, WriteParam},
    label::Labelled,
    relocatable::{ICProgramFragment, RlValue},
    AppendError, AssembleError, AssemblyFile, Label,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Directive {
    Instruction(Instruction),
    Labels(Labelled<()>),
    DATA(Vec<Labelled<RlValue>>),
    ZEROS(usize),
    JMP(Labelled<ReadParam>),
    MOV(Labelled<ReadParam>, Labelled<WriteParam>),
    MOVM(ReadParam, WriteParam, usize),
    LOAD(Labelled<ReadParam>, Labelled<WriteParam>),
    STORE(Labelled<ReadParam>, Labelled<ReadParam>),
}

// const s: usize = size_of::<Directive>();

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
    #[error("Error while sub-assembling for instruction {0}")]
    SubAssemble(&'static str, #[source] Box<AssembleError>),
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
            Directive::MOV(a, b) => {
                vec![Left(Directive::Instruction(Instruction::ADD(
                    a,
                    ReadParam::Immediate(ICValue(0).into()).into(),
                    b,
                )))]
            }
            Directive::MOVM(a, b, len) => {
                let mut res = Vec::with_capacity(len);
                for i in 0..len {
                    res.push(Left(Directive::MOV(
                        Either::from(a.clone().offset(ICValue(i as _)))
                            .into_inner()
                            .into(),
                        b.clone().offset(ICValue(i as _)).into(),
                    )));
                }
                res
            }
            /*
                load a b => mov a $0
                            mov $0:0 b
            */
            Directive::LOAD(a, b) => {
                // find a label unused by both
                let lbl = Label::unused(a.lbls.iter().chain(b.lbls.iter()));
                let code = AssemblyFile(vec![
                    Some(Directive::MOV(
                        a,
                        WriteParam::Position(RlValue::Reference {
                            lbl: lbl.clone(),
                            offset: 0.into(),
                        })
                        .into(),
                    ))
                    .into(),
                    Some(Directive::MOV(
                        ReadParam::Position(RlValue::Absolute(ICValue(0))).labelled(lbl),
                        b,
                    ))
                    .into(),
                ])
                .assemble()
                .map_err(|err| ExpandError::SubAssemble("load", Box::new(err)))?;
                vec![Right(code)]
            }
            /*
                store a b => mov b $0
                             mov a $0:0
            */
            Directive::STORE(a, b) => {
                // find a label unused by both
                let lbl = Label::unused(a.lbls.iter().chain(b.lbls.iter()));
                let code = AssemblyFile(vec![
                    Some(Directive::MOV(
                        b,
                        WriteParam::Position(RlValue::Reference {
                            lbl: lbl.clone(),
                            offset: 0.into(),
                        })
                        .into(),
                    ))
                    .into(),
                    Some(Directive::MOV(
                        a,
                        WriteParam::Position(RlValue::Absolute(ICValue(0))).labelled(lbl),
                    ))
                    .into(),
                ])
                .assemble()
                .map_err(|err| ExpandError::SubAssemble("load", Box::new(err)))?;
                vec![Right(code)]
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
            Directive::MOV(a, b) => write!(f, "mov {a} {b}"),
            Directive::MOVM(a, b, len) => write!(f, "mov {a} {b} {len}"),
            Directive::LOAD(a, b) => write!(f, "load {a} {b}"),
            Directive::STORE(a, b) => write!(f, "store {a} {b}"),
        }
    }
}
