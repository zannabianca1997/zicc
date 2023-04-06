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
    AppendError, AssembleError, Label,
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
    PUSH(Labelled<ReadParam>),
    PUSHM(ReadParam, usize),
    POP(Labelled<WriteParam>),
    POPM(WriteParam, usize),
    LOAD(Labelled<ReadParam>, Labelled<WriteParam>),
    LOADM(ReadParam, WriteParam, usize),
    STORE(Labelled<ReadParam>, Labelled<ReadParam>),
    STOREM(ReadParam, ReadParam, usize),
    CALL(Labelled<ReadParam>),
    RET,
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
                load a b => mov a
                            mov :0 b
            */
            Directive::LOAD(a, b) => {
                let lbl = Label::anonimous();
                vec![
                    Left(Directive::MOV(
                        a,
                        WriteParam::Position(RlValue::Reference {
                            lbl: lbl.clone(),
                            offset: 0.into(),
                        })
                        .into(),
                    )),
                    Left(Directive::MOV(
                        ReadParam::Position(RlValue::Absolute(ICValue(0))).labelled(lbl),
                        b,
                    )),
                ]
            }
            Directive::LOADM(a, b, len) => {
                let mut code = vec![];
                for i in 0..len {
                    let lbl = Label::anonimous();
                    code.push(Left(Directive::MOV(
                        a.clone().into(),
                        WriteParam::Position(RlValue::Reference {
                            lbl: lbl.clone(),
                            offset: 0.into(),
                        })
                        .into(),
                    )));
                    if i != 0 {
                        code.push(Left(Directive::Instruction(Instruction::ADD(
                            ReadParam::Position(RlValue::Reference {
                                lbl: lbl.clone(),
                                offset: 0.into(),
                            })
                            .into(),
                            ReadParam::Immediate(RlValue::Absolute(ICValue(i as _))).into(),
                            WriteParam::Position(RlValue::Reference {
                                lbl: lbl.clone(),
                                offset: 0.into(),
                            })
                            .into(),
                        ))));
                    }
                    code.push(Left(Directive::MOV(
                        ReadParam::Position(RlValue::Absolute(ICValue(0))).labelled(lbl),
                        b.clone().offset(ICValue(i as _)).into(),
                    )));
                }

                code
            }
            /*
                store a b => mov b
                             mov a $0:0
            */
            Directive::STORE(a, b) => {
                let lbl = Label::anonimous();
                vec![
                    Left(Directive::MOV(
                        b,
                        WriteParam::Position(RlValue::Reference {
                            lbl: lbl.clone(),
                            offset: 0.into(),
                        })
                        .into(),
                    )),
                    Left(Directive::MOV(
                        a,
                        WriteParam::Position(RlValue::Absolute(ICValue(0))).labelled(lbl),
                    )),
                ]
            }
            Directive::STOREM(a, b, len) => {
                let mut code = vec![];
                for i in 0..len {
                    let lbl = Label::anonimous();
                    code.push(Left(Directive::MOV(
                        b.clone().into(),
                        WriteParam::Position(RlValue::Reference {
                            lbl: lbl.clone(),
                            offset: 0.into(),
                        })
                        .into(),
                    )));
                    if i != 0 {
                        code.push(Left(Directive::Instruction(Instruction::ADD(
                            ReadParam::Position(RlValue::Reference {
                                lbl: lbl.clone(),
                                offset: 0.into(),
                            })
                            .into(),
                            ReadParam::Immediate(RlValue::Absolute(ICValue(i as _))).into(),
                            WriteParam::Position(RlValue::Reference {
                                lbl: lbl.clone(),
                                offset: 0.into(),
                            })
                            .into(),
                        ))));
                    }
                    code.push(Left(Directive::MOV(
                        // Either the param with the offset, or for a immediate param just itself
                        Either::from(a.clone().offset(ICValue(i as _)))
                            .into_inner()
                            .into(),
                        WriteParam::Position(RlValue::Absolute(ICValue(0))).labelled(lbl),
                    )));
                }

                code
            }
            /*
                push a [l] => mov a @0 [l]
                              incb #l
            */
            Directive::PUSH(a) => {
                vec![
                    Left(Directive::MOV(a, WriteParam::Relative(ICValue(0)).into())),
                    Left(Directive::Instruction(Instruction::INCB(
                        ReadParam::Immediate(RlValue::Absolute(ICValue(1))).into(),
                    ))),
                ]
            }
            Directive::PUSHM(a, len) => {
                vec![
                    Left(Directive::MOVM(a, WriteParam::Relative(ICValue(0)), len)),
                    Left(Directive::Instruction(Instruction::INCB(
                        ReadParam::Immediate(RlValue::Absolute(ICValue(len as _))).into(),
                    ))),
                ]
            }
            /*
                pop a [l] => incb #-l
                             mov @0 a [l]
            */
            Directive::POP(a) => {
                vec![
                    Left(Directive::Instruction(Instruction::INCB(
                        ReadParam::Immediate(RlValue::Absolute(-ICValue(1))).into(),
                    ))),
                    Left(Directive::MOV(ReadParam::Relative(ICValue(0)).into(), a)),
                ]
            }
            Directive::POPM(a, len) => {
                vec![
                    Left(Directive::Instruction(Instruction::INCB(
                        ReadParam::Immediate(RlValue::Absolute(-ICValue(len as _))).into(),
                    ))),
                    Left(Directive::MOVM(ReadParam::Relative(ICValue(0)), a, len)),
                ]
            }
            Directive::CALL(a) => {
                let lbl = Label::anonimous();
                vec![
                    Left(Directive::PUSH(
                        ReadParam::Immediate(RlValue::Reference {
                            lbl: lbl.clone(),
                            offset: ICValue(0),
                        })
                        .into(),
                    )),
                    Left(Directive::JMP(a)),
                    Left(Directive::Labels(().labelled(lbl))),
                ]
            }
            Directive::RET => {
                vec![
                    Left(Directive::Instruction(Instruction::INCB(
                        ReadParam::Immediate(RlValue::Absolute(-ICValue(1))).into(),
                    ))),
                    Left(Directive::JMP(ReadParam::Relative(ICValue(0)).into())),
                ]
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
            Directive::LOADM(a, b, len) => write!(f, "load {a} {b} {len}"),
            Directive::STORE(a, b) => write!(f, "store {a} {b}"),
            Directive::STOREM(a, b, len) => write!(f, "store {a} {b} {len}"),
            Directive::PUSH(a) => write!(f, "push {a}"),
            Directive::PUSHM(a, len) => write!(f, "push {a} {len}"),
            Directive::POP(a) => write!(f, "pop {a}"),
            Directive::POPM(a, len) => write!(f, "pop {a} {len}"),
            Directive::CALL(a) => write!(f, "call {a}"),
            Directive::RET => write!(f, "ret"),
        }
    }
}
