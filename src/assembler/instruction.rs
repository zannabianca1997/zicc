//! Assembler instructions

use thiserror::Error;

use crate::ICValue;

use super::{label::Labelled, relocatable::RlValue};

/// A param that can be written to
#[derive(Debug, Clone, PartialEq, Eq)]
enum WriteParam {
    Position(RlValue),
    Relative(ICValue),
}
#[derive(Debug, Error)]
#[error("Immediate mode cannot be written to")]
struct ImmediateModeInWriteParam;
impl TryFrom<ReadParam> for WriteParam {
    type Error = ImmediateModeInWriteParam;

    fn try_from(value: ReadParam) -> Result<Self, Self::Error> {
        match value {
            ReadParam::Position(v) => Ok(Self::Position(v)),
            ReadParam::Immediate(_) => Err(ImmediateModeInWriteParam),
            ReadParam::Relative(v) => Ok(Self::Relative(v)),
        }
    }
}

/// A param that can be readed
#[derive(Debug, Clone, PartialEq, Eq)]
enum ReadParam {
    Position(RlValue),
    Immediate(ICValue),
    Relative(ICValue),
}
impl From<WriteParam> for ReadParam {
    fn from(value: WriteParam) -> Self {
        match value {
            WriteParam::Position(v) => Self::Position(v),
            WriteParam::Relative(v) => Self::Relative(v),
        }
    }
}

/// A intcode instruction
#[derive(Debug, Clone, PartialEq, Eq)]
enum Instruction {
    ADD(
        Labelled<ReadParam>,
        Labelled<ReadParam>,
        Labelled<WriteParam>,
    ),
    MUL(
        Labelled<ReadParam>,
        Labelled<ReadParam>,
        Labelled<WriteParam>,
    ),
    IN(Labelled<WriteParam>),
    OUT(Labelled<ReadParam>),
    JZ(Labelled<ReadParam>, Labelled<ReadParam>),
    JNZ(Labelled<ReadParam>, Labelled<ReadParam>),
    SLT(
        Labelled<ReadParam>,
        Labelled<ReadParam>,
        Labelled<WriteParam>,
    ),
    SEQ(
        Labelled<ReadParam>,
        Labelled<ReadParam>,
        Labelled<WriteParam>,
    ),
    INCB(Labelled<ReadParam>),
    HALT,
}
