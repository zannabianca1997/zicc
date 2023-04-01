//! Assembler instructions

use arrayvec::ArrayVec;
use either::Either::{self, Left, Right};
use enum_primitive_derive::Primitive;
use strum_macros::{
    Display, EnumDiscriminants, EnumMessage, EnumString, EnumVariantNames, ToString,
};
use thiserror::Error;

use crate::ICValue;

use super::{label::Labelled, relocatable::RlValue};

/// A param that can be written to
#[derive(Debug, Clone, PartialEq, Eq, EnumDiscriminants)]
#[strum_discriminants(name(WriteMode))]
pub enum WriteParam {
    Position(RlValue),
    Relative(ICValue),
}
#[derive(Debug, Error)]
#[error("Unrecognized param mode {0}")]
pub struct UnrecognizedModeError(u8);

impl TryFrom<u8> for WriteMode {
    type Error = Either<UnrecognizedModeError, ImmediateModeInWriteParam>;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(Self::Position),
            1 => Err(Right(ImmediateModeInWriteParam)),
            2 => Ok(Self::Relative),
            _ => Err(Left(UnrecognizedModeError(value))),
        }
    }
}
impl TryFrom<ReadMode> for WriteMode {
    type Error = ImmediateModeInWriteParam;

    fn try_from(value: ReadMode) -> Result<Self, Self::Error> {
        match value {
            ReadMode::Position => Ok(Self::Position),
            ReadMode::Immediate => Err(ImmediateModeInWriteParam),
            ReadMode::Relative => Ok(Self::Relative),
        }
    }
}

#[derive(Debug, Error)]
#[error("Immediate mode cannot be written to")]
pub struct ImmediateModeInWriteParam;
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
#[derive(Debug, Clone, PartialEq, Eq, EnumDiscriminants)]
#[strum_discriminants(name(ReadMode))]
pub enum ReadParam {
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

impl TryFrom<u8> for ReadMode {
    type Error = Either<UnrecognizedModeError, ImmediateModeInWriteParam>;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(Self::Position),
            1 => Ok(Self::Immediate),
            2 => Ok(Self::Relative),
            _ => Err(Left(UnrecognizedModeError(value))),
        }
    }
}
impl From<WriteMode> for ReadMode {
    fn from(value: WriteMode) -> Self {
        match value {
            WriteMode::Position => Self::Position,
            WriteMode::Relative => Self::Relative,
        }
    }
}

/// A intcode instruction
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Instruction {
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

/// The first value of an instruction
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InstructionHeader {
    ADD(ReadMode, ReadMode, WriteMode),
    MUL(ReadMode, ReadMode, WriteMode),
    IN(WriteMode),
    OUT(ReadMode),
    JZ(ReadMode, ReadMode),
    JNZ(ReadMode, ReadMode),
    SLT(ReadMode, ReadMode, WriteMode),
    SEQ(ReadMode, ReadMode, WriteMode),
    INCB(ReadMode),
    HALT,
}

#[derive(Debug, Error)]
pub enum InvalidInstructionHeader {
    #[error(transparent)]
    UnrecognizedOpCode(#[from] UnrecognizedOpCode),
    #[error(transparent)]
    UnrecognizedMode(#[from] UnrecognizedModeError),
    #[error(transparent)]
    ImmediateModeInWriteParam(#[from] ImmediateModeInWriteParam),
    #[error("Too many param mode for opcode {0}: expected {1}")]
    TooManyModes(OpCode, u8),
    #[error("Negative instruction header")]
    NegativeInstructionHeader,
}
impl From<<ReadMode as TryFrom<u8>>::Error> for InvalidInstructionHeader {
    fn from(value: <ReadMode as TryFrom<u8>>::Error) -> Self {
        match value {
            Left(err) => err.into(),
            Right(err) => err.into(),
        }
    }
}

impl TryFrom<ICValue> for InstructionHeader {
    type Error = InvalidInstructionHeader;

    fn try_from(value: ICValue) -> Result<Self, Self::Error> {
        use InvalidInstructionHeader::*;
        let (opcode, modes) = {
            if value.0 < 0 {
                return Err(NegativeInstructionHeader);
            }
            let mut value = value.0 as u64;
            let opcode = OpCode::try_from((value % 100) as u8)?;
            value /= 100;
            let mut modes = [ReadMode::Position; 3];
            for i in 0..opcode.param_num() {
                modes[i] = ReadMode::try_from((value % 10) as u8)?;
                value /= 10;
            }
            if value > 0 {
                return Err(TooManyModes(opcode, opcode.param_num() as u8));
            }
            (opcode, modes)
        };
        use OpCode::*;
        Ok(match (opcode, modes) {
            (ADD, [a, b, c, ..]) => Self::ADD(a, b, c.try_into()?),
            (MUL, [a, b, c, ..]) => Self::MUL(a, b, c.try_into()?),
            (IN, [a, ..]) => Self::IN(a.try_into()?),
            (OUT, [a, ..]) => Self::OUT(a),
            (JZ, [a, b, ..]) => Self::JZ(a, b),
            (JNZ, [a, b, ..]) => Self::JNZ(a, b),
            (SLT, [a, b, c, ..]) => Self::SLT(a, b, c.try_into()?),
            (SEQ, [a, b, c, ..]) => Self::SEQ(a, b, c.try_into()?),
            (INCB, [a, ..]) => Self::INCB(a),
            (HALT, [..]) => Self::HALT,
        })
    }
}

/// A intcode opcode
#[derive(Debug, Clone, Copy, PartialEq, Eq, Display)]
pub enum OpCode {
    ADD = 1,
    MUL = 2,
    IN = 3,
    OUT = 4,
    JZ = 5,
    JNZ = 6,
    SLT = 7,
    SEQ = 8,
    INCB = 9,
    HALT = 99,
}
impl OpCode {
    const fn param_num(&self) -> usize {
        match self {
            OpCode::ADD | OpCode::MUL | OpCode::SLT | OpCode::SEQ => 3,
            OpCode::JZ | OpCode::JNZ => 2,
            OpCode::IN | OpCode::OUT | OpCode::INCB => 1,
            OpCode::HALT => 0,
        }
    }
}

#[derive(Debug, Error)]
#[error("Unrecognized opcode {0}")]
pub struct UnrecognizedOpCode(u8);

impl TryFrom<u8> for OpCode {
    type Error = UnrecognizedOpCode;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        use OpCode::*;
        match value {
            1 => Ok(ADD),
            2 => Ok(MUL),
            3 => Ok(IN),
            4 => Ok(OUT),
            5 => Ok(JZ),
            6 => Ok(JNZ),
            7 => Ok(SLT),
            8 => Ok(SEQ),
            9 => Ok(INCB),
            99 => Ok(HALT),
            _ => Err(UnrecognizedOpCode(value)),
        }
    }
}
