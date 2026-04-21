#![doc = include_str!("../README.md")]

use num_derive::{FromPrimitive, ToPrimitive};
use num_traits::FromPrimitive;
use snafu::{OptionExt, Snafu};

/// An IntCode instruction
///
/// `ReadParam` is used for params that are only read by the instruction,
/// `WriteParam` instead params that are only written to.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Instruction<ReadParam, WriteParam> {
    Add(
        (ReadParamMode, ReadParam),
        (ReadParamMode, ReadParam),
        (WriteParamMode, WriteParam),
    ),
    Mul(
        (ReadParamMode, ReadParam),
        (ReadParamMode, ReadParam),
        (WriteParamMode, WriteParam),
    ),
    Inp((WriteParamMode, WriteParam)),
    Out((ReadParamMode, ReadParam)),
    Jnz((ReadParamMode, ReadParam), (ReadParamMode, ReadParam)),
    Jez((ReadParamMode, ReadParam), (ReadParamMode, ReadParam)),
    Slt(
        (ReadParamMode, ReadParam),
        (ReadParamMode, ReadParam),
        (WriteParamMode, WriteParam),
    ),
    Seq(
        (ReadParamMode, ReadParam),
        (ReadParamMode, ReadParam),
        (WriteParamMode, WriteParam),
    ),
    Inb((ReadParamMode, ReadParam)),
    Hlt,
}

/// An IntCode opcode
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, FromPrimitive, ToPrimitive)]
#[repr(u8)]
pub enum OpCode {
    ADD = 01,
    MUL = 02,
    INP = 03,
    OUT = 04,
    JNZ = 05,
    JEZ = 06,
    SLT = 07,
    SEQ = 08,
    INB = 09,
    HLT = 99,
}

impl OpCode {
    pub fn param_count(&self) -> usize {
        use OpCode::*;

        match self {
            ADD | MUL | SLT | SEQ => 3,
            JNZ | JEZ => 2,
            INP | OUT | INB => 1,
            HLT => 0,
        }
    }

    pub fn to_u8(&self) -> u8 {
        *self as u8
    }

    pub fn from_u8(code: u8) -> Result<Self, InvalidOpCode> {
        FromPrimitive::from_u8(code).context(InvalidOpCodeSnafu { code })
    }
}

impl<R, W> Instruction<R, W> {
    /// Get the opcode for this instruction
    pub fn opcode(&self) -> OpCode {
        use Instruction::*;
        use OpCode::*;

        match self {
            Add(_, _, _) => ADD,
            Mul(_, _, _) => MUL,
            Inp(_) => INP,
            Out(_) => OUT,
            Jnz(_, _) => JNZ,
            Jez(_, _) => JEZ,
            Slt(_, _, _) => SLT,
            Seq(_, _, _) => SEQ,
            Inb(_) => INB,
            Hlt => HLT,
        }
    }
}

impl Instruction<(), ()> {
    /// Decode an instruction code
    ///
    /// Decode an instruction code into its parts: opcode and params modes.
    pub fn decode(code: u16) -> Result<Self, InvalidCode> {
        use Instruction::*;
        use OpCode::*;

        let opcode = OpCode::from_u8((code % 100) as _)?;

        if code >= 100 * 10u16.pow(opcode.param_count() as _) {
            return Err(InvalidCode::AdditionalDigits { code });
        }

        let a = ((code / 100) % 10) as u8;
        let b = ((code / 1000) % 10) as u8;
        let c = ((code / 10000) % 10) as u8;

        Ok(match opcode {
            ADD => Add(
                (ReadParamMode::from_u8(a)?, ()),
                (ReadParamMode::from_u8(b)?, ()),
                (ReadParamMode::from_u8(c)?.try_into()?, ()),
            ),
            MUL => Mul(
                (ReadParamMode::from_u8(a)?, ()),
                (ReadParamMode::from_u8(b)?, ()),
                (ReadParamMode::from_u8(c)?.try_into()?, ()),
            ),
            INP => Inp((ReadParamMode::from_u8(a)?.try_into()?, ())),
            OUT => Out((ReadParamMode::from_u8(a)?, ())),
            JNZ => Jnz(
                (ReadParamMode::from_u8(a)?, ()),
                (ReadParamMode::from_u8(b)?, ()),
            ),
            JEZ => Jez(
                (ReadParamMode::from_u8(a)?, ()),
                (ReadParamMode::from_u8(b)?, ()),
            ),
            SLT => Slt(
                (ReadParamMode::from_u8(a)?, ()),
                (ReadParamMode::from_u8(b)?, ()),
                (ReadParamMode::from_u8(c)?.try_into()?, ()),
            ),
            SEQ => Seq(
                (ReadParamMode::from_u8(a)?, ()),
                (ReadParamMode::from_u8(b)?, ()),
                (ReadParamMode::from_u8(c)?.try_into()?, ()),
            ),
            INB => Inb((ReadParamMode::from_u8(a)?, ())),
            HLT => Hlt,
        })
    }
}

impl<R, W> From<Instruction<R, W>> for OpCode {
    fn from(value: Instruction<R, W>) -> Self {
        value.opcode()
    }
}

impl<R, W> From<&Instruction<R, W>> for OpCode {
    fn from(value: &Instruction<R, W>) -> Self {
        value.opcode()
    }
}

/// Mode of a param you can write to
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default, FromPrimitive, ToPrimitive,
)]
#[repr(u8)]
pub enum ReadParamMode {
    #[default]
    Absolute = 0,
    Immediate = 1,
    Relative = 2,
}

impl From<WriteParamMode> for ReadParamMode {
    fn from(value: WriteParamMode) -> Self {
        match value {
            WriteParamMode::Absolute => Self::Absolute,
            WriteParamMode::Relative => Self::Relative,
        }
    }
}

/// Mode of a param you can read from
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default, FromPrimitive, ToPrimitive,
)]
#[repr(u8)]
pub enum WriteParamMode {
    #[default]
    Absolute = 0,
    Relative = 2,
}

impl TryFrom<ReadParamMode> for WriteParamMode {
    type Error = ImmediateModeOnWriteParam;

    fn try_from(value: ReadParamMode) -> Result<Self, Self::Error> {
        match value {
            ReadParamMode::Absolute => Ok(Self::Absolute),
            ReadParamMode::Immediate => Err(ImmediateModeOnWriteParam),
            ReadParamMode::Relative => Ok(Self::Relative),
        }
    }
}

impl ReadParamMode {
    pub fn to_u8(self) -> u8 {
        self as u8
    }

    pub fn from_u8(code: u8) -> Result<Self, InvalidParamModeCode> {
        FromPrimitive::from_u8(code).context(InvalidParamModeCodeSnafu { code })
    }
}

impl WriteParamMode {
    pub fn to_u8(self) -> u8 {
        self as u8
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default, Snafu)]
#[snafu(display("Immediate mode `#` is invalid on writable params"))]
pub struct ImmediateModeOnWriteParam;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Snafu)]
#[snafu(display("{code} is not a valid opcode"))]
pub struct InvalidOpCode {
    code: u8,
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Snafu)]
#[snafu(display("{code} is not a valid param mode code"))]
pub struct InvalidParamModeCode {
    code: u8,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Snafu)]
pub enum InvalidCode {
    #[snafu(display("{code} has additional digits over the needed params"))]
    AdditionalDigits { code: u16 },
    #[snafu(transparent)]
    InvalidOpcode { source: InvalidOpCode },
    #[snafu(transparent)]
    InvalidParamModeCode { source: InvalidParamModeCode },
    #[snafu(transparent)]
    ImmediateModeOnWriteParam { source: ImmediateModeOnWriteParam },
}
