//! Assembler instructions

use std::{collections::HashSet, fmt::Display, iter::once, mem};

use arrayvec::ArrayVec;
use either::Either::{self, Left, Right};
use strum_macros::{Display, EnumDiscriminants};
use thiserror::Error;

use crate::intcode::ICValue;

use super::{
    label::Labelled,
    relocatable::{AppendError, ICProgramFragment, RlValue},
};

/// A param that can be written to
#[derive(Debug, Clone, PartialEq, Eq, EnumDiscriminants)]
#[strum_discriminants(name(WriteMode))]
pub enum WriteParam {
    Position(RlValue),
    Relative(ICValue),
}
impl WriteParam {
    fn mode(&self) -> WriteMode {
        match self {
            WriteParam::Position(_) => WriteMode::Position,
            WriteParam::Relative(_) => WriteMode::Relative,
        }
    }
}
impl Display for WriteParam {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            WriteParam::Position(v) => write!(f, "{v}"),
            WriteParam::Relative(v) => write!(f, "@{v}"),
        }
    }
}
impl TryFrom<(WriteMode, RlValue)> for WriteParam {
    type Error = ReferenceInParamError;

    fn try_from(value: (WriteMode, RlValue)) -> Result<Self, Self::Error> {
        match value {
            (WriteMode::Position, v) => Ok(Self::Position(v)),
            (WriteMode::Relative, RlValue::Absolute(v)) => Ok(Self::Relative(v)),
            (WriteMode::Relative, RlValue::Reference { .. }) => {
                Err(ReferenceInParamError::Relative)
            }
        }
    }
}
impl From<WriteParam> for RlValue {
    fn from(value: WriteParam) -> Self {
        match value {
            WriteParam::Position(v) => v,
            WriteParam::Relative(v) => RlValue::Absolute(v),
        }
    }
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
impl From<WriteMode> for ICValue {
    fn from(value: WriteMode) -> Self {
        match value {
            WriteMode::Position => ICValue(0),
            WriteMode::Relative => ICValue(2),
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
    Immediate(RlValue),
    Relative(ICValue),
}
impl ReadParam {
    fn mode(&self) -> ReadMode {
        match self {
            ReadParam::Position(_) => ReadMode::Position,
            ReadParam::Immediate(_) => ReadMode::Immediate,
            ReadParam::Relative(_) => ReadMode::Relative,
        }
    }
}
impl Display for ReadParam {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ReadParam::Position(v) => write!(f, "{v}"),
            ReadParam::Immediate(v) => write!(f, "#{v}"),
            ReadParam::Relative(v) => write!(f, "@{v}"),
        }
    }
}
impl From<WriteParam> for ReadParam {
    fn from(value: WriteParam) -> Self {
        match value {
            WriteParam::Position(v) => Self::Position(v),
            WriteParam::Relative(v) => Self::Relative(v),
        }
    }
}

#[derive(Debug, Error)]
pub enum ReferenceInParamError {
    #[error("Absolute references cannot be in immediate params")]
    Immediate,
    #[error("Absolute references cannot be in relative params")]
    Relative,
}

impl TryFrom<(ReadMode, RlValue)> for ReadParam {
    type Error = ReferenceInParamError;

    fn try_from(value: (ReadMode, RlValue)) -> Result<Self, Self::Error> {
        match value {
            (ReadMode::Position, v) => Ok(Self::Position(v)),
            (ReadMode::Immediate, RlValue::Absolute(v)) => Ok(Self::Relative(v)),
            (ReadMode::Immediate, RlValue::Reference { .. }) => {
                Err(ReferenceInParamError::Immediate)
            }
            (ReadMode::Relative, RlValue::Absolute(v)) => Ok(Self::Relative(v)),
            (ReadMode::Relative, RlValue::Reference { .. }) => Err(ReferenceInParamError::Relative),
        }
    }
}
impl From<ReadParam> for RlValue {
    fn from(value: ReadParam) -> Self {
        match value {
            ReadParam::Position(v) | ReadParam::Immediate(v) => v,
            ReadParam::Relative(v) => RlValue::Absolute(v),
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
impl From<ReadMode> for ICValue {
    fn from(value: ReadMode) -> Self {
        match value {
            ReadMode::Position => ICValue(0),
            ReadMode::Immediate => ICValue(1),
            ReadMode::Relative => ICValue(2),
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

#[derive(Debug, Error)]
pub enum TakeInstructionError {
    #[error(transparent)]
    InvalidInstructionHeader(#[from] InvalidInstructionHeader),
    #[error(transparent)]
    ReferenceInParam(#[from] ReferenceInParamError),
    #[error("Instruction was cut short")]
    CutShort,
}

#[derive(Debug, Error, PartialEq, Eq)]
#[error("Error in building instruction")]
pub struct GenerateInstructionError(
    #[from]
    #[source]
    pub AppendError,
);

impl Instruction {
    /// Build an instruction from assemblable code
    pub fn from_assembly(
        iter: &mut impl Iterator<Item = Labelled<RlValue>>,
    ) -> Result<Labelled<Self>, TakeInstructionError> {
        let Labelled {
            inner: header,
            lbls,
        } = iter.next().ok_or(TakeInstructionError::CutShort)?;
        let header = InstructionHeader::try_from(header)?;
        let mut args: ArrayVec<_, 3> = iter.take(header.param_num()).collect();
        if args.len() < header.param_num() {
            return Err(TakeInstructionError::CutShort);
        }
        Ok(Labelled {
            inner: header.build_instruction(args.as_mut())?,
            lbls,
        })
    }

    /// Generate assemblable code
    pub fn generate(self) -> Result<ICProgramFragment, GenerateInstructionError> {
        let header: ICValue = InstructionHeader::from(&self).into();
        let mut params = ArrayVec::<Labelled<RlValue>, 3>::new();
        use Instruction::*;
        match self {
            ADD(a, b, c) | MUL(a, b, c) | SLT(a, b, c) | SEQ(a, b, c) => {
                params.push(a.map(Into::into));
                params.push(b.map(Into::into));
                params.push(c.map(Into::into));
            }
            JZ(a, b) | JNZ(a, b) => {
                params.push(a.map(Into::into));
                params.push(b.map(Into::into));
            }
            INCB(a) | OUT(a) => params.push(a.map(Into::into)),
            IN(a) => params.push(a.map(Into::into)),
            HALT => (),
        }

        once(Labelled {
            inner: RlValue::Absolute(header),
            lbls: HashSet::new(),
        })
        .chain(params.into_iter())
        .map(ICProgramFragment::from)
        .collect::<Result<_, _>>()
        .map_err(Into::into)
    }
}
impl Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match OpCode::from(self) {
            OpCode::ADD => write!(f, "add"),
            OpCode::MUL => write!(f, "mul"),
            OpCode::IN => write!(f, "in"),
            OpCode::OUT => write!(f, "out"),
            OpCode::JZ => write!(f, "jz"),
            OpCode::JNZ => write!(f, "jnz"),
            OpCode::SLT => write!(f, "slt"),
            OpCode::SEQ => write!(f, "seq"),
            OpCode::INCB => write!(f, "incb"),
            OpCode::HALT => write!(f, "halt"),
        }?;
        match self {
            Instruction::ADD(a, b, c)
            | Instruction::MUL(a, b, c)
            | Instruction::SLT(a, b, c)
            | Instruction::SEQ(a, b, c) => {
                write!(f, " {a} {b} {c}")
            }
            Instruction::JZ(a, b) | Instruction::JNZ(a, b) => write!(f, " {a} {b}"),
            Instruction::OUT(a) | Instruction::INCB(a) => write!(f, " {a}"),
            Instruction::IN(a) => write!(f, " {a}"),
            Instruction::HALT => Ok(()),
        }
    }
}

/// The first value of an instruction
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum InstructionHeader {
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
impl InstructionHeader {
    /// Number of params needed
    fn param_num(&self) -> usize {
        OpCode::from(self).param_num()
    }

    /// Buil the instruction from the header.
    ///
    /// # Panics
    /// Panic if `params` is shorter than `param_num`
    fn build_instruction(
        self,
        params: &mut [Labelled<RlValue>],
    ) -> Result<Instruction, ReferenceInParamError> {
        use Instruction::*;
        Ok(match self {
            InstructionHeader::ADD(a, b, c) => ADD(
                mem::take(&mut params[0])
                    .map(|v| (a, v).try_into())
                    .transpose()?,
                mem::take(&mut params[1])
                    .map(|v| (b, v).try_into())
                    .transpose()?,
                mem::take(&mut params[2])
                    .map(|v| (c, v).try_into())
                    .transpose()?,
            ),
            InstructionHeader::MUL(a, b, c) => MUL(
                mem::take(&mut params[0])
                    .map(|v| (a, v).try_into())
                    .transpose()?,
                mem::take(&mut params[1])
                    .map(|v| (b, v).try_into())
                    .transpose()?,
                mem::take(&mut params[2])
                    .map(|v| (c, v).try_into())
                    .transpose()?,
            ),
            InstructionHeader::IN(a) => IN(mem::take(&mut params[0])
                .map(|v| (a, v).try_into())
                .transpose()?),
            InstructionHeader::OUT(a) => OUT(mem::take(&mut params[0])
                .map(|v| (a, v).try_into())
                .transpose()?),
            InstructionHeader::JZ(a, b) => JZ(
                mem::take(&mut params[0])
                    .map(|v| (a, v).try_into())
                    .transpose()?,
                mem::take(&mut params[1])
                    .map(|v| (b, v).try_into())
                    .transpose()?,
            ),
            InstructionHeader::JNZ(a, b) => JNZ(
                mem::take(&mut params[0])
                    .map(|v| (a, v).try_into())
                    .transpose()?,
                mem::take(&mut params[1])
                    .map(|v| (b, v).try_into())
                    .transpose()?,
            ),
            InstructionHeader::SLT(a, b, c) => SLT(
                mem::take(&mut params[0])
                    .map(|v| (a, v).try_into())
                    .transpose()?,
                mem::take(&mut params[1])
                    .map(|v| (b, v).try_into())
                    .transpose()?,
                mem::take(&mut params[2])
                    .map(|v| (c, v).try_into())
                    .transpose()?,
            ),
            InstructionHeader::SEQ(a, b, c) => SEQ(
                mem::take(&mut params[0])
                    .map(|v| (a, v).try_into())
                    .transpose()?,
                mem::take(&mut params[1])
                    .map(|v| (b, v).try_into())
                    .transpose()?,
                mem::take(&mut params[2])
                    .map(|v| (c, v).try_into())
                    .transpose()?,
            ),
            InstructionHeader::INCB(a) => INCB(
                mem::take(&mut params[0])
                    .map(|v| (a, v).try_into())
                    .transpose()?,
            ),
            InstructionHeader::HALT => HALT,
        })
    }
}
impl From<&Instruction> for InstructionHeader {
    fn from(value: &Instruction) -> Self {
        use InstructionHeader::*;
        match value {
            Instruction::ADD(a, b, c) => ADD(a.inner.mode(), b.inner.mode(), c.inner.mode()),
            Instruction::MUL(a, b, c) => MUL(a.inner.mode(), b.inner.mode(), c.inner.mode()),
            Instruction::IN(a) => IN(a.inner.mode()),
            Instruction::OUT(a) => OUT(a.inner.mode()),
            Instruction::JZ(a, b) => JZ(a.inner.mode(), b.inner.mode()),
            Instruction::JNZ(a, b) => JNZ(a.inner.mode(), b.inner.mode()),
            Instruction::SLT(a, b, c) => SLT(a.inner.mode(), b.inner.mode(), c.inner.mode()),
            Instruction::SEQ(a, b, c) => SEQ(a.inner.mode(), b.inner.mode(), c.inner.mode()),
            Instruction::INCB(a) => INCB(a.inner.mode()),
            Instruction::HALT => HALT,
        }
    }
}

#[derive(Debug, Error)]
pub enum InvalidInstructionHeader {
    #[error("Instruction header cannot be a relative value")]
    IsRelative,
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
impl TryFrom<RlValue> for InstructionHeader {
    type Error = InvalidInstructionHeader;

    fn try_from(value: RlValue) -> Result<Self, Self::Error> {
        use InvalidInstructionHeader::*;
        let value = value.try_into_absolute().map_err(|_| IsRelative)?;

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
impl From<InstructionHeader> for ICValue {
    fn from(value: InstructionHeader) -> Self {
        use InstructionHeader::*;
        let opcode = ICValue::from(OpCode::from(&value) as i64);
        let mut modes = ArrayVec::<ICValue, 3>::new();
        match value {
            ADD(a, b, c) | MUL(a, b, c) | SLT(a, b, c) | SEQ(a, b, c) => {
                modes.push(a.into());
                modes.push(b.into());
                modes.push(c.into());
            }
            JZ(a, b) | JNZ(a, b) => {
                modes.push(a.into());
                modes.push(b.into());
            }
            INCB(a) | OUT(a) => modes.push(a.into()),
            IN(a) => modes.push(a.into()),
            HALT => (),
        }
        let mut res = ICValue(0);
        for m in modes.into_iter().rev() {
            res = res * ICValue(10) + m
        }
        res * ICValue(100) + opcode
    }
}

/// A intcode opcode
#[derive(Debug, Clone, Copy, PartialEq, Eq, Display)]
pub enum OpCode {
    ADD = 1,
    MUL = 2,
    IN = 3,
    OUT = 4,
    JNZ = 5,
    JZ = 6,
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
impl From<&InstructionHeader> for OpCode {
    fn from(value: &InstructionHeader) -> Self {
        use InstructionHeader::*;
        match value {
            ADD(_, _, _) => Self::ADD,
            MUL(_, _, _) => Self::MUL,
            IN(_) => Self::IN,
            OUT(_) => Self::OUT,
            JZ(_, _) => Self::JZ,
            JNZ(_, _) => Self::JNZ,
            SLT(_, _, _) => Self::SLT,
            SEQ(_, _, _) => Self::SEQ,
            INCB(_) => Self::INCB,
            HALT => Self::HALT,
        }
    }
}
impl From<&Instruction> for OpCode {
    fn from(value: &Instruction) -> Self {
        use Instruction::*;
        match value {
            ADD(_, _, _) => Self::ADD,
            MUL(_, _, _) => Self::MUL,
            IN(_) => Self::IN,
            OUT(_) => Self::OUT,
            JZ(_, _) => Self::JZ,
            JNZ(_, _) => Self::JNZ,
            SLT(_, _, _) => Self::SLT,
            SEQ(_, _, _) => Self::SEQ,
            INCB(_) => Self::INCB,
            HALT => Self::HALT,
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
            5 => Ok(JNZ),
            6 => Ok(JZ),
            7 => Ok(SLT),
            8 => Ok(SEQ),
            9 => Ok(INCB),
            99 => Ok(HALT),
            _ => Err(UnrecognizedOpCode(value)),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::borrow::Cow;

    use crate::assembler::label::Labellable;
    use crate::identifier::Identifier;

    use super::super::label::Label;
    use super::{ICValue, Instruction, ReadParam, RlValue, WriteParam};

    #[test]
    fn generate_add() {
        let instr = Instruction::ADD(
            ReadParam::Position(RlValue::Absolute(ICValue(3))).into(),
            ReadParam::Immediate(RlValue::Absolute(ICValue(3))).labelled(Label::Global(
                Identifier::new(Cow::Borrowed("def")).unwrap(),
            )),
            WriteParam::Relative(ICValue(3)).into(),
        );
        let code: Vec<_> = instr
            .generate()
            .unwrap()
            .emit()
            .unwrap()
            .into_iter()
            .map(|v| v.0)
            .collect();
        assert_eq!(code, vec![21001, 3, 3, 3])
    }
}
