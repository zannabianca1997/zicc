#![doc = include_str!("../README.md")]

use derive_more::Display;
use num_derive::{FromPrimitive, ToPrimitive};
use num_traits::FromPrimitive;
use snafu::{OptionExt, Snafu};
use zicc_limits::Value;

/// An IntCode instruction
///
/// `ReadParam` is used for params that are only read by the instruction,
/// `WriteParam` instead params that are only written to.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Instruction<Param> {
    Add(
        (ReadParamMode, Param),
        (ReadParamMode, Param),
        (WriteParamMode, Param),
    ),
    Mul(
        (ReadParamMode, Param),
        (ReadParamMode, Param),
        (WriteParamMode, Param),
    ),
    Inp((WriteParamMode, Param)),
    Out((ReadParamMode, Param)),
    Jnz((ReadParamMode, Param), (ReadParamMode, Param)),
    Jez((ReadParamMode, Param), (ReadParamMode, Param)),
    Slt(
        (ReadParamMode, Param),
        (ReadParamMode, Param),
        (WriteParamMode, Param),
    ),
    Seq(
        (ReadParamMode, Param),
        (ReadParamMode, Param),
        (WriteParamMode, Param),
    ),
    Inb((ReadParamMode, Param)),
    Hlt,
}

/// An IntCode opcode
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, FromPrimitive, ToPrimitive, Display)]
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

    pub fn from_u8(code: u8) -> Result<Self, InvalidOpCodeError> {
        FromPrimitive::from_u8(code).context(InvalidOpCodeSnafu { code })
    }
}

impl<P> Instruction<P> {
    /// Encode the instruction back to an instruction code (inverse of [`Self::decode`])
    pub fn code(&self) -> Value {
        use Instruction::*;

        let op = self.opcode().to_u8() as u16;

        match self {
            Add((a, _), (b, _), (c, _))
            | Mul((a, _), (b, _), (c, _))
            | Slt((a, _), (b, _), (c, _))
            | Seq((a, _), (b, _), (c, _)) => {
                op + a.to_u8() as u16 * 100 + b.to_u8() as u16 * 1000 + c.to_u8() as u16 * 10000
            }
            Jnz((a, _), (b, _)) | Jez((a, _), (b, _)) => {
                op + a.to_u8() as u16 * 100 + b.to_u8() as u16 * 1000
            }
            Inp((a, _)) => op + a.to_u8() as u16 * 100,
            Out((a, _)) | Inb((a, _)) => op + a.to_u8() as u16 * 100,
            Hlt => op,
        }
        .into()
    }

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

    pub fn len(&self) -> usize {
        1 + self.opcode().param_count()
    }
}

impl Instruction<()> {
    /// Decode an instruction code
    ///
    /// Decode an instruction code into its parts: opcode and params modes.
    pub fn decode(code: &Value) -> Result<Self, InvalidCodeError> {
        use Instruction::*;
        use OpCode::*;

        let code = u32::try_from(code)
            .ok()
            .with_context(|| AdditionalDigitsSnafu { code: code.clone() })?;

        let opcode = OpCode::from_u8((code % 100) as _)?;

        if (code as u32) >= 100 * 10u32.pow(opcode.param_count() as _) {
            return Err(InvalidCodeError::AdditionalDigits { code: code.into() });
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

impl<P> From<Instruction<P>> for OpCode {
    fn from(value: Instruction<P>) -> Self {
        value.opcode()
    }
}

impl<P> From<&Instruction<P>> for OpCode {
    fn from(value: &Instruction<P>) -> Self {
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
    type Error = ImmediateModeOnWriteParamError;

    fn try_from(value: ReadParamMode) -> Result<Self, Self::Error> {
        match value {
            ReadParamMode::Absolute => Ok(Self::Absolute),
            ReadParamMode::Immediate => Err(ImmediateModeOnWriteParamError),
            ReadParamMode::Relative => Ok(Self::Relative),
        }
    }
}

impl ReadParamMode {
    pub fn to_u8(self) -> u8 {
        self as u8
    }

    pub fn from_u8(code: u8) -> Result<Self, InvalidParamModeCodeError> {
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
pub struct ImmediateModeOnWriteParamError;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Snafu)]
#[snafu(display("{code} is not a valid opcode"))]
pub struct InvalidOpCodeError {
    code: u8,
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Snafu)]
#[snafu(display("{code} is not a valid param mode code"))]
pub struct InvalidParamModeCodeError {
    code: u8,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Snafu)]
pub enum InvalidCodeError {
    #[snafu(display("{code} has additional digits over the needed params"))]
    AdditionalDigits { code: Value },
    #[snafu(transparent)]
    InvalidOpcode { source: InvalidOpCodeError },
    #[snafu(transparent)]
    InvalidParamModeCode { source: InvalidParamModeCodeError },
    #[snafu(transparent)]
    ImmediateModeOnWriteParam {
        source: ImmediateModeOnWriteParamError,
    },
}

#[cfg(test)]
mod tests {
    use super::*;
    use Instruction::*;
    use ReadParamMode as R;
    use WriteParamMode as W;

    // --- ReadParamMode ---

    /// 0 decodes to Absolute
    #[test]
    fn read_param_mode_from_u8_should_parse_absolute() {
        assert_eq!(R::from_u8(0), Ok(R::Absolute));
    }

    /// 1 decodes to Immediate
    #[test]
    fn read_param_mode_from_u8_should_parse_immediate() {
        assert_eq!(R::from_u8(1), Ok(R::Immediate));
    }

    /// 2 decodes to Relative
    #[test]
    fn read_param_mode_from_u8_should_parse_relative() {
        assert_eq!(R::from_u8(2), Ok(R::Relative));
    }

    /// 3 and above are rejected
    #[test]
    fn read_param_mode_from_u8_should_reject_invalid() {
        assert!(R::from_u8(3).is_err());
        assert!(R::from_u8(255).is_err());
    }

    /// to_u8 then from_u8 is identity for all variants
    #[test]
    fn read_param_mode_to_u8_should_roundtrip() {
        for mode in [R::Absolute, R::Immediate, R::Relative] {
            assert_eq!(R::from_u8(mode.to_u8()), Ok(mode));
        }
    }

    // --- WriteParamMode ---

    /// Absolute encodes to 0
    #[test]
    fn write_param_mode_absolute_should_encode_to_0() {
        assert_eq!(W::Absolute.to_u8(), 0);
    }

    /// Relative encodes to 2
    #[test]
    fn write_param_mode_relative_should_encode_to_2() {
        assert_eq!(W::Relative.to_u8(), 2);
    }

    /// ReadParamMode::Absolute converts to WriteParamMode::Absolute
    #[test]
    fn write_param_mode_should_convert_from_absolute() {
        assert_eq!(W::try_from(R::Absolute), Ok(W::Absolute));
    }

    /// ReadParamMode::Relative converts to WriteParamMode::Relative
    #[test]
    fn write_param_mode_should_convert_from_relative() {
        assert_eq!(W::try_from(R::Relative), Ok(W::Relative));
    }

    /// Immediate mode is rejected as a write mode
    #[test]
    fn write_param_mode_should_reject_immediate() {
        assert_eq!(
            W::try_from(R::Immediate),
            Err(ImmediateModeOnWriteParamError)
        );
    }

    // --- decode happy paths ---

    /// 99 → Hlt
    #[test]
    fn decode_should_parse_hlt() {
        assert_eq!(Instruction::decode(&99.into()), Ok(Hlt));
    }

    /// 1 → Add with all Absolute modes
    #[test]
    fn decode_should_parse_add_with_all_absolute_modes() {
        assert_eq!(
            Instruction::decode(&1.into()),
            Ok(Add((R::Absolute, ()), (R::Absolute, ()), (W::Absolute, ())))
        );
    }

    /// 101 → Add with Immediate first param
    #[test]
    fn decode_should_parse_add_with_immediate_first_param() {
        assert_eq!(
            Instruction::decode(&101.into()),
            Ok(Add(
                (R::Immediate, ()),
                (R::Absolute, ()),
                (W::Absolute, ())
            ))
        );
    }

    /// 20001 → Add with Relative write param (c=2)
    #[test]
    fn decode_should_parse_add_with_relative_write_param() {
        assert_eq!(
            Instruction::decode(&20001.into()),
            Ok(Add((R::Absolute, ()), (R::Absolute, ()), (W::Relative, ())))
        );
    }

    /// 2 → Mul with all Absolute modes
    #[test]
    fn decode_should_parse_mul() {
        assert_eq!(
            Instruction::decode(&2.into()),
            Ok(Mul((R::Absolute, ()), (R::Absolute, ()), (W::Absolute, ())))
        );
    }

    /// 3 → Inp Absolute
    #[test]
    fn decode_should_parse_inp_absolute() {
        assert_eq!(Instruction::decode(&3.into()), Ok(Inp((W::Absolute, ()))));
    }

    /// 203 → Inp Relative
    #[test]
    fn decode_should_parse_inp_relative() {
        assert_eq!(Instruction::decode(&203.into()), Ok(Inp((W::Relative, ()))));
    }

    /// 4 → Out Absolute
    #[test]
    fn decode_should_parse_out_absolute() {
        assert_eq!(Instruction::decode(&4.into()), Ok(Out((R::Absolute, ()))));
    }

    /// 104 → Out Immediate
    #[test]
    fn decode_should_parse_out_immediate() {
        assert_eq!(
            Instruction::decode(&104.into()),
            Ok(Out((R::Immediate, ())))
        );
    }

    /// 5 → Jnz with Absolute modes
    #[test]
    fn decode_should_parse_jnz() {
        assert_eq!(
            Instruction::decode(&5.into()),
            Ok(Jnz((R::Absolute, ()), (R::Absolute, ())))
        );
    }

    /// 6 → Jez with Absolute modes
    #[test]
    fn decode_should_parse_jez() {
        assert_eq!(
            Instruction::decode(&6.into()),
            Ok(Jez((R::Absolute, ()), (R::Absolute, ())))
        );
    }

    /// 7 → Slt with all Absolute modes
    #[test]
    fn decode_should_parse_slt() {
        assert_eq!(
            Instruction::decode(&7.into()),
            Ok(Slt((R::Absolute, ()), (R::Absolute, ()), (W::Absolute, ())))
        );
    }

    /// 8 → Seq with all Absolute modes
    #[test]
    fn decode_should_parse_seq() {
        assert_eq!(
            Instruction::decode(&8.into()),
            Ok(Seq((R::Absolute, ()), (R::Absolute, ()), (W::Absolute, ())))
        );
    }

    /// 9 → Inb Absolute
    #[test]
    fn decode_should_parse_inb() {
        assert_eq!(Instruction::decode(&9.into()), Ok(Inb((R::Absolute, ()))));
    }

    // --- decode error paths ---

    /// Opcode 0 is invalid
    #[test]
    fn decode_should_reject_invalid_opcode() {
        assert!(matches!(
            Instruction::decode(&0.into()),
            Err(InvalidCodeError::InvalidOpcode { .. })
        ));
        assert!(matches!(
            Instruction::decode(&10.into()),
            Err(InvalidCodeError::InvalidOpcode { .. })
        ));
    }

    /// Mode digit 3 is not a valid param mode
    #[test]
    fn decode_should_reject_invalid_param_mode() {
        assert!(matches!(
            Instruction::decode(&301.into()),
            Err(InvalidCodeError::InvalidParamModeCode { .. })
        ));
    }

    /// Immediate mode (1) on a write param is rejected
    #[test]
    fn decode_should_reject_immediate_mode_on_write_param() {
        // Add: c=1 → 10001
        assert!(matches!(
            Instruction::decode(&10001.into()),
            Err(InvalidCodeError::ImmediateModeOnWriteParam { .. })
        ));
    }

    /// Extra digits beyond the param count are rejected
    #[test]
    fn decode_should_reject_additional_digits_on_hlt() {
        assert!(matches!(
            Instruction::decode(&199.into()),
            Err(InvalidCodeError::AdditionalDigits { .. })
        ));
    }

    /// Extra digit in second-param position for a 1-param instruction
    #[test]
    fn decode_should_reject_additional_digits_on_one_param_instruction() {
        // Out (1 param): b digit set → 1004
        assert!(matches!(
            Instruction::decode(&1004.into()),
            Err(InvalidCodeError::AdditionalDigits { .. })
        ));
    }

    // --- code direct encoding ---

    /// Hlt encodes to 99
    #[test]
    fn hlt_should_encode_to_99() {
        assert_eq!(Hlt::<()>.code(), 99.into());
    }

    /// Add with all Absolute modes encodes to 1
    #[test]
    fn add_with_absolute_modes_should_encode_to_1() {
        assert_eq!(
            Add::<()>((R::Absolute, ()), (R::Absolute, ()), (W::Absolute, ())).code(),
            1.into()
        );
    }

    /// Add with Immediate first param encodes to 101
    #[test]
    fn add_with_immediate_first_param_should_encode_to_101() {
        assert_eq!(
            Add::<()>((R::Immediate, ()), (R::Absolute, ()), (W::Absolute, ())).code(),
            101.into()
        );
    }

    /// Add with Relative write param encodes to 20001
    #[test]
    fn add_with_relative_write_param_should_encode_to_20001() {
        assert_eq!(
            Add::<()>((R::Absolute, ()), (R::Absolute, ()), (W::Relative, ())).code(),
            20001.into()
        );
    }

    /// Inp with Relative mode encodes to 203
    #[test]
    fn inp_with_relative_mode_should_encode_to_203() {
        assert_eq!(Inp::<()>((W::Relative, ())).code(), 203.into());
    }

    /// Out with Immediate mode encodes to 104
    #[test]
    fn out_with_immediate_mode_should_encode_to_104() {
        assert_eq!(Out::<()>((R::Immediate, ())).code(), 104.into());
    }

    // --- roundtrip ---

    /// decode then code is identity for a representative set of valid codes
    #[test]
    fn decode_then_code_should_be_identity() {
        let codes: &[Value] = &[
            1.into(),
            2.into(),
            3.into(),
            4.into(),
            5.into(),
            6.into(),
            7.into(),
            8.into(),
            9.into(),
            99.into(), // default modes
            101.into(),
            1001.into(),
            20001.into(), // Add variants
            203.into(),
            104.into(),
            1005.into(), // misc mode variants
        ];
        for code in codes {
            let instr =
                Instruction::decode(code).unwrap_or_else(|e| panic!("decode({code}) failed: {e}"));
            assert_eq!(&instr.code(), code, "roundtrip failed for code {code}");
        }
    }

    /// code then decode is identity for all constructed Instruction<(),()> values
    #[test]
    fn code_then_decode_should_be_identity() {
        let instructions: &[Instruction<()>] = &[
            Hlt,
            Add((R::Absolute, ()), (R::Absolute, ()), (W::Absolute, ())),
            Add((R::Immediate, ()), (R::Relative, ()), (W::Relative, ())),
            Mul((R::Absolute, ()), (R::Immediate, ()), (W::Absolute, ())),
            Inp((W::Absolute, ())),
            Inp((W::Relative, ())),
            Out((R::Absolute, ())),
            Out((R::Immediate, ())),
            Jnz((R::Absolute, ()), (R::Immediate, ())),
            Jez((R::Relative, ()), (R::Absolute, ())),
            Slt((R::Absolute, ()), (R::Absolute, ()), (W::Relative, ())),
            Seq((R::Immediate, ()), (R::Absolute, ()), (W::Absolute, ())),
            Inb((R::Absolute, ())),
            Inb((R::Relative, ())),
        ];
        for instr in instructions {
            let code = instr.code();
            let decoded =
                Instruction::decode(&code).unwrap_or_else(|e| panic!("decode({code}) failed: {e}"));
            assert_eq!(&decoded, instr, "roundtrip failed for {instr:?}");
        }
    }
}
