#![doc = include_str!("../README.md")]

use arrayvec::ArrayVec;

/// An IntCode instruction
///
/// `ReadParam` is used for params that are only read by the instruction,
/// `WriteParam` instead params that are only written to.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Instruction<ReadParam, WriteParam> {
    Add(ReadParam, ReadParam, WriteParam),
    Mul(ReadParam, ReadParam, WriteParam),
    In(WriteParam),
    Out(ReadParam),
    Jnz(ReadParam, ReadParam),
    Jz(ReadParam, ReadParam),
    Slt(ReadParam, ReadParam, WriteParam),
    Seq(ReadParam, ReadParam, WriteParam),
    Incb(ReadParam),
    Halt,
}

/// An IntCode opcode
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u8)]
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
    fn params_len(&self) -> usize {
        match self {
            OpCode::ADD | OpCode::MUL | OpCode::SLT | OpCode::SEQ => 3,
            OpCode::JNZ | OpCode::JZ => 2,
            OpCode::IN | OpCode::OUT | OpCode::INCB => 1,
            OpCode::HALT => 0,
        }
    }
}

impl<R, W> Instruction<R, W> {
    /// Get the opcode for this instruction
    pub fn opcode(&self) -> OpCode {
        match self {
            Instruction::Add(_, _, _) => OpCode::ADD,
            Instruction::Mul(_, _, _) => OpCode::MUL,
            Instruction::In(_) => OpCode::IN,
            Instruction::Out(_) => OpCode::OUT,
            Instruction::Jnz(_, _) => OpCode::JNZ,
            Instruction::Jz(_, _) => OpCode::JZ,
            Instruction::Slt(_, _, _) => OpCode::SLT,
            Instruction::Seq(_, _, _) => OpCode::SEQ,
            Instruction::Incb(_) => OpCode::INCB,
            Instruction::Halt => OpCode::HALT,
        }
    }

    /// Parse the parameter of an instruction from a stream
    ///
    /// The stream will be advanced to the end of the instruction. If an error
    /// happens, it will be advanced of one if the error is in the opcode, or to
    /// the end if the error is in the params.
    pub fn parse<P, EO, EW, ER>(
        params: &mut impl Iterator<Item = P>,
        parse_opcode: impl FnOnce(P) -> Result<OpCode, EO>,
        mut parse_read: impl FnMut(P) -> Result<R, ER>,
        mut parse_write: impl FnMut(P) -> Result<W, EW>,
    ) -> Result<Self, ParseInstructionError<EO, ER, EW>> {
        let opcode = params
            .next()
            .ok_or(ParseInstructionError::NotEnoughParams)
            .and_then(|p| {
                parse_opcode(p)
                    .map_err(|source| ParseInstructionError::<EO, ER, EW>::InvalidOpCode { source })
            })?;
        let mut parse_write = |i: &mut arrayvec::IntoIter<_, 3>| {
            i.next()
                .ok_or(ParseInstructionError::NotEnoughParams)
                .and_then(|p| {
                    parse_write(p).map_err(|source| {
                        ParseInstructionError::<EO, ER, EW>::InvalidWriteParam { source }
                    })
                })
        };
        let mut parse_read = |i: &mut arrayvec::IntoIter<_, 3>| {
            i.next()
                .ok_or(ParseInstructionError::NotEnoughParams)
                .and_then(|p| {
                    parse_read(p).map_err(|source| {
                        ParseInstructionError::<EO, ER, EW>::InvalidReadParam { source }
                    })
                })
        };

        // Greedily take all the params, so the iterator is left always in a coherent position
        let mut params = params
            .take(opcode.params_len())
            .collect::<ArrayVec<_, 3>>()
            .into_iter();

        Ok(match opcode {
            OpCode::ADD => {
                let a = parse_read(&mut params)?;
                let b = parse_read(&mut params)?;
                let c = parse_write(&mut params)?;
                Instruction::Add(a, b, c)
            }
            OpCode::MUL => {
                let a = parse_read(&mut params)?;
                let b = parse_read(&mut params)?;
                let c = parse_write(&mut params)?;
                Instruction::Mul(a, b, c)
            }
            OpCode::IN => {
                let a = parse_write(&mut params)?;
                Instruction::In(a)
            }
            OpCode::OUT => {
                let a = parse_read(&mut params)?;
                Instruction::Out(a)
            }
            OpCode::JNZ => {
                let a = parse_read(&mut params)?;
                let b = parse_read(&mut params)?;
                Instruction::Jnz(a, b)
            }
            OpCode::JZ => {
                let a = parse_read(&mut params)?;
                let b = parse_read(&mut params)?;
                Instruction::Jz(a, b)
            }
            OpCode::SLT => {
                let a = parse_read(&mut params)?;
                let b = parse_read(&mut params)?;
                let c = parse_write(&mut params)?;
                Instruction::Slt(a, b, c)
            }
            OpCode::SEQ => {
                let a = parse_read(&mut params)?;
                let b = parse_read(&mut params)?;
                let c = parse_write(&mut params)?;
                Instruction::Seq(a, b, c)
            }
            OpCode::INCB => {
                let a = parse_read(&mut params)?;
                Instruction::Incb(a)
            }
            OpCode::HALT => Instruction::Halt,
        })
    }

    /// Serialize this instruction
    pub fn serialize<P>(
        self,
        serialize_opcode: impl FnOnce(OpCode) -> P,
        mut serialize_read: impl FnMut(R) -> P,
        mut serialize_write: impl FnMut(W) -> P,
    ) -> ArrayVec<P, 4> {
        let mut out = ArrayVec::new();
        out.push(serialize_opcode(self.opcode()));
        match self {
            Instruction::Add(a, b, c)
            | Instruction::Mul(a, b, c)
            | Instruction::Slt(a, b, c)
            | Instruction::Seq(a, b, c) => {
                out.push(serialize_read(a));
                out.push(serialize_read(b));
                out.push(serialize_write(c));
            }
            Instruction::Jnz(a, b) | Instruction::Jz(a, b) => {
                out.push(serialize_read(a));
                out.push(serialize_read(b));
            }
            Instruction::In(a) => out.push(serialize_write(a)),
            Instruction::Out(a) | Instruction::Incb(a) => out.push(serialize_read(a)),
            Instruction::Halt => {}
        }
        out
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

#[derive(Debug)]
pub enum ParseInstructionError<EO, ER, EW> {
    NotEnoughParams,
    InvalidOpCode { source: EO },
    InvalidReadParam { source: ER },
    InvalidWriteParam { source: EW },
}
