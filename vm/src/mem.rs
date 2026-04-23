use serde::{Deserialize, Serialize};
use snafu::{ResultExt, Snafu};
use zicc_intcode::{Instruction, InvalidCodeError, ReadParamMode, WriteParamMode};
use zicc_limits::{CastValueToIntError, PointerOffset, Value};

use crate::program::Program;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Memory {
    ip: usize,
    rb: PointerOffset,
    content: Vec<Value>,
}

/// To simulate infinite memory, this is the value returned for any index
/// over the current memory lenght
static DEFAULT_OVER_MEMORY: Value = Value::ZERO;

impl Memory {
    /// Create a new memory, with nothing inside
    pub fn new() -> Self {
        Self {
            content: vec![],
            ip: 0,
            rb: 0,
        }
    }

    /// Empty the memory, keeping the allocations
    pub fn clear(&mut self) {
        self.content.clear();
        self.ip = 0;
        self.rb = 0;
    }

    /// Load a program into memory
    ///
    /// This will also set the register to the start of the program, and clear
    /// the rest of the memory
    pub fn load(&mut self, program: &Program) {
        self.content
            .resize(program.len(), DEFAULT_OVER_MEMORY.clone());
        self.content.clone_from_slice(&program.content);
        self.ip = 0;
        self.rb = 0;
    }

    /// Read an instruction at the current instruction pointer
    pub fn read_instruction(&self) -> Result<Instruction<&Value>, ReadInstructionError> {
        use Instruction::*;

        let [opcode, a, b, c] = [0, 1, 2, 3].map(|i| self.get(self.ip + i));
        Ok(match Instruction::decode(opcode)? {
            Add((ma, ()), (mb, ()), (mc, ())) => Add((ma, a), (mb, b), (mc, c)),
            Mul((ma, ()), (mb, ()), (mc, ())) => Mul((ma, a), (mb, b), (mc, c)),
            Inp((ma, ())) => Inp((ma, a)),
            Out((ma, ())) => Out((ma, a)),
            Jnz((ma, ()), (mb, ())) => Jnz((ma, a), (mb, b)),
            Jez((ma, ()), (mb, ())) => Jez((ma, a), (mb, b)),
            Slt((ma, ()), (mb, ()), (mc, ())) => Slt((ma, a), (mb, b), (mc, c)),
            Seq((ma, ()), (mb, ()), (mc, ())) => Seq((ma, a), (mb, b), (mc, c)),
            Inb((ma, ())) => Inb((ma, a)),
            Hlt => Hlt,
        })
    }

    /// Advance over an instruction
    pub fn advance_over<P>(&mut self, instr: &Instruction<P>) {
        self.ip += 1 + instr.opcode().param_count()
    }

    /// Jump to a given point
    pub fn jump(&mut self, pos: &Value) -> Result<(), JumpedOutOfMemError> {
        self.ip =
            usize::try_from(pos).with_context(|_| JumpedOutOfMemSnafu { pos: pos.clone() })?;
        Ok(())
    }

    /// Read a value from a position
    ///
    /// Read a value from a position, and return the result.
    ///
    /// The lifetime constraints are because if the mode is immediate, the
    /// reference to the position is returned.
    pub fn read<'v>(
        &'v self,
        mode: ReadParamMode,
        pos_or_value: &'v Value,
    ) -> Result<&'v Value, IndexError> {
        let index = match mode {
            ReadParamMode::Absolute => self.value_to_index(pos_or_value, false),
            ReadParamMode::Immediate => return Ok(pos_or_value),
            ReadParamMode::Relative => self.value_to_index(pos_or_value, true),
        }?;

        Ok(self.get(index))
    }

    fn get(&self, index: usize) -> &Value {
        self.content.get(index).unwrap_or(&DEFAULT_OVER_MEMORY)
    }

    /// Write a value to a position
    ///
    /// Replace the value at that position with a given one. Will allocate
    /// as needed to fit long distance writes.
    ///
    /// As for now the memory is contiguos, beware that writes to very far
    /// memory location will allocate enormous quantity of memory.
    pub fn write(
        &mut self,
        mode: WriteParamMode,
        pos: &Value,
        value: Value,
    ) -> Result<(), IndexError> {
        let index = match mode {
            WriteParamMode::Absolute => self.value_to_index(pos, false),
            WriteParamMode::Relative => self.value_to_index(pos, true),
        }?;

        // If value is not the default, extend memory to need and write it.
        // Else, write if in memory and trim if needed
        if value != DEFAULT_OVER_MEMORY {
            if index >= self.content.len() {
                self.content.resize(index + 1, DEFAULT_OVER_MEMORY.clone());
            }

            self.content[index] = value;
        } else if let Some(cell) = self.content.get_mut(index) {
            *cell = value;

            // Trim the memory from exceeding default values
            if index + 1 == self.content.len() {
                while self.content.pop_if(|v| v == &DEFAULT_OVER_MEMORY).is_some() {}
            }
        }

        Ok(())
    }

    fn value_to_index(&self, pos: &Value, relative: bool) -> Result<usize, IndexError> {
        let offset = if relative { self.rb } else { 0 };
        let resulting = pos + offset;
        usize::try_from(resulting).with_context(|_| OutOfMemSnafu {
            pos: pos.clone(),
            relative: relative.then_some(self.rb),
        })
    }
}

impl Default for Memory {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Snafu, Clone)]
pub enum IndexError {
    OutOfMem {
        pos: Value,
        relative: Option<PointerOffset>,
        source: <usize as TryFrom<Value>>::Error,
    },
}

#[derive(Debug, Snafu, Clone)]
pub enum ReadInstructionError {
    #[snafu(transparent)]
    InvalidCode { source: InvalidCodeError },
}

#[derive(Debug, Snafu, Clone)]
pub struct JumpedOutOfMemError {
    pos: Value,
    source: CastValueToIntError,
}
