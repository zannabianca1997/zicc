use derive_more::Display;
use serde::{Deserialize, Serialize};
use snafu::{ResultExt, Snafu};
use zicc_limits::Value;

use zicc_vm_program::Program;
use zicc_vm_stream::{Reader, Writer};

use crate::{
    hooks::Hooks,
    mem::{IndexError, JumpedOutOfMemError, Memory, ReadInstructionError},
};

pub mod cli;
pub mod hooks;
pub mod mem;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Vm<Hooks: ?Sized = ()> {
    program: Program,
    memory: Memory,

    input: Option<Value>,

    hooks: Hooks,
}

impl Vm {
    /// Create a new vm to run the given program
    pub fn new(program: Program) -> Self {
        let mut memory = Memory::new();
        memory.load(&program);

        Self {
            program,
            memory,
            input: None,
            hooks: (),
        }
    }
}

impl<H> Vm<H> {
    /// Create a new vm to run the given program, adding hooks to the runtime
    pub fn new_with_hooks(program: Program, hooks: H) -> Self {
        let Vm {
            program,
            memory,
            input,
            hooks: (),
        } = Vm::new(program);

        Self {
            program,
            memory,
            input,
            hooks,
        }
    }

    /// Reset the vm
    ///
    /// This will reload the program, and flush any input or output
    pub fn reset(&mut self) {
        self.memory.load(&self.program);
        self.input = None;
    }

    /// Feed input in the vm
    ///
    /// Return [`Some`] if the input queue is full
    pub fn input(&mut self, value: Value) -> Option<Value> {
        if self.input.is_some() {
            return Some(value);
        }
        self.input = Some(value);
        None
    }

    /// Run until a stop state
    ///
    /// Run the vm until a stop state is reached - meaning some sort of
    /// interaction is required to continue operating.
    pub fn run(&mut self) -> Result<StopState, RuntimeError>
    where
        H: Hooks,
    {
        loop {
            if let State::StopState(stop_state) = self.step()? {
                return Ok(stop_state);
            }
        }
    }

    /// Drive the vm
    ///
    /// Drive the vm until halted, using the given streams as input and output.
    pub fn drive<R, W>(
        &mut self,
        mut input: R,
        mut output: W,
    ) -> Result<(), DriveError<R::Error, W::Error>>
    where
        R: Reader,
        W: Writer,
        H: Hooks,
    {
        loop {
            match self.run()? {
                StopState::Output(value) => output
                    .write(value)
                    .map_err(|source| DriveError::Writer { source })?,
                StopState::NeedInput => {
                    self.input(
                        input
                            .read()
                            .map_err(|source| DriveError::Reader { source })?
                            .ok_or(DriveError::UnexpectedEof)?,
                    );
                }
                StopState::Halted => return Ok(()),
            }
        }
    }

    /// Advance the vm monotonically
    pub fn step(&mut self) -> Result<State, RuntimeError>
    where
        H: Hooks,
    {
        let (pos, instruction) = self
            .memory
            .read_instruction()
            .context(ReadInstructionSnafu)?;

        self.hooks
            .before_instruction(&instruction, pos.clone(), &self.memory);

        match instruction {
            zicc_intcode::Instruction::Add(a, b, c) => {
                let a = self.memory.read(a).context(ReadMemorySnafu)?;
                let b = self.memory.read(b).context(ReadMemorySnafu)?;

                let res = a + b;

                self.memory.write(c, res).context(WriteMemorySnafu)?;
                self.memory.advance_over(&instruction);
            }
            zicc_intcode::Instruction::Mul(a, b, c) => {
                let a = self.memory.read(a).context(ReadMemorySnafu)?;
                let b = self.memory.read(b).context(ReadMemorySnafu)?;

                let res = a * b;

                self.memory.write(c, res).context(WriteMemorySnafu)?;
                self.memory.advance_over(&instruction);
            }
            zicc_intcode::Instruction::Inp(a) => {
                let Some(value) = self.input.take() else {
                    return Ok(State::StopState(StopState::NeedInput));
                };
                self.memory.write(a, value).context(WriteMemorySnafu)?;
                self.memory.advance_over(&instruction);
            }
            zicc_intcode::Instruction::Out(a) => {
                let a = self.memory.read(a).context(ReadMemorySnafu)?.clone();
                self.memory.advance_over(&instruction);
                return Ok(State::StopState(StopState::Output(a)));
            }
            zicc_intcode::Instruction::Jnz(a, b) => {
                let a = self.memory.read(a).context(ReadMemorySnafu)?;

                if a != &Value::ZERO {
                    let b = self.memory.read(b).context(ReadMemorySnafu)?.clone();
                    self.memory.jump(&b).context(JumpedOutOfMemSnafu)?;
                } else {
                    self.memory.advance_over(&instruction);
                }
            }
            zicc_intcode::Instruction::Jez(a, b) => {
                let a = self.memory.read(a).context(ReadMemorySnafu)?;

                if a == &Value::ZERO {
                    let b = self.memory.read(b).context(ReadMemorySnafu)?.clone();
                    self.memory.jump(&b).context(JumpedOutOfMemSnafu)?;
                } else {
                    self.memory.advance_over(&instruction);
                }
            }
            zicc_intcode::Instruction::Slt(a, b, c) => {
                let a = self.memory.read(a).context(ReadMemorySnafu)?;
                let b = self.memory.read(b).context(ReadMemorySnafu)?;

                let res = if a < b { Value::from(1) } else { Value::ZERO };

                self.memory.write(c, res).context(WriteMemorySnafu)?;
                self.memory.advance_over(&instruction);
            }
            zicc_intcode::Instruction::Seq(a, b, c) => {
                let a = self.memory.read(a).context(ReadMemorySnafu)?;
                let b = self.memory.read(b).context(ReadMemorySnafu)?;

                let res = if a == b { Value::from(1) } else { Value::ZERO };

                self.memory.write(c, res).context(WriteMemorySnafu)?;
                self.memory.advance_over(&instruction);
            }
            zicc_intcode::Instruction::Inb(a) => {
                let a = self.memory.read(a).context(ReadMemorySnafu)?.clone();
                self.memory.increase_relative_base(&a);
                self.memory.advance_over(&instruction);
            }
            zicc_intcode::Instruction::Hlt => return Ok(State::StopState(StopState::Halted)),
        };
        Ok(State::Running)
    }
}

/// State of the Vm after a step
#[derive(Debug, Clone)]
pub enum State {
    /// Vm is running
    Running,
    /// Vm has stopped
    StopState(StopState),
}

/// Reason a Vm has stopped
#[derive(Debug, Clone)]
pub enum StopState {
    /// Vm has produced output
    Output(Value),
    /// Vm has finished its input buffer
    NeedInput,
    /// Vm has halted
    Halted,
}

/// Runtime error
#[derive(Debug, Snafu)]
pub enum RuntimeError {
    ReadInstruction { source: ReadInstructionError },
    ReadMemory { source: IndexError },
    WriteMemory { source: IndexError },
    JumpedOutOfMem { source: JumpedOutOfMemError },
}

/// Error while driving the virtual machine
#[derive(Debug, Display)]
pub enum DriveError<ReaderError, WriterError> {
    #[display("Error in input")]
    Reader { source: ReaderError },
    #[display("Error in output")]
    Writer { source: WriterError },
    #[display("{source}")]
    Runtime { source: RuntimeError },
    #[display("Unexpected end of input")]
    UnexpectedEof,
}

impl<R, W> From<RuntimeError> for DriveError<R, W> {
    fn from(source: RuntimeError) -> Self {
        DriveError::Runtime { source }
    }
}

impl<R, W> std::error::Error for DriveError<R, W>
where
    R: std::error::Error + 'static,
    W: std::error::Error + 'static,
{
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            DriveError::Reader { source } => Some(source),
            DriveError::Writer { source } => Some(source),
            DriveError::Runtime { source } => std::error::Error::source(source),
            DriveError::UnexpectedEof => None,
        }
    }
}
