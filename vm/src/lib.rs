use std::io;

use serde::{Deserialize, Serialize};
use snafu::{OptionExt, Snafu};
use zicc_limits::Value;

use crate::{
    mem::Memory,
    program::Program,
    stream::{Reader, Writer},
};

pub mod mem;
pub mod program;
pub mod stream;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Vm {
    program: Program,
    memory: Memory,

    input: Option<Value>,
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
    pub fn run(&mut self) -> Result<StopState, RuntimeError> {
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
        mut input: Reader<R>,
        mut output: Writer<W>,
    ) -> Result<(), DriveError>
    where
        R: io::Read,
        W: io::Write,
    {
        loop {
            match self.run()? {
                StopState::Output(value) => output.write(value)?,
                StopState::NeedInput => {
                    self.input(input.read()?.context(UnexpectedEofSnafu)?);
                }
                StopState::Halted => return Ok(()),
            }
        }
    }

    pub fn step(&mut self) -> Result<State, RuntimeError> {
        todo!()
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

#[derive(Debug, Snafu)]
pub enum RuntimeError {}

#[derive(Debug, Snafu)]
pub enum DriveError {
    #[snafu(transparent)]
    Stream { source: stream::Error },
    #[snafu(transparent)]
    Runtime { source: RuntimeError },
    #[snafu(display("Program requested more input that available"))]
    UnexpectedEof,
}
