use serde::{Deserialize, Serialize};
use snafu::{ResultExt, Snafu};
use zicc_intcode::{ReadParamMode, WriteParamMode};
use zicc_limits::{Pointer, PointerOffset, Value};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Mem {
    ip: Pointer,
    rb: PointerOffset,
    content: Vec<Value>,
}

/// To simulate infinite memory, this is the value returned for any index
/// over the current memory lenght
static DEFAULT_OVER_MEMORY: Value = Value::ZERO;

impl Mem {
    pub fn new() -> Self {
        Self {
            content: vec![],
            ip: 0,
            rb: 0,
        }
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

        Ok(self.content.get(index).unwrap_or(&DEFAULT_OVER_MEMORY))
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
            while index >= self.content.len() {
                self.content.push(DEFAULT_OVER_MEMORY.clone());
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

impl Default for Mem {
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
