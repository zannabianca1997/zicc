//! Implementation of a intcode machine
//!
//! This is kept indipendent from the rest of the project, so it can be substituted with any equivalent implementation

use thiserror::Error;

pub type VMInt = i64;

/// Any state of a machine
#[derive(Debug)]
pub enum State<'vm> {
    Stopped(StopState<'vm>),
    Running,
}

/// Possible state in which a machine need interaction
#[derive(Debug)]
pub enum StopState<'vm> {
    NeedInput(NeedInput<'vm>),
    HasOutput(HasOutput<'vm>),
    Halted,
}

#[derive(Debug)]
pub struct NeedInput<'vm>(&'vm mut Option<VMInt>);
impl NeedInput<'_> {
    pub fn give(self, input: VMInt) {
        debug_assert!(self.0.is_none(), "The input slot should be empty");
        *self.0 = Some(input)
    }
}
#[derive(Debug)]
pub struct HasOutput<'vm>(&'vm mut Option<VMInt>);
impl HasOutput<'_> {
    pub fn get(self) -> VMInt {
        self.0.take().expect("The output slot should be filled")
    }
}

/// Possible errors during execution
#[derive(Debug, PartialEq, Eq, Error)]
pub enum RuntimeError {
    #[error("{0} is not a valid opcode")]
    InvalidOpcode(u8),
    #[error("{0} is not a valid param mode")]
    InvalidParamMode(u8),
    #[error("Tried to write to immediate param")]
    WritingToImmediate,
    #[error("Tried to read from negative index")]
    ReadingFromNegative,
    #[error("Tried to write to negative index")]
    WritingToNegative,
    #[error("Tried to jump to negative index")]
    JumpToNegative,
    #[error("An instruction began with a negative value")]
    NegativeInstruction,
    #[error("An integer overflow happened during add")]
    OverflowInAdd,
    #[error("An integer overflow happened during mul")]
    OverflowInMul,
    #[error("An integer overflow happened to rb")]
    OverflowInRb,
    #[error("An integer overflow happened to pc during a jump")]
    OverflowInJump,
}

/// All the intcode memory
pub struct Memory {
    memory: Vec<VMInt>,

    pc: usize,
    rb: isize,
}
impl Memory {
    /// Read from the memory
    pub fn read(&self, pos: usize) -> VMInt {
        if pos < self.memory.len() {
            self.memory[pos] // return from the memory
        } else {
            0 // simulate infinite 0 memory
        }
    }
    /// Write to the memory
    pub fn write(&mut self, pos: usize, value: VMInt) {
        match (pos >= self.memory.len(), value == 0) {
            (true, true) => (), // nothing to do, setting 0 to after the last value is unusefule
            (true, false) => {
                self.memory.resize(pos + 1, 0);
                self.memory[pos] = value;
            }
            (false, _) => self.memory[pos] = value,
        }
    }

    /// Read from a param
    fn read_param(&self, param: Param) -> Result<VMInt, RuntimeError> {
        match param.0 {
            ParamMode::Position => {
                if param.1 >= 0 {
                    Ok(self.read(param.1 as usize))
                } else {
                    Err(RuntimeError::ReadingFromNegative)
                }
            }
            ParamMode::Immediate => Ok(param.1),
            ParamMode::Relative => {
                if (param.1 + self.rb as VMInt) >= 0 {
                    Ok(self.read((param.1 + self.rb as VMInt) as usize))
                } else {
                    Err(RuntimeError::ReadingFromNegative)
                }
            }
        }
    }
    /// Write to a param
    fn write_param(&mut self, param: Param, value: VMInt) -> Result<(), RuntimeError> {
        match param.0 {
            ParamMode::Position => {
                if param.1 >= 0 {
                    self.write(param.1 as usize, value);
                    Ok(())
                } else {
                    Err(RuntimeError::WritingToNegative)
                }
            }
            ParamMode::Immediate => Err(RuntimeError::WritingToImmediate),
            ParamMode::Relative => {
                if (param.1 + self.rb as VMInt) >= 0 {
                    self.write((param.1 + self.rb as VMInt) as usize, value);
                    Ok(())
                } else {
                    Err(RuntimeError::WritingToNegative)
                }
            }
        }
    }
}

/// A intcode virtual machine
pub struct VM {
    memory: Memory,

    output: Option<VMInt>,
    input: Option<VMInt>,
}

impl VM {
    pub fn new(code: Vec<VMInt>) -> Self {
        Self {
            memory: Memory {
                memory: code,
                pc: 0,
                rb: 0,
            },
            output: None,
            input: None,
        }
    }

    pub fn run(&mut self) -> Result<StopState, RuntimeError> {
        let stop_state = loop {
            if let StateBase::Stopped(stop_state) = step_impl(self)? {
                break stop_state;
            }
        };
        Ok(self.add_references(stop_state))
    }

    pub fn step(&mut self) -> Result<State, RuntimeError> {
        Ok(match step_impl(self)? {
            StateBase::Stopped(stop_state) => State::Stopped(self.add_references(stop_state)),
            StateBase::Running => State::Running,
        })
    }

    fn add_references(&mut self, stop_state: StopStateBase) -> StopState<'_> {
        match stop_state {
            StopStateBase::NeedInput => StopState::NeedInput(NeedInput(&mut self.input)),
            StopStateBase::HasOutput => StopState::HasOutput(HasOutput(&mut self.output)),
            StopStateBase::Halted => StopState::Halted,
        }
    }
}

// --- Implementation ---

/// Possible state in which a machine need interaction
#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq, Eq))]
enum StopStateBase {
    NeedInput,
    HasOutput,
    Halted,
}

/// Any state of a machine
#[derive(Debug)]
enum StateBase {
    Stopped(StopStateBase),
    Running,
}

fn step_impl(
    VM {
        memory: mem,
        output,
        input,
    }: &mut VM,
) -> Result<StateBase, RuntimeError> {
    if output.is_some() {
        // do not do anything if output is full
        return Ok(StateBase::Stopped(StopStateBase::HasOutput));
    }
    // -- Reading operation

    let mut op = mem.read(mem.pc);
    // extract opcode
    let opcode = Opcode::from(op)?;
    op /= 100;
    // extract params
    let params = {
        let mut params: [Param; 3] = [(ParamMode::Position, 0); 3];
        for i in 0..(opcode.param_num() as usize) {
            params[i] = (ParamMode::from(op % 10)?, mem.read(mem.pc + 1 + i));
            op /= 10;
        }
        params
    };

    // execute instruction
    Ok(match opcode {
        Opcode::ADD => {
            let a = mem.read_param(params[0])?;
            let b = mem.read_param(params[1])?;
            let res = VMInt::checked_add(a, b).ok_or(RuntimeError::OverflowInAdd)?;
            mem.write_param(params[2], res)?;
            mem.pc += 1 + opcode.param_num() as usize;
            StateBase::Running
        }
        Opcode::MUL => {
            let a = mem.read_param(params[0])?;
            let b = mem.read_param(params[1])?;
            let res = VMInt::checked_mul(a, b).ok_or(RuntimeError::OverflowInMul)?;
            mem.write_param(params[2], res)?;
            mem.pc += 1 + opcode.param_num() as usize;
            StateBase::Running
        }

        Opcode::IN => match input.take() {
            // read a value
            Some(val) => {
                mem.write_param(params[0], val)?;
                mem.pc += 1 + opcode.param_num() as usize;
                StateBase::Running
            }
            None => StateBase::Stopped(StopStateBase::NeedInput),
        },
        Opcode::OUT => {
            let val = mem.read_param(params[0])?;
            debug_assert!(output.is_none()); // ensure we do not lose output
            *output = Some(val);
            mem.pc += 1 + opcode.param_num() as usize;
            StateBase::Stopped(StopStateBase::HasOutput)
        }

        Opcode::JZ => {
            if mem.read_param(params[0])? == 0 {
                // jump
                let pos = mem.read_param(params[1])?;
                if pos >= 0 {
                    mem.pc = usize::try_from(pos).map_err(|_| RuntimeError::OverflowInJump)?;
                } else {
                    return Err(RuntimeError::JumpToNegative);
                }
            } else {
                // do not jump
                mem.pc += 1 + opcode.param_num() as usize;
            }
            StateBase::Running
        }
        Opcode::JNZ => {
            if mem.read_param(params[0])? != 0 {
                // jump
                let pos = mem.read_param(params[1])?;
                if pos >= 0 {
                    mem.pc = usize::try_from(pos).map_err(|_| RuntimeError::OverflowInJump)?;
                } else {
                    return Err(RuntimeError::JumpToNegative);
                }
            } else {
                // do not jump
                mem.pc += 1 + opcode.param_num() as usize;
            }
            StateBase::Running
        }

        Opcode::SLT => {
            let a = mem.read_param(params[0])?;
            let b = mem.read_param(params[1])?;
            let res = if a < b { 1 } else { 0 };
            mem.write_param(params[2], res)?;
            mem.pc += 1 + opcode.param_num() as usize;
            StateBase::Running
        }
        Opcode::SEQ => {
            let a = mem.read_param(params[0])?;
            let b = mem.read_param(params[1])?;
            let res = if a == b { 1 } else { 0 };
            mem.write_param(params[2], res)?;
            mem.pc += 1 + opcode.param_num() as usize;
            StateBase::Running
        }

        Opcode::INCB => {
            let val = mem.read_param(params[0])?;
            mem.rb = mem
                .rb
                .checked_add(isize::try_from(val).map_err(|_| RuntimeError::OverflowInRb)?)
                .ok_or(RuntimeError::OverflowInRb)?;
            mem.pc += 1 + opcode.param_num() as usize;
            StateBase::Running
        }

        Opcode::HALT => StateBase::Stopped(StopStateBase::Halted),
    })
}

// --- Parameters and instructions

#[derive(Copy, Clone)]
enum ParamMode {
    Position,
    Immediate,
    Relative,
}

impl ParamMode {
    fn from(value: VMInt) -> Result<ParamMode, RuntimeError> {
        match value {
            0 => Ok(Self::Position),
            1 => Ok(Self::Immediate),
            2 => Ok(Self::Relative),
            _ => Err(RuntimeError::InvalidParamMode(value as u8)),
        }
    }
}

#[derive(Copy, Clone)]
enum Opcode {
    ADD,
    MUL,
    IN,
    OUT,
    JZ,
    JNZ,
    SLT,
    SEQ,
    INCB,
    HALT,
}

impl Opcode {
    fn from(value: VMInt) -> Result<Opcode, RuntimeError> {
        match value % 100 {
            1 => Ok(Self::ADD),
            2 => Ok(Self::MUL),
            3 => Ok(Self::IN),
            4 => Ok(Self::OUT),
            5 => Ok(Self::JNZ),
            6 => Ok(Self::JZ),
            7 => Ok(Self::SLT),
            8 => Ok(Self::SEQ),
            9 => Ok(Self::INCB),
            99 => Ok(Self::HALT),
            v if v < 0 => Err(RuntimeError::NegativeInstruction),
            v => Err(RuntimeError::InvalidOpcode(v as u8)),
        }
    }

    fn param_num(&self) -> usize {
        match self {
            Self::ADD => 3,
            Self::MUL => 3,
            Self::IN => 1,
            Self::OUT => 1,
            Self::JZ => 2,
            Self::JNZ => 2,
            Self::SLT => 3,
            Self::SEQ => 3,
            Self::INCB => 1,
            Self::HALT => 0,
        }
    }
}

type Param = (ParamMode, VMInt);

#[cfg(test)]
mod tests {
    use super::*;

    fn _run_test(
        code: &[VMInt],
        mut input: &[VMInt],
        expected_output: &[VMInt],
        expected_end_state: Result<StopStateBase, RuntimeError>,
    ) {
        // programming machine
        let mut machine = VM::new(Vec::from(code));
        let mut output = vec![];
        let end_state = loop {
            match machine.run() {
                Ok(StopState::NeedInput(need_input)) => {
                    let Some((next_input, rest)) = input.split_first() else {
                        break Ok(StopStateBase::NeedInput);
                    };
                    input = rest;
                    need_input.give(*next_input)
                }
                Ok(StopState::HasOutput(has_output)) => output.push(has_output.get()),
                Ok(StopState::Halted) => break Ok(StopStateBase::Halted),
                Err(err) => break Err(err),
            }
        };
        assert_eq!(
            end_state, expected_end_state,
            "The vm stopped on {end_state:?} instead of {expected_end_state:?}"
        );
        assert_eq!(&output, expected_output, "The vm gave the wrong output")
    }

    macro_rules! tests {
        ($($name:ident [$($progv:literal),*] {$($io_name:ident: [$($inpv:literal),*] => [$($outv:literal),*] $(, $stop_state:expr)?;)+};)*) => {
            $(
                mod $name {
                    use super::*;

                    const _PROGRAM: &[VMInt] = &[$($progv ,)*];
                    $(
                        #[test]
                        fn $io_name() {
                            let _stop_state = Result::<StopStateBase, RuntimeError>::Ok(StopStateBase::Halted);
                            $(
                            let  _stop_state = $stop_state;
                            )?
                            _run_test(_PROGRAM, &[$($inpv),*], &[$($outv),*], _stop_state)
                        }
                    )*
                }
            )*
        }
    }

    tests! {
        empty [99] {
            no_input: [] => [];
            input: [1] => [];
            more_input: [1,2,3] => [];
        };
        hello [
            4, 3,
            101, 72, 14, 3,
            101, 1, 4, 4,
            5, 3, 16,
            99,
            29, 7, 0, 3, -67, -12, 87, -8, 3, -6, -8, -67, -23, -10
        ] {
            no_input: [] => [72, 101, 108, 108, 111, 44, 32, 119, 111, 114, 108, 100, 33, 10];
        };
        count [
            4,15,
            1001,15,1,15,
            8,15,16,14,
            1006,14,0,
            99,
            -1,1,11
        ] {
            no_input: [] => [1,2,3,4,5,6,7,8,9,10];
        };
        cat [
            3, 3,
            104, -1,
            1005, 3, 0,
            99
        ] {
            shortest: [0] => [0];
            longer: [1,2,3,0] => [1,2,3,0];
            unterminated: [1,2,3] => [1,2,3], Ok(StopStateBase::NeedInput);
        };
        test_add [
            1101, 4, 3, 5,
            104, -1,
            99
        ] {
            no_input: [] => [7];
        };
        test_mul [
            1102, 4, 3, 5,
            104, -1,
            99
        ] {
            no_input: [] => [12];
        };
        test_invalid_op [42] {
            no_input: [] => [], Err(RuntimeError::InvalidOpcode(42));
        };
        test_invalid_param [501] {
            no_input: [] => [], Err(RuntimeError::InvalidParamMode(5));
        };
        test_writing_to_immediate [11101,1,1,0] {
            no_input: [] => [], Err(RuntimeError::WritingToImmediate);
        };
        test_writing_to_negative [1101,1,1,-2] {
            no_input: [] => [], Err(RuntimeError::WritingToNegative);
        };
        test_reading_from_negative [1001,-2,1,5] {
            no_input: [] => [], Err(RuntimeError::ReadingFromNegative);
        };
        test_jump_to_negative [1106,0,-3] {
            no_input: [] => [], Err(RuntimeError::JumpToNegative);
        };
        day_9_boost_self_test [
            1102,34463338,34463338,63,1007,63,34463338,63,1005,63,53,1101,0,3,1000,109,988,209,12,
            9,1000,209,6,209,3,203,0,1008,1000,1,63,1005,63,65,1008,1000,2,63,1005,63,904,1008,1000,
            0,63,1005,63,58,4,25,104,0,99,4,0,104,0,99,4,17,104,0,99,0,0,1102,1,24,1017,1101,0,36,
            1006,1101,0,30,1011,1101,26,0,1018,1101,32,0,1015,1101,34,0,1004,1101,0,37,1002,1101,25,
            0,1012,1102,38,1,1010,1101,29,0,1019,1101,308,0,1029,1102,1,696,1027,1102,1,429,1022,
            1102,1,21,1005,1102,1,33,1013,1101,39,0,1008,1102,20,1,1009,1101,0,652,1025,1102,313,1,
            1028,1101,0,31,1003,1102,661,1,1024,1101,35,0,1016,1101,0,23,1000,1102,28,1,1014,1102,0,
            1,1020,1102,27,1,1007,1101,0,1,1021,1102,22,1,1001,1101,703,0,1026,1101,0,422,1023,109,-5,
            2101,0,9,63,1008,63,31,63,1005,63,205,1001,64,1,64,1105,1,207,4,187,1002,64,2,64,109,6,
            2102,1,3,63,1008,63,37,63,1005,63,227,1105,1,233,4,213,1001,64,1,64,1002,64,2,64,109,11,
            21108,40,40,3,1005,1015,255,4,239,1001,64,1,64,1106,0,255,1002,64,2,64,109,-3,21107,41,40,
            2,1005,1011,275,1001,64,1,64,1105,1,277,4,261,1002,64,2,64,109,4,2107,28,-6,63,1005,63,
            297,1001,64,1,64,1106,0,299,4,283,1002,64,2,64,109,15,2106,0,0,4,305,1106,0,317,1001,64,
            1,64,1002,64,2,64,109,-23,2108,22,4,63,1005,63,337,1001,64,1,64,1105,1,339,4,323,1002,64,
            2,64,109,6,21101,42,0,0,1008,1011,40,63,1005,63,363,1001,64,1,64,1105,1,365,4,345,1002,64,
            2,64,109,-17,1207,7,21,63,1005,63,381,1105,1,387,4,371,1001,64,1,64,1002,64,2,64,109,14,
            1201,-1,0,63,1008,63,25,63,1005,63,407,1105,1,413,4,393,1001,64,1,64,1002,64,2,64,109,15,
            2105,1,0,1001,64,1,64,1105,1,431,4,419,1002,64,2,64,109,-23,2101,0,6,63,1008,63,36,63,1005,
            63,453,4,437,1106,0,457,1001,64,1,64,1002,64,2,64,109,10,2108,21,-5,63,1005,63,475,4,463,
            1106,0,479,1001,64,1,64,1002,64,2,64,109,-3,1201,2,0,63,1008,63,20,63,1005,63,505,4,485,
            1001,64,1,64,1105,1,505,1002,64,2,64,109,4,2107,35,-5,63,1005,63,527,4,511,1001,64,1,64,
            1105,1,527,1002,64,2,64,109,15,1206,-5,543,1001,64,1,64,1105,1,545,4,533,1002,64,2,64,109,
            -8,1205,3,563,4,551,1001,64,1,64,1106,0,563,1002,64,2,64,109,-5,1206,7,581,4,569,1001,64,1,
            64,1105,1,581,1002,64,2,64,109,-8,1207,-3,38,63,1005,63,599,4,587,1105,1,603,1001,64,1,64,
            1002,64,2,64,109,19,1205,-4,619,1001,64,1,64,1105,1,621,4,609,1002,64,2,64,109,-13,1208,-4,
            27,63,1005,63,639,4,627,1105,1,643,1001,64,1,64,1002,64,2,64,109,5,2105,1,8,4,649,1001,64,
            1,64,1106,0,661,1002,64,2,64,109,-16,1202,4,1,63,1008,63,34,63,1005,63,683,4,667,1106,0,687,
            1001,64,1,64,1002,64,2,64,109,26,2106,0,1,1001,64,1,64,1105,1,705,4,693,1002,64,2,64,109,
            -9,21102,43,1,-7,1008,1010,46,63,1005,63,725,1105,1,731,4,711,1001,64,1,64,1002,64,2,64,109,
            -26,1202,9,1,63,1008,63,26,63,1005,63,755,1001,64,1,64,1105,1,757,4,737,1002,64,2,64,109,34,
            21108,44,43,-8,1005,1017,773,1106,0,779,4,763,1001,64,1,64,1002,64,2,64,109,-15,21102,45,1,1,
            1008,1011,45,63,1005,63,801,4,785,1106,0,805,1001,64,1,64,1002,64,2,64,109,-14,1208,10,35,63,
            1005,63,821,1106,0,827,4,811,1001,64,1,64,1002,64,2,64,109,17,2102,1,-4,63,1008,63,20,63,1005,
            63,853,4,833,1001,64,1,64,1106,0,853,1002,64,2,64,109,6,21107,46,47,-4,1005,1015,871,4,859,
            1105,1,875,1001,64,1,64,1002,64,2,64,109,-10,21101,47,0,4,1008,1013,47,63,1005,63,901,4,881,
            1001,64,1,64,1105,1,901,4,64,99,21102,27,1,1,21102,1,915,0,1106,0,922,21201,1,37790,1,204,1,
            99,109,3,1207,-2,3,63,1005,63,964,21201,-2,-1,1,21102,1,942,0,1106,0,922,22102,1,1,-1,21201,
            -2,-3,1,21102,957,1,0,1105,1,922,22201,1,-1,-2,1105,1,968,21201,-2,0,-2,109,-3,2105,1,0
        ] {
            part_1: [1] => [2671328082];
            part_2: [2] => [59095];
        };
    }
}
