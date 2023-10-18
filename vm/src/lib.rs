//! Implementation of a intcode machine
//!
//! This is kept indipendent from the rest of the project, so it can be substituted with any equivalent implementation

#![feature(never_type)]

use std::collections::VecDeque;

use thiserror::Error;

/// Possible state in which a machine need interaction
#[derive(Debug, PartialEq, Eq)]
pub enum ICMachineStopState {
    EmptyInput,
    RuntimeErr(ICRuntimeErr),
    Halted,
}

/// Possible errors during execution
#[derive(Debug, PartialEq, Eq, Error)]
pub enum ICRuntimeErr {
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
}

#[derive(Copy, Clone)]
enum ICParamMode {
    Position,
    Immediate,
    Relative,
}

impl ICParamMode {
    fn from(value: u8) -> Result<ICParamMode, ICRuntimeErr> {
        match value {
            0 => Ok(Self::Position),
            1 => Ok(Self::Immediate),
            2 => Ok(Self::Relative),
            _ => Err(ICRuntimeErr::InvalidParamMode(value)),
        }
    }
}

#[derive(Copy, Clone)]
enum ICOpCode {
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

impl ICOpCode {
    fn from(value: u8) -> Result<ICOpCode, ICRuntimeErr> {
        match value {
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
            _ => Err(ICRuntimeErr::InvalidOpcode(value)),
        }
    }

    fn param_num(&self) -> u8 {
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

/// Any state of a machine
pub enum ICMachineState {
    Stopped(ICMachineStopState),
    Running,
}

impl ICMachineState {
    /// shortcut to avoid boilerplate
    fn stop_for(err: ICRuntimeErr) -> Self {
        ICMachineState::Stopped(ICMachineStopState::RuntimeErr(err))
    }
}

/// A intcode machine
pub trait ICMachine {
    /// The int type this machine work with
    type IntType;
    /// The possible errors in giving input to this machine
    type InputErr;

    /// Create a new intcode machine
    fn new(program: &[Self::IntType]) -> Self;

    /// Give input to the machine
    fn give_input(&mut self, input: Self::IntType) -> Result<(), Self::InputErr>;
    /// Get output from the machine
    fn get_output(&mut self) -> Option<Self::IntType>;

    /// Run the machine until it need interaction
    fn run(&mut self) -> ICMachineStopState;
    /// Step the machine forward
    fn step(&mut self) -> ICMachineState;
}

// --- Implementation ---

pub type VMInt = i64;

pub struct ICMachineData {
    memory: Vec<VMInt>,

    pc: usize,
    rb: isize,

    inp_queue: VecDeque<VMInt>,
    out_queue: VecDeque<VMInt>,
}

type ICParam = (ICParamMode, VMInt);

impl ICMachineData {
    /// Read from the machine memory
    fn read_mem(&self, pos: usize) -> VMInt {
        if pos < self.memory.len() {
            self.memory[pos] // return from the memory
        } else {
            0 // simulate infinite 0 memory
        }
    }
    /// Write to the machine memory
    fn write_mem(&mut self, pos: usize, value: VMInt) {
        if pos >= self.memory.len() {
            self.memory.resize(pos + 1, 0);
        }
        self.memory[pos] = value;
    }
    /// Read from a param
    fn read_param(&self, param: ICParam) -> Result<VMInt, ICRuntimeErr> {
        match param.0 {
            ICParamMode::Position => {
                if param.1 >= 0 {
                    Ok(self.read_mem(param.1 as usize))
                } else {
                    Err(ICRuntimeErr::ReadingFromNegative)
                }
            }
            ICParamMode::Immediate => Ok(param.1),
            ICParamMode::Relative => {
                if (param.1 + self.rb as VMInt) >= 0 {
                    Ok(self.read_mem((param.1 + self.rb as VMInt) as usize))
                } else {
                    Err(ICRuntimeErr::ReadingFromNegative)
                }
            }
        }
    }
    /// Write to a param
    fn write_param(&mut self, param: ICParam, value: VMInt) -> Result<(), ICRuntimeErr> {
        match param.0 {
            ICParamMode::Position => {
                if param.1 >= 0 {
                    self.write_mem(param.1 as usize, value);
                    Ok(())
                } else {
                    Err(ICRuntimeErr::WritingToNegative)
                }
            }
            ICParamMode::Immediate => Err(ICRuntimeErr::WritingToImmediate),
            ICParamMode::Relative => {
                if (param.1 + self.rb as VMInt) >= 0 {
                    self.write_mem((param.1 + self.rb as VMInt) as usize, value);
                    Ok(())
                } else {
                    Err(ICRuntimeErr::WritingToNegative)
                }
            }
        }
    }
}

impl ICMachine for ICMachineData {
    type IntType = VMInt;
    type InputErr = !;

    fn new(program: &[Self::IntType]) -> ICMachineData {
        ICMachineData {
            memory: program.to_vec(),

            pc: 0,
            rb: 0,

            inp_queue: VecDeque::new(),
            out_queue: VecDeque::new(),
        }
    }

    fn give_input(&mut self, input: Self::IntType) -> Result<(), Self::InputErr> {
        self.inp_queue.push_back(input);
        Ok(())
    }
    fn get_output(&mut self) -> Option<Self::IntType> {
        self.out_queue.pop_front()
    }

    fn run(&mut self) -> ICMachineStopState {
        loop {
            match self.step() {
                ICMachineState::Running => (),
                ICMachineState::Stopped(a) => return a,
            }
        }
    }
    fn step(&mut self) -> ICMachineState {
        // -- Reading operation

        let mut op = self.read_mem(self.pc);
        // extract opcode
        let opcode = match ICOpCode::from((op % 100) as u8) {
            Ok(res) => res,
            Err(err) => return ICMachineState::stop_for(err),
        };
        op /= 100;
        // extract params
        let mut params: [ICParam; 3] = [(ICParamMode::Position, 0); 3];
        for i in 0..(opcode.param_num() as usize) {
            params[i] = (
                match ICParamMode::from((op % 10) as u8) {
                    Ok(res) => res,
                    Err(err) => return ICMachineState::stop_for(err),
                },
                self.read_mem(self.pc + 1 + i),
            );
            op /= 10;
        }
        let params = params;

        // execute instruction
        match opcode {
            ICOpCode::ADD => match self.read_param(params[0]).and_then(|a| {
                self.read_param(params[1])
                    .and_then(|b| self.write_param(params[2], a + b))
            }) {
                Ok(()) => {
                    self.pc += 1 + opcode.param_num() as usize;
                    ICMachineState::Running
                }
                Err(err) => ICMachineState::stop_for(err),
            },
            ICOpCode::MUL => match self.read_param(params[0]).and_then(|a| {
                self.read_param(params[1])
                    .and_then(|b| self.write_param(params[2], a * b))
            }) {
                Ok(()) => {
                    self.pc += 1 + opcode.param_num() as usize;
                    ICMachineState::Running
                }
                Err(err) => ICMachineState::stop_for(err),
            },

            ICOpCode::IN => match self.inp_queue.pop_front() {
                // read a value
                Some(val) => match self.write_param(params[0], val) {
                    Ok(()) => {
                        self.pc += 1 + opcode.param_num() as usize;
                        ICMachineState::Running
                    }
                    Err(err) => ICMachineState::stop_for(err),
                },
                None => ICMachineState::Stopped(ICMachineStopState::EmptyInput),
            },
            ICOpCode::OUT => match self.read_param(params[0]) {
                Ok(val) => {
                    self.out_queue.push_back(val);
                    self.pc += 1 + opcode.param_num() as usize;
                    ICMachineState::Running
                }
                Err(err) => ICMachineState::stop_for(err),
            },

            ICOpCode::JZ => match self.read_param(params[0]) {
                Ok(0) => {
                    // jump
                    match self.read_param(params[1]) {
                        Ok(pos) if pos < 0 => {
                            ICMachineState::stop_for(ICRuntimeErr::JumpToNegative)
                        }
                        Ok(pos) => {
                            self.pc = pos as usize;
                            ICMachineState::Running
                        }
                        Err(err) => ICMachineState::stop_for(err),
                    }
                }
                Ok(_) => {
                    // do not jump
                    self.pc += 1 + opcode.param_num() as usize;
                    ICMachineState::Running
                }
                Err(err) => ICMachineState::stop_for(err),
            },
            ICOpCode::JNZ => match self.read_param(params[0]) {
                Ok(0) => {
                    // do not jump
                    self.pc += 1 + opcode.param_num() as usize;
                    ICMachineState::Running
                }
                Ok(_) => {
                    // jump
                    match self.read_param(params[1]) {
                        Ok(pos) if pos < 0 => {
                            ICMachineState::stop_for(ICRuntimeErr::JumpToNegative)
                        }
                        Ok(pos) => {
                            self.pc = pos as usize;
                            ICMachineState::Running
                        }
                        Err(err) => ICMachineState::stop_for(err),
                    }
                }
                Err(err) => ICMachineState::stop_for(err),
            },

            ICOpCode::SLT => match self.read_param(params[0]).and_then(|a| {
                self.read_param(params[1])
                    .and_then(|b| self.write_param(params[2], if a < b { 1 } else { 0 }))
            }) {
                Ok(()) => {
                    self.pc += 1 + opcode.param_num() as usize;
                    ICMachineState::Running
                }
                Err(err) => ICMachineState::stop_for(err),
            },
            ICOpCode::SEQ => match self.read_param(params[0]).and_then(|a| {
                self.read_param(params[1])
                    .and_then(|b| self.write_param(params[2], if a == b { 1 } else { 0 }))
            }) {
                Ok(()) => {
                    self.pc += 1 + opcode.param_num() as usize;
                    ICMachineState::Running
                }
                Err(err) => ICMachineState::stop_for(err),
            },

            ICOpCode::INCB => match self.read_param(params[0]) {
                Ok(val) => {
                    self.rb += val as isize;
                    self.pc += 1 + opcode.param_num() as usize;
                    ICMachineState::Running
                }
                Err(err) => ICMachineState::stop_for(err),
            },
            ICOpCode::HALT => ICMachineState::Stopped(ICMachineStopState::Halted),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! ic_tests {
        ($($name:ident: [$($progv:literal ,)*], {$($io_name:ident: [$($inpv:literal ,)*] => $($stopstate:pat)? , [$($outv:literal ,)*];)+}; )*) => {
        $(
            mod $name {
                use super::*;

                const PROGRAM: &[VMInt] = &[$($progv ,)*];
                $(
                    #[test]
                    #[allow(unreachable_patterns)]
                    fn $io_name() {
                        // programming machine
                        let mut machine: ICMachineData = ICMachineData::new(PROGRAM);
                        // loading input
                        for v in &[$($inpv ,)*] {
                            assert_eq!(machine.give_input(*v), Ok(()));
                        }
                        // running machine
                        match machine.run() {
                            $(
                                $stopstate => (),
                                res => assert!{false, "Machine stopped on {:?} instead of {}", res, stringify!{$stopstate}},
                            )?
                            ICMachineStopState::Halted => (),
                            res => assert!{false, "Machine stopped on {:?} instead of {}", res, stringify!{ICMachineStopState::Halted}},
                        };
                        // unloading output
                        let mut output: Vec<VMInt> = vec![];
                        loop {
                            match machine.get_output() {
                                Some(v) => output.push(v),
                                None => break,
                            }
                        }
                        let output = output;
                        // comparing results
                        assert_eq!(output, &[$($outv ,)*]);
                    }
                )*
            }
        )*
        }
    }

    ic_tests!(
        empty: [99,], {
            no_input: [] => ,[];
            input: [1,] => ,[];
            more_input: [1,2,3,] => ,[];
        };
        hello: [
            4, 3,
            101, 72, 14, 3,
            101, 1, 4, 4,
            5, 3, 16,
            99,
            29, 7, 0, 3, -67, -12, 87, -8, 3, -6, -8, -67, -23, -10,
        ], {
            no_input: [] => ,[72, 101, 108, 108, 111, 44, 32, 119, 111, 114, 108, 100, 33, 10,];
        };
        count: [
            4,15,
            1001,15,1,15,
            8,15,16,14,
            1006,14,0,
            99,
            -1,1,11,
        ], {
            no_input: [] => ,[1,2,3,4,5,6,7,8,9,10,];
        };
        cat: [
            3, 3,
            104, -1,
            1005, 3, 0,
            99,
        ], {
            shortest: [0,] => ,[0,];
            longer: [1,2,3,0,] => ,[1,2,3,0,];
            unterminated: [1,2,3,] => ICMachineStopState::EmptyInput, [1,2,3,];
        };
        test_add: [
            1101, 4, 3, 5,
            104, -1,
            99,
        ], {
            no_input: [] => ,[7,];
        };
        test_mul: [
            1102, 4, 3, 5,
            104, -1,
            99,
        ], {
            no_input: [] => ,[12,];
        };
        test_invalid_op: [42,], {
            no_input: [] => ICMachineStopState::RuntimeErr(ICRuntimeErr::InvalidOpcode(42)),[];
        };
        test_invalid_param: [501,], {
            no_input: [] => ICMachineStopState::RuntimeErr(ICRuntimeErr::InvalidParamMode(5)),[];
        };
        test_writing_to_immediate: [11101,1,1,0,], {
            no_input: [] => ICMachineStopState::RuntimeErr(ICRuntimeErr::WritingToImmediate),[];
        };
        test_writing_to_negative: [1101,1,1,-2,], {
            no_input: [] => ICMachineStopState::RuntimeErr(ICRuntimeErr::WritingToNegative),[];
        };
        test_reading_from_negative: [1001,-2,1,5,], {
            no_input: [] => ICMachineStopState::RuntimeErr(ICRuntimeErr::ReadingFromNegative),[];
        };
        test_jump_to_negative: [1106,0,-3,], {
            no_input: [] => ICMachineStopState::RuntimeErr(ICRuntimeErr::JumpToNegative),[];
        };
        day_9_boost_self_test: [
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
            -2,-3,1,21102,957,1,0,1105,1,922,22201,1,-1,-2,1105,1,968,21201,-2,0,-2,109,-3,2105,1,0,
        ], {
            part_1: [1,] => , [2671328082, ];
            part_2: [2,] => , [59095,];
        };
    );
}
