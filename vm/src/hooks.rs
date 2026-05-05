//! Hooks called during runtime

use std::ops::Range;

use zicc_intcode::Instruction;

use crate::mem::Memory;

pub trait Hooks {
    fn before_instruction(
        &mut self,
        _instruction: &Instruction<usize>,
        _pos: Range<usize>,
        _memory: &Memory,
    ) {
    }
}

impl<T: Hooks> Hooks for &mut T {
    fn before_instruction(
        &mut self,
        instruction: &Instruction<usize>,
        pos: Range<usize>,
        memory: &Memory,
    ) {
        T::before_instruction(self, instruction, pos, memory);
    }
}

impl Hooks for () {}

impl<A> Hooks for (A,)
where
    A: Hooks,
{
    fn before_instruction(
        &mut self,
        instruction: &Instruction<usize>,
        pos: Range<usize>,
        memory: &Memory,
    ) {
        self.0.before_instruction(instruction, pos, memory);
    }
}
impl<A, B> Hooks for (A, B)
where
    A: Hooks,
    B: Hooks,
{
    fn before_instruction(
        &mut self,
        instruction: &Instruction<usize>,
        pos: Range<usize>,
        memory: &Memory,
    ) {
        self.0.before_instruction(instruction, pos.clone(), memory);
        self.1.before_instruction(instruction, pos, memory);
    }
}

impl<T: Hooks> Hooks for [T] {
    fn before_instruction(
        &mut self,
        instruction: &Instruction<usize>,
        pos: Range<usize>,
        memory: &Memory,
    ) {
        for h in self {
            h.before_instruction(instruction, pos.clone(), memory);
        }
    }
}

impl<T: Hooks, const N: usize> Hooks for [T; N] {
    fn before_instruction(
        &mut self,
        instruction: &Instruction<usize>,
        pos: Range<usize>,
        memory: &Memory,
    ) {
        self.as_mut_slice().before_instruction(instruction, pos, memory);
    }
}

impl<T: Hooks> Hooks for Option<T> {
    fn before_instruction(
        &mut self,
        instruction: &Instruction<usize>,
        pos: Range<usize>,
        memory: &Memory,
    ) {
        if let Some(h) = self {
            h.before_instruction(instruction, pos, memory);
        }
    }
}
