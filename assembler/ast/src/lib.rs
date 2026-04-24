use crate::labelled::Labelled;

pub mod directive;
pub mod expression;
pub mod identifier;
pub mod instruction;
pub mod labelled;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Line {
    /// Instruction to copy verbatim
    Instruction(instruction::Instruction),
    /// Directive to convert before emitting
    Directive(directive::Directive),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct File {
    pub lines: Vec<Labelled<Line>>,
    pub trailing: Labelled<()>,
}
