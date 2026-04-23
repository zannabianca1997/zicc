use crate::labelled::Labelled;

pub mod expression;
pub mod identifier;
pub mod labelled;
pub mod instruction {
    use crate::expression::Expr;

    pub type Instruction = zicc_intcode::Instruction<Expr>;
}
pub mod directive {
    use crate::{expression::Expr, labelled::Labelled};

    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub enum Directive {
        /// List of value to copy verbatim
        Values(Vec<Labelled<Expr>>),
    }
}

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
