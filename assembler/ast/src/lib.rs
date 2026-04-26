use zicc_display::DisplayWith;

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

impl DisplayWith for Line {
    fn fmt_with(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        interner: &string_interner::DefaultStringInterner,
    ) -> std::fmt::Result {
        match self {
            Line::Instruction(instruction) => instruction.fmt_with(f, interner),
            Line::Directive(directive) => directive.fmt_with(f, interner),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct File {
    pub lines: Vec<Labelled<Line>>,
    pub trailing: Labelled<()>,
}

impl DisplayWith for File {
    fn fmt_with(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        interner: &string_interner::DefaultStringInterner,
    ) -> std::fmt::Result {
        for line in &self.lines {
            line.fmt_with(f, interner)?;
            writeln!(f)?;
        }

        if self.trailing.is_labelled() {
            self.trailing.fmt_labels_with(f, interner)?;
            writeln!(f)?;
        }

        Ok(())
    }
}
