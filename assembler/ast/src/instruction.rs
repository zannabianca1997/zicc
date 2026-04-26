use std::fmt;

use zicc_display::DisplayWith;

use crate::{expression::Expr, labelled::Labelled};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Instruction(pub zicc_intcode::Instruction<Labelled<Expr>>);

impl DisplayWith for Instruction {
    fn fmt_with(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        interner: &string_interner::DefaultStringInterner,
    ) -> std::fmt::Result {
        match &self.0 {
            zicc_intcode::Instruction::Add(a, b, c) => {
                write!(f, "ADD ")?;
                print_read(a.0, &a.1, f, interner)?;
                write!(f, " ")?;
                print_read(b.0, &b.1, f, interner)?;
                write!(f, " ")?;
                print_write(c.0, &c.1, f, interner)?;
            }
            zicc_intcode::Instruction::Mul(a, b, c) => {
                write!(f, "MUL ")?;
                print_read(a.0, &a.1, f, interner)?;
                write!(f, " ")?;
                print_read(b.0, &b.1, f, interner)?;
                write!(f, " ")?;
                print_write(c.0, &c.1, f, interner)?;
            }
            zicc_intcode::Instruction::Inp(a) => {
                write!(f, "INP ")?;
                print_write(a.0, &a.1, f, interner)?;
            }
            zicc_intcode::Instruction::Out(a) => {
                write!(f, "OUT ")?;
                print_read(a.0, &a.1, f, interner)?;
            }
            zicc_intcode::Instruction::Jnz(a, b) => {
                write!(f, "JNZ ")?;
                print_read(a.0, &a.1, f, interner)?;
                write!(f, " ")?;
                print_read(b.0, &b.1, f, interner)?;
            }
            zicc_intcode::Instruction::Jez(a, b) => {
                write!(f, "JEZ ")?;
                print_read(a.0, &a.1, f, interner)?;
                write!(f, " ")?;
                print_read(b.0, &b.1, f, interner)?;
            }
            zicc_intcode::Instruction::Slt(a, b, c) => {
                write!(f, "SLT ")?;
                print_read(a.0, &a.1, f, interner)?;
                write!(f, " ")?;
                print_read(b.0, &b.1, f, interner)?;
                write!(f, " ")?;
                print_write(c.0, &c.1, f, interner)?;
            }
            zicc_intcode::Instruction::Seq(a, b, c) => {
                write!(f, "SEQ ")?;
                print_read(a.0, &a.1, f, interner)?;
                write!(f, " ")?;
                print_read(b.0, &b.1, f, interner)?;
                write!(f, " ")?;
                print_write(c.0, &c.1, f, interner)?;
            }
            zicc_intcode::Instruction::Inb(a) => {
                write!(f, "INB ")?;
                print_read(a.0, &a.1, f, interner)?;
            }
            zicc_intcode::Instruction::Hlt => write!(f, "HLT")?,
        };
        Ok(())
    }
}

fn print_read(
    mode: zicc_intcode::ReadParamMode,
    expr: &Labelled<Expr>,
    f: &mut std::fmt::Formatter<'_>,
    interner: &string_interner::StringInterner<string_interner::backend::StringBackend>,
) -> fmt::Result {
    match mode {
        zicc_intcode::ReadParamMode::Absolute => (),
        zicc_intcode::ReadParamMode::Immediate => write!(f, "#")?,
        zicc_intcode::ReadParamMode::Relative => write!(f, "@")?,
    };

    expr.fmt_with(f, interner)
}
fn print_write(
    mode: zicc_intcode::WriteParamMode,
    expr: &Labelled<Expr>,
    f: &mut std::fmt::Formatter<'_>,
    interner: &string_interner::StringInterner<string_interner::backend::StringBackend>,
) -> fmt::Result {
    print_read(mode.into(), expr, f, interner)
}
