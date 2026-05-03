use zicc_compiler_lexer::int_literal::IntLiteral;
use zicc_display::DisplayWith;
use zicc_intcode::{ReadParamMode, WriteParamMode};

use crate::{expression::Expr, labelled::Labelled};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Directive {
    /// List of value to copy verbatim
    Data(Vec<Labelled<Expr>>),
    /// Specific number of zeros
    ///
    /// Not labelled as this is not a memory position
    Zeros(IntLiteral),
    /// Unconditional jump: `JMP {a}` → `JEQ #0 {a}`
    Jmp((ReadParamMode, Labelled<Expr>)),
    /// Increment by 1: `INC {a}` → `ADD {a} #1 {a}`
    Inc((WriteParamMode, Expr)),
    /// Decrement by 1: `DEC {a}` → `ADD {a} #-1 {a}`
    Dec((WriteParamMode, Expr)),
    /// Copy value: `MOV {a} {b}` → `ADD {a} #0 {b}`
    Mov(
        (ReadParamMode, Labelled<Expr>),
        (WriteParamMode, Labelled<Expr>),
    ),
    /// Push to stack: `PUSH {a}` → `INB #1; MOV {a} @-1`
    Push((ReadParamMode, Labelled<Expr>)),
    /// Pop from stack: `POP [{a}]` → `INB #-1; [MOV @0 {a}]`
    Pop(Option<(WriteParamMode, Labelled<Expr>)>),
    /// Call procedure: `CALL {a}` → `PUSH # $1; JMP {a}; $1: POP`
    Call((ReadParamMode, Labelled<Expr>)),
    /// Return from procedure: `RET` → `JMP @-1`
    Ret,
    /// Load from pointer: `LOAD {ptr} {dest}` → `MOV {ptr} $1; MOV $1:0 {dest}`
    Load(
        (ReadParamMode, Labelled<Expr>),
        (WriteParamMode, Labelled<Expr>),
    ),
    /// Store to pointer: `STORE {src} {ptr}` → `MOV {ptr} $1; MOV {src} $1:0`
    Store(
        (ReadParamMode, Labelled<Expr>),
        (ReadParamMode, Labelled<Expr>),
    ),
}

fn fmt_param<M: Into<ReadParamMode> + Copy>(
    mode: M,
    expr: &Labelled<Expr>,
    f: &mut std::fmt::Formatter<'_>,
    interner: &string_interner::DefaultStringInterner,
) -> std::fmt::Result {
    match mode.into() {
        ReadParamMode::Absolute => (),
        ReadParamMode::Immediate => write!(f, "#")?,
        ReadParamMode::Relative => write!(f, "@")?,
    };
    expr.fmt_with(f, interner)
}

impl DisplayWith for Directive {
    fn fmt_with(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        interner: &string_interner::DefaultStringInterner,
    ) -> std::fmt::Result {
        match self {
            Directive::Data(data) => {
                write!(f, "DATA")?;
                for value in data {
                    write!(f, " ")?;
                    value.fmt_with(f, interner)?;
                }
            }
            Directive::Zeros(len) => write!(f, "ZEROS {len}")?,
            Directive::Jmp((mode, target)) => {
                write!(f, "JMP ")?;
                fmt_param(*mode, target, f, interner)?;
            }
            Directive::Inc((mode, target)) => {
                write!(f, "INC ")?;
                fmt_param(*mode, &Labelled::unlabelled(target.clone()), f, interner)?;
            }
            Directive::Dec((mode, target)) => {
                write!(f, "DEC ")?;
                fmt_param(*mode, &Labelled::unlabelled(target.clone()), f, interner)?;
            }
            Directive::Mov((src_mode, src), (dst_mode, dst)) => {
                write!(f, "MOV ")?;
                fmt_param(*src_mode, src, f, interner)?;
                write!(f, " ")?;
                fmt_param(*dst_mode, dst, f, interner)?;
            }
            Directive::Push((mode, value)) => {
                write!(f, "PUSH ")?;
                fmt_param(*mode, value, f, interner)?;
            }
            Directive::Pop(dst) => {
                write!(f, "POP")?;
                if let Some((mode, dst)) = dst {
                    write!(f, " ")?;
                    fmt_param(*mode, dst, f, interner)?;
                }
            }
            Directive::Call((mode, target)) => {
                write!(f, "CALL ")?;
                fmt_param(*mode, target, f, interner)?;
            }
            Directive::Ret => write!(f, "RET")?,
            Directive::Load((ptr_mode, ptr), (dst_mode, dst)) => {
                write!(f, "LOAD ")?;
                fmt_param(*ptr_mode, ptr, f, interner)?;
                write!(f, " ")?;
                fmt_param(*dst_mode, dst, f, interner)?;
            }
            Directive::Store((src_mode, src), (ptr_mode, ptr)) => {
                write!(f, "STORE ")?;
                fmt_param(*src_mode, src, f, interner)?;
                write!(f, " ")?;
                fmt_param(*ptr_mode, ptr, f, interner)?;
            }
        };

        Ok(())
    }
}
