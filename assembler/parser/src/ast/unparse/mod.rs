use super::*;

use std::fmt::Write;

pub struct DisplayUnparsed<'a, T: ?Sized>(&'a T);
impl<T: Unparse> Display for DisplayUnparsed<'_, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.unparse(f)
    }
}

pub trait Unparse {
    fn unparse(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result;
    fn display(&self) -> DisplayUnparsed<Self> {
        DisplayUnparsed(self)
    }
}

trait NeedComma<Before> {
    fn need_comma(&self, before: &Before) -> bool;
}

impl Unparse for File<'_> {
    fn unparse(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.statements.unparse(f)
    }
}
impl Unparse for Vec<Labelled<'_, Option<Statement<'_>>>> {
    fn unparse(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for s in self {
            s.unparse(f)?;
            writeln!(f)?;
        }
        Ok(())
    }
}

impl<T: Unparse> Unparse for Labelled<'_, T> {
    fn unparse(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for lbl in &self.labels {
            lbl.unparse(f)?
        }
        self.content.unparse(f)
    }
}
impl Unparse for LabelDef<'_> {
    fn unparse(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.label.unparse(f)?;
        f.write_str(": ")
    }
}
impl<T, B> NeedComma<Labelled<'_, B>> for T
where
    T: NeedComma<B>,
{
    fn need_comma(&self, before: &Labelled<'_, B>) -> bool {
        self.need_comma(&before.content)
    }
}

impl<T: Unparse> Unparse for Option<T> {
    fn unparse(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(this) = self.as_ref() {
            this.unparse(f)
        } else {
            Ok(())
        }
    }
}

impl<T: Unparse> Unparse for Box<T> {
    fn unparse(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Box::as_ref(self).unparse(f)
    }
}

impl Unparse for StringLit<'_> {
    fn unparse(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "\"{}\"", self.content)
    }
}

impl Unparse for LabelRef<'_> {
    fn unparse(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LabelRef::Identifier(ident) => ident.unparse(f),
            LabelRef::SpecialIdentifier(sident) => sident.unparse(f),
            LabelRef::Error(e) => <!>::from(*e),
        }
    }
}

impl Unparse for Identifier<'_> {
    fn unparse(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Identifier::Unnamed(n, ..) => write!(f, "${n}"),
            Identifier::Named(n, ..) => write!(f, "{n}"),
        }
    }
}
impl Unparse for SpecialIdentifier {
    fn unparse(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            SpecialIdentifier::Start => "$start",
            SpecialIdentifier::End => "$end",
            SpecialIdentifier::UnitStart => "$unit_start",
            SpecialIdentifier::UnitEnd => "$unit_end",
        })
    }
}

macro_rules! unparse_for_statement {
    (
        $($variant:ident)*
    ) => {
        impl Unparse for Statement<'_> {
            fn unparse(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    $(
                    Statement::$variant(stm) => Unparse::unparse(stm, f),
                    )*
                    Statement::Error(e) => <!>::from(*e)
                }
            }

        }
    };
}

unparse_for_statement! {
    Ints Zeros Instruction Inc Dec Jmp Mov Load Store Call Ret Export Entry
}

impl Unparse for Instruction<'_> {
    fn unparse(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Instruction::Add(a, b, c) => {
                f.write_str("ADD ")?;
                a.unparse(f)?;
                if b.need_comma(a) {
                    f.write_char(',')?;
                }
                f.write_char(' ')?;
                b.unparse(f)?;
                if c.need_comma(b) {
                    f.write_char(',')?;
                }
                f.write_char(' ')?;
                c.unparse(f)?;
                Ok(())
            }
            Instruction::Mul(a, b, c) => {
                f.write_str("MUL ")?;
                a.unparse(f)?;
                if b.need_comma(a) {
                    f.write_char(',')?;
                }
                f.write_char(' ')?;
                b.unparse(f)?;
                if c.need_comma(b) {
                    f.write_char(',')?;
                }
                f.write_char(' ')?;
                c.unparse(f)?;
                Ok(())
            }
            Instruction::In(a) => {
                f.write_str("IN ")?;
                a.unparse(f)?;
                Ok(())
            }
            Instruction::Out(a) => {
                f.write_str("OUT ")?;
                a.unparse(f)?;
                Ok(())
            }
            Instruction::Jnz(a, b) => {
                f.write_str("JNZ ")?;
                a.unparse(f)?;
                if b.need_comma(a) {
                    f.write_char(',')?;
                }
                f.write_char(' ')?;
                b.unparse(f)?;
                Ok(())
            }
            Instruction::Jz(a, b) => {
                f.write_str("JZ ")?;
                a.unparse(f)?;
                if b.need_comma(a) {
                    f.write_char(',')?;
                }
                f.write_char(' ')?;
                b.unparse(f)?;
                Ok(())
            }
            Instruction::Slt(a, b, c) => {
                f.write_str("SLT ")?;
                a.unparse(f)?;
                if b.need_comma(a) {
                    f.write_char(',')?;
                }
                f.write_char(' ')?;
                b.unparse(f)?;
                if c.need_comma(b) {
                    f.write_char(',')?;
                }
                f.write_char(' ')?;
                c.unparse(f)?;
                Ok(())
            }
            Instruction::Seq(a, b, c) => {
                f.write_str("SEQ ")?;
                a.unparse(f)?;
                if b.need_comma(a) {
                    f.write_char(',')?;
                }
                f.write_char(' ')?;
                b.unparse(f)?;
                if c.need_comma(b) {
                    f.write_char(',')?;
                }
                f.write_char(' ')?;
                c.unparse(f)?;
                Ok(())
            }
            Instruction::Incb(a) => {
                f.write_str("INCB ")?;
                a.unparse(f)?;
                Ok(())
            }
            Instruction::Halt => f.write_str("HALT"),
            Instruction::Error(e) => <!>::from(*e),
        }
    }
}

mod params;

mod statements;

mod expression;

#[cfg(test)]
mod tests {
    use super::*;

    use test_sources::test_sources;

    #[cfg(feature = "parse")]
    #[test_sources]
    fn sources(source: &str) {
        use errors::{PanicAccumulator, RootAccumulator};

        let ast = crate::parse(source, &mut PanicAccumulator::<ParseError>::new()).unwrap();
        let unparsed = ast.display().to_string();

        let mut errs = RootAccumulator::<ParseError>::new();
        let reparsed = crate::parse(&unparsed, &mut errs);
        match errs.finish(|| reparsed.unwrap()) {
            Err(errs) => {
                for err in errs {
                    eprintln!("{err}");
                    panic!("Unparsed ast did not reparse")
                }
            }
            Ok(reparsed) => {
                assert_eq!(ast, reparsed, "The unparsed ast did not reparse to himself")
            }
        }
    }
}
