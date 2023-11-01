use super::*;

impl Unparse for IntsStm<'_> {
    fn unparse(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("INTS")?;
        let Some((first, rest)) = self.values.split_first() else {
            return Ok(());
        };
        f.write_char(' ')?;
        first.unparse(f)?;
        let mut before = first;
        for p in rest {
            if p.need_comma(before) {
                f.write_char(',')?;
            }
            f.write_char(' ')?;
            p.unparse(f)?;
            before = p;
        }
        Ok(())
    }
}
impl Unparse for ZerosStm<'_> {
    fn unparse(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("ZEROS ")?;
        self.0.unparse(f)
    }
}

impl Unparse for IncStm<'_> {
    fn unparse(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("INC ")?;
        self.0.unparse(f)
    }
}
impl Unparse for DecStm<'_> {
    fn unparse(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("DEC ")?;
        self.0.unparse(f)
    }
}

impl Unparse for JmpStm<'_> {
    fn unparse(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("JMP ")?;
        self.0.unparse(f)
    }
}

impl Unparse for MovStm<'_> {
    fn unparse(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("MOV ")?;
        match self {
            Self::Single(a, b) => {
                a.unparse(f)?;
                if b.need_comma(a) {
                    f.write_char(',')?;
                }
                f.write_char(' ')?;
                b.unparse(f)
            }
            Self::Multiple(a, b, c) => {
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
        }
    }
}
impl Unparse for StoreStm<'_> {
    fn unparse(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(if *self.relative() {
            "STORER "
        } else {
            "STORE "
        })?;
        match self {
            Self::Single {
                relative: _,
                from: a,
                ptr: b,
            } => {
                a.unparse(f)?;
                if b.need_comma(a) {
                    f.write_char(',')?;
                }
                f.write_char(' ')?;
                b.unparse(f)
            }
            Self::Multiple {
                relative: _,
                from: a,
                ptr: b,
                l: c,
            } => {
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
        }
    }
}
impl Unparse for LoadStm<'_> {
    fn unparse(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(if *self.relative() { "LOADR " } else { "LOAD " })?;
        match self {
            Self::Single {
                relative: _,
                ptr: a,
                to: b,
            } => {
                a.unparse(f)?;
                if b.need_comma(a) {
                    f.write_char(',')?;
                }
                f.write_char(' ')?;
                b.unparse(f)
            }
            Self::Multiple {
                relative: _,
                ptr: a,
                to: b,
                l: c,
            } => {
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
        }
    }
}

impl Unparse for CallStm<'_> {
    fn unparse(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("CALL ")?;
        self.0.unparse(f)?;
        if self.1.need_comma(&self.0) {
            f.write_char(',')?;
        }
        f.write_char(' ')?;
        self.1.unparse(f)
    }
}
impl Unparse for RetStm {
    fn unparse(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("RET")
    }
}

impl Unparse for ExportStm<'_> {
    fn unparse(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("EXPORT")?;
        for lbl in &self.exported {
            f.write_char(' ')?;
            lbl.unparse(f)?;
        }
        Ok(())
    }
}
impl Unparse for EntryStm<'_> {
    fn unparse(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("ENTRY ")?;
        self.entry.unparse(f)
    }
}
