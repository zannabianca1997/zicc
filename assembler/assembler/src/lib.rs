#![feature(box_patterns)]
#![feature(unwrap_infallible)]
#![feature(assert_matches)]
#![feature(never_type)]
#![feature(box_into_inner)]

use std::collections::{
    btree_map::Entry::{Occupied, Vacant},
    BTreeMap, BTreeSet,
};

use bincode::BorrowDecode;

use errors::{Accumulator, PanicAccumulator};
use itertools::Itertools;
use lexer::{Identifier, SpecialIdentifier, StringLit};
use parse_from_rust::ica;
use parser::ast::*;
use serde::{Deserialize, Serialize};
use thiserror::Error;
use vm::VMInt;

pub use parser;

#[derive(Debug, Clone, Copy, Error)]
pub enum AssembleError<'s> {
    #[error("Label {0} is use in a constant expression")]
    LabelInConstExpr(LabelRef<'s>),
    #[error("Label {0} is not defined in this file")]
    UndefinedExport(Identifier<'s>),
    #[error("Label {0} was defined before")]
    RedefinedGlobal(Identifier<'s>, Identifier<'s>),
    #[error("Label {0} is undefined")]
    Undefined(Identifier<'s>),
    #[error("Another entry was defined before {0}")]
    RedefinedEntry(Identifier<'s>),
    #[error("Two units define an entry point")]
    RedefinedEntryInAnotherUnit,
}

/// A intcode code, made up of various units
#[derive(Debug, Clone)]
pub struct Code<'s> {
    /// All the values of this code
    values: Vec<Expression<'s>>,
    /// The global identifiers of this code
    globals: BTreeMap<Identifier<'s>, VMInt>,
    /// The entry point of this code
    entry: Option<VMInt>,
}
impl<'s> Code<'s> {
    pub fn new() -> Self {
        Self {
            values: vec![],
            globals: BTreeMap::new(),
            entry: None,
        }
    }
    pub fn push_unit(
        &mut self,
        Unit {
            values,
            globals,
            entry,
        }: Unit<'s>,
        mut errors: impl Accumulator<Error = AssembleError<'s>>,
    ) {
        for (g, p) in globals {
            match self.globals.entry(g) {
                Vacant(v) => {
                    v.insert(p + self.values.len() as VMInt);
                }
                Occupied(occ) => errors.push(AssembleError::RedefinedGlobal(g, *occ.key())),
            }
        }
        if let Some(entry) = entry {
            match &mut self.entry {
                Some(_) => errors.push(AssembleError::RedefinedEntryInAnotherUnit),
                e @ None => *e = Some(entry + self.values.len() as VMInt),
            }
        }
        let unit_start = Expression::Sum(
            Box::new(Expression::Ref(LabelRef::SpecialIdentifier(
                lexer::SpecialIdentifier::Start,
            ))),
            Box::new(Expression::Num(self.values.len() as i64)),
        );
        let unit_end = Expression::Sum(
            Box::new(Expression::Ref(LabelRef::SpecialIdentifier(
                lexer::SpecialIdentifier::Start,
            ))),
            Box::new(Expression::Num((self.values.len() + values.len()) as i64)),
        );
        self.values.extend(values.into_iter().map(|e| {
            e.replace(&mut |e| match e {
                LabelRef::Identifier(_) => Ok(None), // assume it is a global identifier
                LabelRef::SpecialIdentifier(SpecialIdentifier::UnitStart) => {
                    Ok(Some(unit_start.clone()))
                }
                LabelRef::SpecialIdentifier(SpecialIdentifier::UnitEnd) => {
                    Ok(Some(unit_end.clone()))
                }
                LabelRef::SpecialIdentifier(SpecialIdentifier::End | SpecialIdentifier::Start) => {
                    Ok(None)
                }
                LabelRef::Error(e) => <!>::from(*e),
            })
            .constant_folding()
        }))
    }

    pub fn emit(self, mut errors: impl Accumulator<Error = AssembleError<'s>>) -> Vec<VMInt> {
        // generating prologue if needed
        let values = self.entry.into_iter().flat_map(prologue).collect_vec();
        // setting start and end of the code
        let start = Expression::Num(values.len() as VMInt);
        let end = Expression::Num((values.len() + self.values.len()) as VMInt);
        // adding code and solving references
        values
            .into_iter()
            .chain(self.values)
            .flat_map(|e| {
                e.replace(&mut |lbl| match lbl {
                    LabelRef::Identifier(id) => self
                        .globals
                        .get(id)
                        .map(|v| Some(Expression::Num(*v)))
                        .ok_or_else(|| AssembleError::Undefined(*id)),
                    LabelRef::SpecialIdentifier(
                        SpecialIdentifier::UnitStart | SpecialIdentifier::UnitEnd,
                    ) => unreachable!(),
                    LabelRef::SpecialIdentifier(SpecialIdentifier::Start) => {
                        Ok(Some(start.clone()))
                    }
                    LabelRef::SpecialIdentifier(SpecialIdentifier::End) => Ok(Some(end.clone())),
                    LabelRef::Error(e) => <!>::from(*e),
                })
                .extract_errs(&mut errors)
                .map(|e| {
                    if let Expression::Num(e) = e.constant_folding() {
                        e
                    } else {
                        unreachable!()
                    }
                })
            })
            .collect()
    }
}

/// Generate the prologur for an executable. It calls the entry point, then exit immediately
fn prologue(entry_offset: VMInt) -> impl IntoIterator<Item = Expression<'static>> {
    let entry = Box::new(Expression::Sum(
        Box::new(Expression::Ref(LabelRef::SpecialIdentifier(
            SpecialIdentifier::Start,
        ))),
        Box::new(Expression::Num(entry_offset)),
    ));
    let stack_start = Box::new(Expression::Sum(
        Box::new(Expression::Ref(LabelRef::SpecialIdentifier(
            SpecialIdentifier::End,
        ))),
        Box::new(Expression::Num(1)),
    ));
    let prologue: File<'_> = ica!(
        call #{entry} {stack_start};
        halt
    );
    let Unit {
        values,
        globals,
        entry,
    } = Unit::assemble(prologue, PanicAccumulator::<AssembleError>::new());
    debug_assert!(globals.is_empty());
    debug_assert!(entry.is_none());
    values.into_iter().map(|e| {
        e.replace(&mut |lbl| {
            Ok(
                if let LabelRef::SpecialIdentifier(SpecialIdentifier::UnitStart) = lbl {
                    Some(Expression::Num(0))
                } else if let LabelRef::SpecialIdentifier(
                    SpecialIdentifier::End | SpecialIdentifier::Start,
                ) = lbl
                {
                    None
                } else {
                    unreachable!()
                },
            )
        })
    })
}

/// An unit of code
#[derive(Debug, Clone, BorrowDecode, Serialize, Deserialize, PartialEq, Eq)]
// #[bincode(encode_bounds = "'s: 'static")]
pub struct Unit<'s> {
    /// Entry point
    entry: Option<VMInt>,
    /// Labels defined in this unit, and their position
    #[serde(borrow)]
    globals: BTreeMap<Identifier<'s>, VMInt>,
    /// All the values of this unit
    #[serde(borrow)]
    values: Vec<Expression<'s>>,
}
impl<'s> ::bincode::Encode for Unit<'s> {
    fn encode<E: bincode::enc::Encoder>(
        &self,
        encoder: &mut E,
    ) -> Result<(), bincode::error::EncodeError> {
        bincode::Encode::encode(&self.entry, encoder)?;
        bincode::Encode::encode(&self.globals, encoder)?;
        utils::encode_vec(&self.values, encoder)?; // we have to avoid direct encoding cause the implementation of `encode` for `Vec` require `'static`
        Ok(())
    }
}

impl<'s> Unit<'s> {
    pub fn assemble<E>(ast: File<'s>, errors: E) -> Self
    where
        E: Accumulator<Error = AssembleError<'s>>,
    {
        let mut unit = AssemblingUnit {
            values: vec![],
            locals: BTreeMap::new(),
            errors,
            unnamed_counter: ast.max_unnamed_label().map(|l| l + 1).unwrap_or_default(),
            globals: BTreeSet::new(),
            entry: None,
        };
        ast.code_gen(&mut unit);
        // replacing locals
        Unit {
            values: unit
                .values
                .into_iter()
                .flat_map(|e| {
                    e.replace(&mut |lbl| match lbl {
                        LabelRef::Identifier(id) => {
                            if let Some(offset) = unit.locals.get(id) {
                                Ok(Some(Expression::Sum(
                                    Box::new(Expression::Ref(LabelRef::SpecialIdentifier(
                                        SpecialIdentifier::UnitStart,
                                    ))),
                                    Box::new(Expression::Num(*offset)),
                                )))
                            } else {
                                Ok(None) // Must be a global
                            }
                        }
                        LabelRef::SpecialIdentifier(_) => Ok(None),
                        LabelRef::Error(e) => <!>::from(*e),
                    })
                    .extract_errs(&mut unit.errors)
                    .map(|e| e.constant_folding())
                })
                .collect(),
            globals: unit
                .globals
                .into_iter()
                .filter_map(|g| {
                    if let Some(p) = unit.locals.get(&g) {
                        Some((g, *p))
                    } else {
                        unit.errors.push(AssembleError::UndefinedExport(g));
                        None
                    }
                })
                .collect(),
            entry: unit
                .entry
                .and_then(|e| {
                    unit.errors
                        .handle(unit.locals.get(&e).ok_or(AssembleError::Undefined(e)))
                })
                .copied(),
        }
    }

    pub fn is_entry(&self) -> bool {
        self.entry.is_some()
    }
}

/// An unit of code in the process of being assembled
struct AssemblingUnit<'s, Errors> {
    /// All the values of this unit
    values: Vec<Expression<'s>>,
    /// Labels defined in this unit, and their position
    locals: BTreeMap<Identifier<'s>, VMInt>,
    /// Global identifiers
    globals: BTreeSet<Identifier<'s>>,
    /// Entry point
    entry: Option<Identifier<'s>>,
    /// Error accumulator
    errors: Errors,
    /// Counter for unnamed labels
    unnamed_counter: usize,
}

impl<'s, E> AssemblingUnit<'s, E>
where
    E: Accumulator<Error = AssembleError<'s>>,
{
    fn const_expr(&mut self, arg: Expression<'s>) -> Option<VMInt> {
        arg.replace(&mut |lbl| Err(AssembleError::LabelInConstExpr(*lbl)))
            .extract_errs(&mut self.errors)
            .map(|e| match e.constant_folding() {
                Expression::Num(e) => e,
                f => {
                    unreachable!(
                        "expressions should have totally folded by now, instead `{}` remained",
                        f.display().to_string()
                    )
                }
            })
    }

    /// Obtain a new, unnamed label that's not used anywhere else
    fn unnamed_label(&mut self) -> Identifier<'static> {
        let label = self.unnamed_counter;
        self.unnamed_counter += 1;
        Identifier::Unnamed(label, (0, 0))
    }
}

trait CodeGen<'s> {
    fn code_gen<E>(self, unit: &mut AssemblingUnit<'s, E>)
    where
        E: Accumulator<Error = AssembleError<'s>>;
}

impl<'s> CodeGen<'s> for LabelDef<'s> {
    fn code_gen<E>(self, unit: &mut AssemblingUnit<'s, E>)
    where
        E: Accumulator<Error = AssembleError<'s>>,
    {
        unit.locals.insert(self.label, unit.values.len() as VMInt);
    }
}
impl<'s> CodeGen<'s> for Expression<'s> {
    fn code_gen<E>(self, unit: &mut AssemblingUnit<'s, E>)
    where
        E: Accumulator<Error = AssembleError<'s>>,
    {
        unit.values.push(self)
    }
}
impl<'s> CodeGen<'s> for VMInt {
    fn code_gen<E>(self, unit: &mut AssemblingUnit<'s, E>)
    where
        E: Accumulator<Error = AssembleError<'s>>,
    {
        Box::new(Expression::Num(self)).code_gen(unit)
    }
}
impl<'s> CodeGen<'s> for StringLit<'s> {
    fn code_gen<E>(self, unit: &mut AssemblingUnit<'s, E>)
    where
        E: Accumulator<Error = AssembleError<'s>>,
    {
        for ch in self {
            ch.code_gen(unit)
        }
    }
}

impl<'s> CodeGen<'s> for File<'s> {
    fn code_gen<E>(self, unit: &mut AssemblingUnit<'s, E>)
    where
        E: Accumulator<Error = AssembleError<'s>>,
    {
        self.statements.code_gen(unit)
    }
}

impl<'s, T> CodeGen<'s> for Vec<T>
where
    T: CodeGen<'s>,
{
    fn code_gen<E>(self, unit: &mut AssemblingUnit<'s, E>)
    where
        E: Accumulator<Error = AssembleError<'s>>,
    {
        for i in self {
            i.code_gen(unit)
        }
    }
}
impl<'s, T> CodeGen<'s> for Box<T>
where
    T: CodeGen<'s>,
{
    fn code_gen<E>(self, unit: &mut AssemblingUnit<'s, E>)
    where
        E: Accumulator<Error = AssembleError<'s>>,
    {
        let inner = Box::into_inner(self);
        inner.code_gen(unit)
    }
}

impl<'s, T> CodeGen<'s> for Labelled<'s, T>
where
    T: CodeGen<'s>,
{
    fn code_gen<E>(self, unit: &mut AssemblingUnit<'s, E>)
    where
        E: Accumulator<Error = AssembleError<'s>>,
    {
        let Labelled { labels, content } = self;
        for def in labels {
            def.code_gen(unit)
        }
        content.code_gen(unit)
    }
}

impl<'s, T> CodeGen<'s> for Option<T>
where
    T: CodeGen<'s>,
{
    fn code_gen<E>(self, unit: &mut AssemblingUnit<'s, E>)
    where
        E: Accumulator<Error = AssembleError<'s>>,
    {
        if let Some(this) = self {
            this.code_gen(unit)
        }
    }
}

macro_rules! codegen_for_statement {
    (
        $($variant:ident)*
    ) => {
        impl<'s> CodeGen<'s> for Statement<'s> {
            fn code_gen<E>(self, unit: &mut AssemblingUnit<'s, E>)
            where
                E: Accumulator<Error = AssembleError<'s>>,
            {
                match self {
                    $(
                    Statement::$variant(stm) => CodeGen::<'s>::code_gen(stm, unit),
                    )*
                    Statement::Error(e) => <!>::from(e)
                }
            }
        }
    };
}
codegen_for_statement! {
    Ints Zeros Instruction Inc Dec Jmp Mov Load Store Call Ret Export Entry
}

impl<'s> CodeGen<'s> for IntsStm<'s> {
    fn code_gen<E>(self, unit: &mut AssemblingUnit<'s, E>)
    where
        E: Accumulator<Error = AssembleError<'s>>,
    {
        self.values.code_gen(unit)
    }
}

impl<'s> CodeGen<'s> for IntsParam<'s> {
    fn code_gen<E>(self, unit: &mut AssemblingUnit<'s, E>)
    where
        E: Accumulator<Error = AssembleError<'s>>,
    {
        match self {
            IntsParam::Int(i) => i.code_gen(unit),
            IntsParam::Str(s) => s.code_gen(unit),
        }
    }
}

impl<'s> CodeGen<'s> for ZerosStm<'s> {
    fn code_gen<E>(self, unit: &mut AssemblingUnit<'s, E>)
    where
        E: Accumulator<Error = AssembleError<'s>>,
    {
        let n = unit.const_expr(*self.0).unwrap_or_default();
        for _ in 0..n {
            (0 as VMInt).code_gen(unit)
        }
    }
}

impl<'s> CodeGen<'s> for Instruction<'s> {
    fn code_gen<E>(self, unit: &mut AssemblingUnit<'s, E>)
    where
        E: Accumulator<Error = AssembleError<'s>>,
    {
        let opcode = {
            let opcode = self.opcode();
            let mut modes = self.param_modes().into_iter();
            let [a, b, c] = [modes.next(), modes.next(), modes.next()].map(|m| m.unwrap_or(0));
            c * 10000 + b * 1000 + a * 100 + opcode
        };
        let params = self.into_param_values();

        opcode.code_gen(unit);
        for p in params {
            p.code_gen(unit)
        }
    }
}

impl<'s> CodeGen<'s> for IncStm<'s> {
    fn code_gen<E>(self, unit: &mut AssemblingUnit<'s, E>)
    where
        E: Accumulator<Error = AssembleError<'s>>,
    {
        let Self(p) = self;
        ica!(
            add {p.clone()} #1 {p}
        )
        .code_gen(unit)
    }
}

impl<'s> CodeGen<'s> for DecStm<'s> {
    fn code_gen<E>(self, unit: &mut AssemblingUnit<'s, E>)
    where
        E: Accumulator<Error = AssembleError<'s>>,
    {
        let Self(p) = self;
        ica!(
            add {p.clone()} #-1 {p}
        )
        .code_gen(unit)
    }
}

impl<'s> CodeGen<'s> for JmpStm<'s> {
    fn code_gen<E>(self, unit: &mut AssemblingUnit<'s, E>)
    where
        E: Accumulator<Error = AssembleError<'s>>,
    {
        ica!(
            jz #0 {self.0}
        )
        .code_gen(unit)
    }
}

impl<'s> CodeGen<'s> for MovStm<'s> {
    fn code_gen<E>(self, unit: &mut AssemblingUnit<'s, E>)
    where
        E: Accumulator<Error = AssembleError<'s>>,
    {
        match self {
            MovStm::Single(a, b) => ica!(
                add {a} #0 {b}
            )
            .code_gen(unit),
            MovStm::Multiple(a, b, box n) => {
                let Some(n) = unit.const_expr(n) else {
                    return;
                };
                for i in 0..n {
                    let mut a = ReadParam::from(a.clone());
                    *a.as_value_mut().content += i;
                    let mut b = WriteParam::from(b.clone());
                    *b.as_value_mut().content += i;
                    ica!(
                        mov {a} {b}
                    )
                    .code_gen(unit)
                }
            }
        }
    }
}

impl<'s> CodeGen<'s> for LoadStm<'s> {
    fn code_gen<E>(self, unit: &mut AssemblingUnit<'s, E>)
    where
        E: Accumulator<Error = AssembleError<'s>>,
    {
        /*
           Implementation of Load uses self modifing code
           The value of ptr is moved to the next instruction, that read from it and move to the destination
           It could be implemented without, by pushing a `mov {from} {to}; jmp {back}` somewhere on the stack,
           then jumping to it. This require stack management and does not give any semsible benefit
        */
        match self {
            LoadStm::Single { relative, ptr, to } => {
                let label = unit.unnamed_label();
                let param_ref = Box::new(Expression::Ref(LabelRef::Identifier(label)));
                let param = if relative {
                    ReadParam::Relative(RelativeParam {
                        value: Labelled {
                            labels: BTreeSet::from([LabelDef { label }]),
                            content: 0.into(),
                        },
                    })
                } else {
                    ReadParam::Absolute(AbsoluteParam {
                        value: Labelled {
                            labels: BTreeSet::from([LabelDef { label }]),
                            content: 0.into(),
                        },
                    })
                };
                ica!(
                    mov {ptr} {param_ref};
                    mov {param} {to}
                )
                .code_gen(unit)
            }
            LoadStm::Multiple {
                relative,
                ptr,
                to,
                l: n,
            } => {
                let n = unit.const_expr(*n).unwrap_or_default();
                for offset in 0..n {
                    let label = unit.unnamed_label();
                    let param_ref = Box::new(Expression::Ref(LabelRef::Identifier(label)));
                    let param = if relative {
                        ReadParam::Relative(RelativeParam {
                            value: Labelled {
                                labels: BTreeSet::from([LabelDef { label }]),
                                content: 0.into(),
                            },
                        })
                    } else {
                        ReadParam::Absolute(AbsoluteParam {
                            value: Labelled {
                                labels: BTreeSet::from([LabelDef { label }]),
                                content: 0.into(),
                            },
                        })
                    };
                    ica!(
                        add {ptr.clone()} #{offset} {param_ref};
                        mov {param} {to.clone()}
                    )
                    .code_gen(unit)
                }
            }
        }
    }
}
impl<'s> CodeGen<'s> for StoreStm<'s> {
    fn code_gen<E>(self, unit: &mut AssemblingUnit<'s, E>)
    where
        E: Accumulator<Error = AssembleError<'s>>,
    {
        /*
           Implementation of Store uses self modifing code
           The value of ptr is moved to the next instruction, that move the values from it to the destination
           It could be implemented without, by pushing a `mov {from} {to}; jmp {back}` somewhere on the stack,
           then jumping to it. This require stack management and does not give any sensible benefit
        */
        match self {
            StoreStm::Single {
                relative,
                from,
                ptr,
            } => {
                let label = unit.unnamed_label();
                let param_ref = Box::new(Expression::Ref(LabelRef::Identifier(label)));
                let param = if relative {
                    WriteParam::Relative(RelativeParam {
                        value: Labelled {
                            labels: BTreeSet::from([LabelDef { label }]),
                            content: 0.into(),
                        },
                    })
                } else {
                    WriteParam::Absolute(AbsoluteParam {
                        value: Labelled {
                            labels: BTreeSet::from([LabelDef { label }]),
                            content: 0.into(),
                        },
                    })
                };
                ica!(
                    mov {ptr} {param_ref};
                    mov {from} {param}
                )
                .code_gen(unit)
            }
            StoreStm::Multiple {
                relative,
                from,
                ptr,
                l: n,
            } => {
                let n = unit.const_expr(*n).unwrap_or_default();
                for offset in 0..n {
                    let label = unit.unnamed_label();
                    let param_ref = Box::new(Expression::Ref(LabelRef::Identifier(label)));
                    let param = if relative {
                        WriteParam::Relative(RelativeParam {
                            value: Labelled {
                                labels: BTreeSet::from([LabelDef { label }]),
                                content: 0.into(),
                            },
                        })
                    } else {
                        WriteParam::Absolute(AbsoluteParam {
                            value: Labelled {
                                labels: BTreeSet::from([LabelDef { label }]),
                                content: 0.into(),
                            },
                        })
                    };
                    ica!(
                        add {ptr.clone()} #{offset} {param_ref}; // offset the parameter each time a little more
                        mov {from.clone()} {param}
                    )
                    .code_gen(unit)
                }
            }
        }
    }
}

impl<'s> CodeGen<'s> for CallStm<'s> {
    fn code_gen<E>(self, unit: &mut AssemblingUnit<'s, E>)
    where
        E: Accumulator<Error = AssembleError<'s>>,
    {
        let Self(addr, stack_top) = self;
        // adding 1 to the offset for the return address
        let offset =
            Box::new(Expression::Sum(stack_top, Box::new(Expression::Num(1)))).constant_folding();
        let label = unit.unnamed_label();
        ica!(
            incb #{offset.clone()};
            mov #{Box::new(Expression::Ref(LabelRef::Identifier(label)))} @-1;
            jmp {addr};
            {label} :
            incb #-{offset}
        )
        .code_gen(unit);
    }
}

impl<'s> CodeGen<'s> for RetStm {
    fn code_gen<E>(self, unit: &mut AssemblingUnit<'s, E>)
    where
        E: Accumulator<Error = AssembleError<'s>>,
    {
        ica!(
            jmp @-1
        )
        .code_gen(unit)
    }
}

impl<'s> CodeGen<'s> for ExportStm<'s> {
    fn code_gen<E>(self, unit: &mut AssemblingUnit<'s, E>)
    where
        E: Accumulator<Error = AssembleError<'s>>,
    {
        unit.globals.extend(self.exported)
    }
}
impl<'s> CodeGen<'s> for EntryStm<'s> {
    fn code_gen<E>(self, unit: &mut AssemblingUnit<'s, E>)
    where
        E: Accumulator<Error = AssembleError<'s>>,
    {
        match &mut unit.entry {
            Some(_) => unit.errors.push(AssembleError::RedefinedEntry(self.entry)),
            entry @ None => *entry = Some(self.entry),
        }
    }
}
#[cfg(test)]
mod tests {
    use errors::RootAccumulator;
    use parser::ParseError;
    use test_sources::{test_io, test_sources};
    use vm::VMInt;

    use crate::{AssembleError, Code, Unit};

    fn test_run(code: Vec<VMInt>, mut r#in: &[vm::VMInt]) -> Vec<VMInt> {
        let mut vm = vm::VM::new(code);
        let mut out = vec![];
        loop {
            match vm
                .run()
                .expect("The assembled source should not throw errors")
            {
                vm::StopState::NeedInput(need_input) => {
                    let (next_input, rest) = r#in
                        .split_first()
                        .expect("The machine asked for more input");
                    r#in = rest;
                    need_input.give(*next_input)
                }
                vm::StopState::HasOutput(has_output) => out.push(has_output.get()),
                vm::StopState::Halted => break,
            }
        }
        out
    }

    #[test_sources]
    fn assemble(source: &str, assembled: Option<&[vm::VMInt]>) {
        use errors::PanicAccumulator;
        let ast = parser::parse(source, &mut PanicAccumulator::<ParseError>::new()).unwrap();
        let mut errors = RootAccumulator::<AssembleError>::new();
        let unit = Unit::assemble(ast, &mut errors);
        if let Err(errs) = errors.checkpoint() {
            for err in errs {
                eprintln!("{:?}", err)
            }
            panic!("Errors during assembly")
        }
        let mut code = Code::new();
        code.push_unit(unit, &mut errors);
        let code = code.emit(&mut errors);
        if let Err(errs) = errors.checkpoint() {
            for err in errs {
                eprintln!("{:?}", err)
            }
            panic!("Errors during linking")
        }
        if let Some(assembled) = assembled {
            assert_eq!(
                &code, assembled,
                "The assembled code is different from the provided one"
            )
        }
    }

    #[test_io]
    fn io(source: &str, r#in: &[vm::VMInt], expected: &[vm::VMInt]) {
        use errors::PanicAccumulator;

        let ast = parser::parse(source, &mut PanicAccumulator::<ParseError>::new()).unwrap();
        let unit = Unit::assemble(ast, &mut PanicAccumulator::<AssembleError>::new());
        let mut code = Code::new();
        code.push_unit(unit, &mut PanicAccumulator::<AssembleError>::new());
        let code = code.emit(&mut PanicAccumulator::<AssembleError>::new());

        assert_eq!(
            &test_run(code, r#in),
            expected,
            "The machine gave a different output"
        )
    }

    mod globals {
        use std::assert_matches::assert_matches;

        use errors::{PanicAccumulator, RootAccumulator};
        use itertools::Itertools;
        use lexer::Identifier;
        use parser::ParseError;

        use crate::{tests::test_run, AssembleError, Code, Unit};

        #[test]
        fn okay() {
            let file_a = r#"
                // this is file a

                export out_42

                halt // guard to assure we are not entering from this file

                out_42:
                    out #42
                    halt
            "#;
            let file_b = r#"
                // this is file b

                jmp #out_42
            "#;

            let file_a = parser::parse(file_a, &mut PanicAccumulator::<ParseError>::new()).unwrap();
            let file_b = parser::parse(file_b, &mut PanicAccumulator::<ParseError>::new()).unwrap();

            let mut errors = RootAccumulator::<AssembleError>::new();
            let unit_a = Unit::assemble(file_a, &mut errors);
            let unit_b = Unit::assemble(file_b, &mut errors);

            if let Err(errs) = errors.checkpoint() {
                for err in errs {
                    eprintln!("{:?}", err)
                }
                panic!("Errors during assembly")
            }

            let mut code = Code::new();
            code.push_unit(unit_b, &mut errors);
            code.push_unit(unit_a, &mut errors);
            let code = code.emit(&mut errors);
            if let Err(errs) = errors.checkpoint() {
                for err in errs {
                    eprintln!("{:?}", err)
                }
                panic!("Errors during linking")
            }

            assert_eq!(
                &test_run(code, &[]),
                &[42],
                "The machine gave a different output"
            )
        }
        #[test]
        fn err() {
            let file_a = r#"
                // this is file a

                // export out_42

                halt // guard to assure we are not entering from this file

                out_42:
                    out #42
                    halt
            "#;
            let file_b = r#"
                // this is file b

                jmp #out_42
            "#;

            let file_a = parser::parse(file_a, &mut PanicAccumulator::<ParseError>::new()).unwrap();
            let file_b = parser::parse(file_b, &mut PanicAccumulator::<ParseError>::new()).unwrap();

            let mut errors = RootAccumulator::<AssembleError>::new();
            let unit_a = Unit::assemble(file_a, &mut errors);
            let unit_b = Unit::assemble(file_b, &mut errors);

            if let Err(errs) = errors.checkpoint() {
                for err in errs {
                    eprintln!("{:?}", err)
                }
                panic!("Errors during assembly")
            }

            let mut code = Code::new();
            code.push_unit(unit_b, &mut errors);
            code.push_unit(unit_a, &mut errors);
            let _ = code.emit(&mut errors);
            if let Err(errs) = errors.checkpoint() {
                let errs = errs.into_iter().collect_vec();
                assert_matches!(
                    &errs[..],
                    &[AssembleError::Undefined(Identifier::Named("out_42", _))]
                )
            } else {
                panic!("Linking gave no errors")
            }
        }
    }

    mod unit_serialization {
        use crate::{AssembleError, Unit};
        use parser::ParseError;
        use test_sources::test_sources;

        fn unit(source: &str) -> Unit {
            use errors::PanicAccumulator;
            let ast = parser::parse(source, &mut PanicAccumulator::<ParseError>::new()).unwrap();
            Unit::assemble(ast, &mut PanicAccumulator::<AssembleError>::new())
        }

        #[test_sources]
        fn serde(source: &str) {
            let unit = unit(source);
            let serialized = serde_json::to_string(&unit).expect("The unit should serialize");
            let deserialized =
                serde_json::from_str(&serialized).expect("The unit should deserialize");
            assert_eq!(
                unit, deserialized,
                "The deserialized unit should be the same"
            )
        }

        #[test_sources]
        fn bincode(source: &str) {
            let unit = unit(source);
            let serialized = ::bincode::encode_to_vec(&unit, ::bincode::config::standard())
                .expect("The unit should encode");
            let deserialized =
                ::bincode::borrow_decode_from_slice(&serialized, ::bincode::config::standard())
                    .expect("The unit should decode")
                    .0;
            assert_eq!(
                unit, deserialized,
                "The deserialized unit should be the same"
            )
        }
    }
}
