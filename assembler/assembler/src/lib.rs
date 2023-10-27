#![feature(box_patterns)]
#![feature(unwrap_infallible)]
use std::collections::{
    btree_map::Entry::{Occupied, Vacant},
    BTreeMap, BTreeSet,
};

use either::{
    for_both,
    Either::{self, Left},
};
use errors::Accumulator;
use lexer::{Identifier, SpecialIdentifier, StringLit};
use parse_from_rust::ica;
use parser::ast::*;
use vm::VMInt;

#[derive(Debug, Clone, Copy)]
pub enum AssembleError<'s> {
    LabelInConstExpr(LabelRef<'s>),
    UndefinedUnnamed(Identifier<'s>),
    UndefinedExport(Identifier<'s>),
    RedefinedGlobal(Identifier<'s>, Identifier<'s>),
    Undefined(Identifier<'s>),
}

/// A intcode code, made up of various units
pub struct Code<'s> {
    /// All the values of this code
    values: Vec<Box<Expression<'s>>>,
    /// The global identifiers of this code
    globals: BTreeMap<Identifier<'s>, VMInt>,
}
impl<'s> Code<'s> {
    pub fn new() -> Self {
        Self {
            values: vec![],
            globals: BTreeMap::new(),
        }
    }
    pub fn push_unit(
        &mut self,
        Unit { values, globals }: Unit<'s>,
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
                LabelRef::Error(e) => *e,
            })
            .constant_folding()
        }))
    }

    pub fn emit(self, mut errors: impl Accumulator<Error = AssembleError<'s>>) -> Vec<VMInt> {
        let start = Expression::Num(0);
        let end = Expression::Num(self.values.len() as i64);
        self.values
            .into_iter()
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
                    LabelRef::Error(e) => *e,
                })
                .extract_errs(&mut errors)
                .map(|e| {
                    if let box Expression::Num(e) = e.constant_folding() {
                        e
                    } else {
                        unreachable!()
                    }
                })
            })
            .collect()
    }
}

/// An unit of code
pub struct Unit<'s> {
    /// All the values of this unit
    values: Vec<Box<Expression<'s>>>,
    /// Labels defined in this unit, and their position
    globals: BTreeMap<Identifier<'s>, VMInt>,
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
                        LabelRef::Error(e) => *e,
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
        }
    }
}

/// An unit of code in the process of being assembled
struct AssemblingUnit<'s, Errors> {
    /// All the values of this unit
    values: Vec<Box<Expression<'s>>>,
    /// Labels defined in this unit, and their position
    locals: BTreeMap<Identifier<'s>, VMInt>,
    /// Global identifiers
    globals: BTreeSet<Identifier<'s>>,
    /// Error accumulator
    errors: Errors,
    /// Counter for unnamed labels
    unnamed_counter: usize,
}

impl<'s, E> AssemblingUnit<'s, E>
where
    E: Accumulator<Error = AssembleError<'s>>,
{
    fn const_expr(&mut self, arg: Box<Expression<'s>>) -> Option<VMInt> {
        arg.replace(&mut |lbl| Err(AssembleError::LabelInConstExpr(*lbl)))
            .extract_errs(&mut self.errors)
            .map(|e| {
                if let box Expression::Num(e) = e.constant_folding() {
                    e
                } else {
                    unreachable!()
                }
            })
    }

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
impl<'s> CodeGen<'s> for Box<Expression<'s>> {
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

impl<'s, R, L> CodeGen<'s> for Either<L, R>
where
    R: CodeGen<'s>,
    L: CodeGen<'s>,
{
    fn code_gen<E>(self, unit: &mut AssemblingUnit<'s, E>)
    where
        E: Accumulator<Error = AssembleError<'s>>,
    {
        for_both!(self, s=>s.code_gen(unit))
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
                    Statement::Error(e) => e
                }
            }
        }
    };
}
codegen_for_statement! {
    Ints Instruction Inc Dec Jmp Mov Zeros Push Pop Call Ret Export
}

impl<'s> CodeGen<'s> for IntsStm<'s> {
    fn code_gen<E>(self, unit: &mut AssemblingUnit<'s, E>)
    where
        E: Accumulator<Error = AssembleError<'s>>,
    {
        self.values.code_gen(unit)
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

impl<'s> CodeGen<'s> for ZerosStm<'s> {
    fn code_gen<E>(self, unit: &mut AssemblingUnit<'s, E>)
    where
        E: Accumulator<Error = AssembleError<'s>>,
    {
        let Some(n) = unit
            .const_expr(self.0)
            .and_then(|n| usize::try_from(n).ok())
        else {
            return;
        };
        IntsStm {
            values: vec![Left(Box::new(Expression::Num(0))).into(); n],
        }
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
            MovStm::Multiple(a, b, n) => {
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
impl<'s> CodeGen<'s> for PushStm<'s> {
    fn code_gen<E>(self, unit: &mut AssemblingUnit<'s, E>)
    where
        E: Accumulator<Error = AssembleError<'s>>,
    {
        match self {
            PushStm::Single(a) => ica!(
                mov {a} @0;
                incb #1
            )
            .code_gen(unit),
            PushStm::Multiple(a, n) => ica!(
                mov {a} @0 {n.clone()};
                incb #{n}
            )
            .code_gen(unit),
        }
    }
}
impl<'s> CodeGen<'s> for PopStm<'s> {
    fn code_gen<E>(self, unit: &mut AssemblingUnit<'s, E>)
    where
        E: Accumulator<Error = AssembleError<'s>>,
    {
        match self {
            PopStm::Single(a) => ica!(
                mov @-1 {a};
                incb #-1
            )
            .code_gen(unit),
            PopStm::Multiple(a, n) => ica!(
                mov @-{n.clone()} {a} {n.clone()};
                incb #-{n}
            )
            .code_gen(unit),
        }
    }
}

impl<'s> CodeGen<'s> for CallStm<'s> {
    fn code_gen<E>(self, unit: &mut AssemblingUnit<'s, E>)
    where
        E: Accumulator<Error = AssembleError<'s>>,
    {
        let Self(mut addr) = self;
        // patching the address if it's relative, given we have to jump AFTER we push
        if let ReadParam::Relative(RelativeParam {
            value: Labelled { content, .. },
        }) = &mut addr
        {
            **content += -1;
        }
        let label = unit.unnamed_label();
        ica!(
            push #{Box::new(Expression::Ref(LabelRef::Identifier(label)))};
            jmp {addr};
        )
        .code_gen(unit);
        LabelDef { label }.code_gen(unit)
    }
}

impl<'s> CodeGen<'s> for RetStm {
    fn code_gen<E>(self, unit: &mut AssemblingUnit<'s, E>)
    where
        E: Accumulator<Error = AssembleError<'s>>,
    {
        ica!(
            incb #-1;
            jmp @0
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
#[cfg(test)]
mod tests {
    use errors::RootAccumulator;
    use parser::ParseError;
    use test_sources::{test_io, test_sources};

    use crate::{AssembleError, Code, Unit};

    #[test_sources]
    fn assemble(source: &str) {
        let ast = parser::parse(source, &mut RootAccumulator::<ParseError>::new()).unwrap();
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
        let _ = code.emit(&mut errors);
        if let Err(errs) = errors.checkpoint() {
            for err in errs {
                eprintln!("{:?}", err)
            }
            panic!("Errors during linking")
        }
    }
    #[test_io]
    fn io(source: &str, r#in: &[vm::VMInt], expected: &[vm::VMInt]) {
        use vm::ICMachine;
        let ast = parser::parse(source, &mut RootAccumulator::<ParseError>::new()).unwrap();
        let unit = Unit::assemble(ast, &mut RootAccumulator::<AssembleError>::new());
        let mut code = Code::new();
        code.push_unit(unit, &mut RootAccumulator::<AssembleError>::new());
        let code = code.emit(&mut RootAccumulator::<AssembleError>::new());

        let mut vm = vm::ICMachineData::new(&code);
        for i in r#in {
            vm.give_input(*i).into_ok()
        }
        match vm.run() {
            vm::ICMachineStopState::EmptyInput => panic!("The machine asked for more input"),
            vm::ICMachineStopState::RuntimeErr(err) => panic!("The machine gave an error: {err}"),
            vm::ICMachineStopState::Halted => (),
        }
        let out = {
            let mut out = vec![];
            while let Some(v) = vm.get_output() {
                out.push(v)
            }
            out
        };
        assert_eq!(&out, expected, "The machine gave a different output")
    }
}
