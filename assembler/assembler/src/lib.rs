#![feature(box_into_inner)]
#![feature(unwrap_infallible)]

use std::collections::{
    btree_map::Entry::{Occupied, Vacant},
    BTreeMap, BTreeSet,
};

use either::Either;
use itertools::Itertools;
use thiserror::Error;

use errors::{Accumulator, RootAccumulator};
use lexer::{Identifier, SpecialIdentifier, StringLit};
use parser::ast::{
    DecStm, Expression, File, ImmediateParam, IncStm, Instruction, IntsStm, LabelDef, LabelRef,
    Labelled, ReadParam, Statement,
};
use vm::VMInt;

pub struct Code<'s, 'e, E> {
    values: Vec<Expression<'s>>,
    labels: BTreeMap<Identifier<'s>, usize>,
    errors: &'e mut RootAccumulator<E>,
}
impl<'s, 'e, E> Code<'s, 'e, E> {
    fn push_value(&mut self, value: Expression<'s>) {
        self.values.push(value)
    }
    fn push_num(&mut self, value: VMInt) {
        self.values.push(Expression::Num(value))
    }
    fn push_label(&mut self, value: LabelDef<'s>)
    where
        E: From<AssembleError>,
    {
        match self.labels.entry(value.label) {
            Vacant(entry) => {
                entry.insert(self.values.len());
            }
            Occupied(_entry) => self
                .errors
                .push(AssembleError::DoubleDef(value.label.to_string())),
        }
    }

    pub fn new(errors: &'e mut RootAccumulator<E>) -> Self {
        Code {
            values: vec![],
            labels: BTreeMap::new(),
            errors,
        }
    }

    pub fn push_unit(&mut self, ast: File<'s>)
    where
        E: From<AssembleError>,
    {
        let unit_start = self.values.len();
        ast.write_to(self);
        let unit_end = self.values.len();
        for value in &mut self.values[unit_start..unit_end] {
            value.replace(
                LabelRef::SpecialIdentifier(SpecialIdentifier::UnitStart),
                unit_start
                    .try_into()
                    .expect("The lenght should not overflow VMInt"),
            );
            value.replace(
                LabelRef::SpecialIdentifier(SpecialIdentifier::UnitEnd),
                unit_end
                    .try_into()
                    .expect("The lenght should not overflow VMInt"),
            );
        }
    }

    pub fn codegen(self) -> Vec<vm::VMInt>
    where
        E: From<AssembleError>,
    {
        let code_end = self
            .values
            .len()
            .try_into()
            .expect("The lenght should not overflow VMInt");
        self.values
            .into_iter()
            .flat_map(|e| {
                e.calculate(&mut |id| match id {
                    LabelRef::Identifier(id) => match self.labels.get(&id) {
                        Some(pos) => Some(
                            (*pos)
                                .try_into()
                                .expect("The lenght should not overflow VMInt"),
                        ),
                        None => {
                            self.errors.push(AssembleError::UnknowLabel(id.to_string()));
                            None
                        }
                    },
                    LabelRef::SpecialIdentifier(SpecialIdentifier::Start) => Some(0),
                    LabelRef::SpecialIdentifier(SpecialIdentifier::End) => Some(code_end),
                    LabelRef::SpecialIdentifier(
                        SpecialIdentifier::UnitEnd | SpecialIdentifier::UnitStart,
                    ) => unreachable!(),
                    LabelRef::Error(e) => *e,
                })
            })
            .collect_vec()
    }
}

trait WriteTo<'s, 'e, E> {
    fn write_to(self, code: &mut Code<'s, 'e, E>);
}

impl<'s, 'e, E> WriteTo<'s, 'e, E> for File<'s>
where
    E: From<AssembleError>,
{
    fn write_to(self, code: &mut Code<'s, 'e, E>) {
        for s in self.statements {
            s.write_to(code)
        }
    }
}

impl<'s, 'e, E, T> WriteTo<'s, 'e, E> for Labelled<'s, T>
where
    E: From<AssembleError>,
    T: WriteTo<'s, 'e, E>,
{
    fn write_to(self, code: &mut Code<'s, 'e, E>)
    where
        E: From<AssembleError>,
    {
        for def in self.labels {
            def.write_to(code)
        }
        self.content.write_to(code)
    }
}
impl<'s, 'e, E, R, L> WriteTo<'s, 'e, E> for Either<L, R>
where
    E: From<AssembleError>,
    R: WriteTo<'s, 'e, E>,
    L: WriteTo<'s, 'e, E>,
{
    fn write_to(self, code: &mut Code<'s, 'e, E>)
    where
        E: From<AssembleError>,
    {
        match self {
            Either::Left(l) => l.write_to(code),
            Either::Right(r) => r.write_to(code),
        }
    }
}

impl<'s, 'e, E, T> WriteTo<'s, 'e, E> for Option<T>
where
    T: WriteTo<'s, 'e, E>,
{
    fn write_to(self, code: &mut Code<'s, 'e, E>) {
        if let Some(this) = self {
            this.write_to(code)
        }
    }
}
impl<'s, 'e, E, T> WriteTo<'s, 'e, E> for Box<T>
where
    T: WriteTo<'s, 'e, E>,
{
    fn write_to(self, code: &mut Code<'s, 'e, E>) {
        Box::into_inner(self).write_to(code)
    }
}

impl<'s, 'e, E> WriteTo<'s, 'e, E> for Statement<'s>
where
    E: From<AssembleError>,
{
    fn write_to(self, code: &mut Code<'s, 'e, E>) {
        match self {
            Statement::IntsStm(ints) => ints.write_to(code),
            Statement::Instruction(instr) => instr.write_to(code),
            Statement::Inc(inc) => inc.write_to(code),
            Statement::Dec(dec) => dec.write_to(code),
            Statement::Error(e) => e,
        }
    }
}

impl<'s, 'e, E> WriteTo<'s, 'e, E> for IntsStm<'s>
where
    E: From<AssembleError>,
{
    fn write_to(self, code: &mut Code<'s, 'e, E>) {
        for v in self.values {
            v.write_to(code)
        }
    }
}
impl<'s, 'e, E> WriteTo<'s, 'e, E> for StringLit<'s> {
    fn write_to(self, code: &mut Code<'s, 'e, E>) {
        for value in self {
            code.push_value(Expression::Num(value))
        }
    }
}

impl<'s, 'e, E> WriteTo<'s, 'e, E> for Instruction<'s>
where
    E: From<AssembleError>,
{
    fn write_to(self, code: &mut Code<'s, 'e, E>) {
        let mut opcode = self.opcode();
        for (val, mode) in self.param_modes().into_iter().zip([100, 1000, 10000]) {
            opcode += val * mode
        }
        code.push_num(opcode);
        for p in self.into_param_values() {
            p.write_to(code)
        }
    }
}

impl<'s, 'e, E> WriteTo<'s, 'e, E> for IncStm<'s>
where
    E: From<AssembleError>,
{
    fn write_to(self, code: &mut Code<'s, 'e, E>) {
        Instruction::Add(
            self.0.clone().into(),
            ReadParam::Immediate(ImmediateParam {
                value: Labelled {
                    labels: BTreeSet::new(),
                    content: Box::new(Expression::Num(1)),
                },
            }),
            self.0.into(),
        )
        .write_to(code)
    }
}
impl<'s, 'e, E> WriteTo<'s, 'e, E> for DecStm<'s>
where
    E: From<AssembleError>,
{
    fn write_to(self, code: &mut Code<'s, 'e, E>) {
        Instruction::Add(
            self.0.clone().into(),
            ReadParam::Immediate(ImmediateParam {
                value: Labelled {
                    labels: BTreeSet::new(),
                    content: Box::new(Expression::Num(-1)),
                },
            }),
            self.0.into(),
        )
        .write_to(code)
    }
}

impl<'s, 'e, E> WriteTo<'s, 'e, E> for Expression<'s> {
    fn write_to(self, code: &mut Code<'s, 'e, E>) {
        code.push_value(self)
    }
}
impl<'s, 'e, E> WriteTo<'s, 'e, E> for LabelDef<'s>
where
    E: From<AssembleError>,
{
    fn write_to(self, code: &mut Code<'s, 'e, E>) {
        code.push_label(self)
    }
}

#[derive(Debug, Clone, Error)]
pub enum AssembleError {
    #[error("Label was already in use")]
    DoubleDef(String),
    #[error("Unknow label")]
    UnknowLabel(String),
}

#[cfg(test)]
mod sources {
    use super::*;
    use test_sources::{test_io, test_sources};

    #[test_sources]
    fn assemble(source: &str) {
        let ast = parser::parse(source, &mut RootAccumulator::<parser::ParseError>::new()).unwrap();

        let mut errors = RootAccumulator::<AssembleError>::new();
        let mut code = Code::new(&mut errors);
        code.push_unit(ast);
        let code = code.codegen();

        if let Err(errs) = errors.finish_with(code) {
            panic!("Errors in parsing:\n\n{}", errs.into_iter().format("\n"))
        }
    }

    #[test_io]
    fn io(source: &str, r#in: &[vm::VMInt], expected_out: &[vm::VMInt]) {
        use std::mem;
        use vm::ICMachine;

        let ast = parser::parse(source, &mut RootAccumulator::<parser::ParseError>::new()).unwrap();
        let mut errors = RootAccumulator::<AssembleError>::new();
        let mut code = Code::new(&mut errors);
        code.push_unit(ast);
        let code = code.codegen();
        mem::drop(errors);

        let mut vm = vm::ICMachineData::new(&code);
        for &v in r#in {
            vm.give_input(v).into_ok();
        }
        match vm.run() {
            vm::ICMachineStopState::EmptyInput => panic!("The vm was not satisfied with the input"),
            vm::ICMachineStopState::RuntimeErr(err) => {
                panic!("The vm stopped with an error: {err}")
            }
            vm::ICMachineStopState::Halted => {
                let mut out = vec![];
                while let Some(v) = vm.get_output() {
                    out.push(v)
                }
                assert_eq!(&out, expected_out, "The vm gave a different output")
            }
        }
    }
}
