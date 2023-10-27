#![feature(box_into_inner)]
#![feature(unwrap_infallible)]

use std::{
    collections::{
        btree_map::Entry::{Occupied, Vacant},
        BTreeMap, BTreeSet,
    },
    mem,
};

use either::Either::{self, Left};
use itertools::Itertools;
use parse_from_rust::ica;
use thiserror::Error;

use errors::{Accumulator, RootAccumulator};
use lexer::{Identifier, SpecialIdentifier, StringLit};
use parser::ast::{
    AstNode, CallStm, DecStm, Expression, File, IncStm, Instruction, IntsStm, JmpStm, LabelDef,
    LabelRef, Labelled, MovStm, PopStm, PushStm, ReadParam, RetStm, Statement, ZerosStm,
};
use vm::VMInt;

pub struct Code<'s, 'e, E> {
    values: Vec<Expression<'s>>,
    labels: BTreeMap<Identifier<'s>, usize>,
    next_unused_label: usize,
    errors: &'e mut RootAccumulator<E>,
}
impl<'s, 'e, E> Code<'s, 'e, E> {
    fn push_value(&mut self, value: Expression<'s>) {
        self.values.push(value)
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
            next_unused_label: 0,
        }
    }

    pub fn push_unit(&mut self, ast: File<'s>)
    where
        E: From<AssembleError>,
    {
        let unit_start = self.values.len();
        let mut interface = UnitPushingInterface {
            code: self,
            label_mapping: BTreeMap::new(),
            next_unused_label: ast.max_unnamed_label().map(|l| l + 1).unwrap_or_default(),
            unit_start: unit_start
                .try_into()
                .expect("The lenght should not overflow VMInt"),
        };
        ast.write_to(&mut interface);

        let unit_end = self.values.len();
        for value in &mut self.values[unit_start..unit_end] {
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
                self.errors.handle(e.calculate(&mut |id| {
                    match id {
                        LabelRef::Identifier(id) => match self.labels.get(&id) {
                            Some(pos) => Ok((*pos)
                                .try_into()
                                .expect("The lenght should not overflow VMInt")),
                            None => Err(AssembleError::UnknowLabel(id.to_string())),
                        },
                        LabelRef::SpecialIdentifier(SpecialIdentifier::End) => Ok(code_end),
                        LabelRef::SpecialIdentifier(SpecialIdentifier::Start)
                        | LabelRef::SpecialIdentifier(
                            SpecialIdentifier::UnitEnd | SpecialIdentifier::UnitStart,
                        ) => unreachable!(),
                        LabelRef::Error(e) => *e,
                    }
                }))
            })
            .collect_vec()
    }
}

pub struct UnitPushingInterface<'c, 's, 'e, E> {
    code: &'c mut Code<'s, 'e, E>,
    label_mapping: BTreeMap<usize, usize>,
    next_unused_label: usize,
    unit_start: VMInt,
}
impl<'c, 's, 'e, E> UnitPushingInterface<'c, 's, 'e, E> {
    fn push_value(&mut self, mut value: Expression<'s>) {
        value.replace(LabelRef::SpecialIdentifier(SpecialIdentifier::Start), 0);
        value.replace(
            LabelRef::SpecialIdentifier(SpecialIdentifier::UnitStart),
            self.unit_start,
        );
        self.code.push_value(value.constant_folding())
    }
    fn push_num(&mut self, value: VMInt) {
        self.code.push_value(Expression::Num(value))
    }
    fn push_label(&mut self, mut value: LabelDef<'s>)
    where
        E: From<AssembleError>,
    {
        // remap unnamed labels to avoid collision with different units
        if let Identifier::Unnamed(name, _) = &mut value.label {
            *name = *self.label_mapping.entry(*name).or_insert_with(|| {
                let label = self.code.next_unused_label;
                self.code.next_unused_label += 1;
                label
            })
        }
        self.code.push_label(value)
    }

    fn errors(&mut self) -> &mut impl Accumulator<Error = E> {
        self.code.errors
    }

    /// Get a new unnamed label that's not used in the unit being pushed
    fn new_unnamed_label(&mut self) -> Identifier<'static> {
        let label = self.next_unused_label;
        self.next_unused_label += 1;
        Identifier::Unnamed(label, (0, 0))
    }

    fn eval_expr(&self, e: Box<Expression<'_>>) -> Result<i64, AssembleError> {
        e.calculate(&mut |id| match id {
            LabelRef::Identifier(mut id) => {
                if let Identifier::Unnamed(name, _) = &mut id {
                    if let Some(mapped) = self.label_mapping.get(name) {
                        *name = *mapped
                    } else {
                        return Err(AssembleError::UnknowLabel(id.to_string()));
                    }
                }
                match self.code.labels.get(&id) {
                    Some(pos) => Ok((*pos)
                        .try_into()
                        .expect("The lenght should not overflow VMInt")),
                    None => Err(AssembleError::UnknowLabel(id.to_string())),
                }
            }
            LabelRef::SpecialIdentifier(s) => Err(AssembleError::SpecialIdentifierInConstExpr(*s)),
            LabelRef::Error(e) => *e,
        })
    }
}

trait WriteTo<'s, 'e, E> {
    fn write_to(self, code: &mut UnitPushingInterface<'_, 's, 'e, E>);
}

impl<'s, 'e, E> WriteTo<'s, 'e, E> for File<'s>
where
    E: From<AssembleError>,
{
    fn write_to(self, code: &mut UnitPushingInterface<'_, 's, 'e, E>) {
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
    fn write_to(self, code: &mut UnitPushingInterface<'_, 's, 'e, E>)
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
    fn write_to(self, code: &mut UnitPushingInterface<'_, 's, 'e, E>)
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
    fn write_to(self, code: &mut UnitPushingInterface<'_, 's, 'e, E>) {
        if let Some(this) = self {
            this.write_to(code)
        }
    }
}
impl<'s, 'e, E, T> WriteTo<'s, 'e, E> for Box<T>
where
    T: WriteTo<'s, 'e, E>,
{
    fn write_to(self, code: &mut UnitPushingInterface<'_, 's, 'e, E>) {
        Box::into_inner(self).write_to(code)
    }
}

macro_rules! write_to_for_statement {
    ( $($variant:ident)*) => {
        impl<'s, 'e, E> WriteTo<'s, 'e, E> for Statement<'s>
        where
            E: From<AssembleError>,
        {
            fn write_to(self, code: &mut UnitPushingInterface<'_, 's, 'e, E>) {
                match self {
                    $(
                        Statement::$variant(stm) => WriteTo::<'s, 'e, E>::write_to(stm,code),
                    )*
                    Statement::Error(e) => e,
                }
            }
        }
    };
}

write_to_for_statement! {
    Ints Instruction Inc Dec Jmp Mov Zeros Push Pop Call Ret
}

impl<'s, 'e, E> WriteTo<'s, 'e, E> for IntsStm<'s>
where
    E: From<AssembleError>,
{
    fn write_to(self, code: &mut UnitPushingInterface<'_, 's, 'e, E>) {
        for v in self.values {
            v.write_to(code)
        }
    }
}
impl<'s, 'e, E> WriteTo<'s, 'e, E> for StringLit<'s> {
    fn write_to(self, code: &mut UnitPushingInterface<'_, 's, 'e, E>) {
        for value in self {
            code.push_value(Expression::Num(value))
        }
    }
}

impl<'s, 'e, E> WriteTo<'s, 'e, E> for Instruction<'s>
where
    E: From<AssembleError>,
{
    fn write_to(self, code: &mut UnitPushingInterface<'_, 's, 'e, E>) {
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
    fn write_to(self, code: &mut UnitPushingInterface<'_, 's, 'e, E>) {
        ica!(
            add {self.0.clone()} #1 {self.0}
        )
        .write_to(code)
    }
}
impl<'s, 'e, E> WriteTo<'s, 'e, E> for DecStm<'s>
where
    E: From<AssembleError>,
{
    fn write_to(self, code: &mut UnitPushingInterface<'_, 's, 'e, E>) {
        ica!(
            add {self.0.clone()} #-1 {self.0}
        )
        .write_to(code)
    }
}
impl<'s, 'e, E> WriteTo<'s, 'e, E> for JmpStm<'s>
where
    E: From<AssembleError>,
{
    fn write_to(self, code: &mut UnitPushingInterface<'_, 's, 'e, E>) {
        ica!(
            jz #0 {self.0}
        )
        .write_to(code)
    }
}
impl<'s, 'e, E> WriteTo<'s, 'e, E> for MovStm<'s>
where
    E: From<AssembleError>,
{
    fn write_to(self, code: &mut UnitPushingInterface<'_, 's, 'e, E>) {
        match self {
            MovStm::Single(a, b) => ica!(
                add {a} #0 {b}
            )
            .write_to(code),
            MovStm::Multiple(a, b, n) => {
                let n = code.eval_expr(n);
                if let Some(n) = code.errors().handle(n) {
                    for i in 0..n {
                        ica!(
                            mov
                                {
                                    a.clone()
                                    .map_value(|v| Box::new(Expression::Sum(v, Box::new(Expression::Num(i)))))
                                }
                                {
                                    b.clone()
                                    .map_value(|v| Box::new(Expression::Sum(v, Box::new(Expression::Num(i)))))
                                }
                    )
                    .write_to(code)
                    }
                }
            }
        }
    }
}
impl<'s, 'e, E> WriteTo<'s, 'e, E> for ZerosStm<'s>
where
    E: From<AssembleError>,
{
    fn write_to(self, code: &mut UnitPushingInterface<'_, 's, 'e, E>) {
        let n = code.eval_expr(self.0);
        if let Some(n) = code.errors().handle(n) {
            if let Ok(n) = usize::try_from(n) {
                IntsStm {
                    values: vec![
                        Labelled {
                            labels: BTreeSet::new(),
                            content: Left(Box::new(Expression::Num(0)))
                        };
                        n
                    ],
                }
                .write_to(code)
            }
        }
    }
}

impl<'s, 'e, E> WriteTo<'s, 'e, E> for PushStm<'s>
where
    E: From<AssembleError>,
{
    fn write_to(self, code: &mut UnitPushingInterface<'_, 's, 'e, E>) {
        match self {
            PushStm::Single(p) => ica!(
                mov {p} @0;
                incb #1
            ),
            PushStm::Multiple(p, n) => ica!(
                mov {p} @0 {n.clone()};
                incb #{n}
            ),
        }
        .write_to(code)
    }
}
impl<'s, 'e, E> WriteTo<'s, 'e, E> for PopStm<'s>
where
    E: From<AssembleError>,
{
    fn write_to(self, code: &mut UnitPushingInterface<'_, 's, 'e, E>) {
        match self {
            PopStm::Single(p) => ica!(
                mov @-1 {p};
                incb #-1
            ),
            PopStm::Multiple(p, n) => ica!(
                mov  @-{n.clone()} {p} {n.clone()};
                incb #-{n}
            ),
        }
        .write_to(code)
    }
}
impl<'s, 'e, E> WriteTo<'s, 'e, E> for CallStm<'s>
where
    E: From<AssembleError>,
{
    fn write_to(self, code: &mut UnitPushingInterface<'_, 's, 'e, E>) {
        let Self(mut p) = self;
        if let ReadParam::Relative(rp) = &mut p {
            // jmp will be executed after the push, we have to adjust the parameter to make it point to the same location
            let expr = Box::as_mut(&mut rp.value.content);
            let taken = mem::replace(expr, Expression::Num(0xdeadbeef));
            *expr =
                Expression::Sub(Box::new(taken), Box::new(Expression::Num(1))).constant_folding();
        }
        let label: Identifier = code.new_unnamed_label();
        ica!(
            push #{Labelled::from(Box::new(Expression::Ref(LabelRef::Identifier(label))))};
            jmp {p}
        )
        .write_to(code);
        code.push_label(LabelDef { label })
    }
}
impl<'s, 'e, E> WriteTo<'s, 'e, E> for RetStm
where
    E: From<AssembleError>,
{
    fn write_to(self, code: &mut UnitPushingInterface<'_, 's, 'e, E>) {
        ica!(
            incb #-1;
            jmp @0
        )
        .write_to(code);
    }
}

impl<'s, 'e, E> WriteTo<'s, 'e, E> for Expression<'s> {
    fn write_to(self, code: &mut UnitPushingInterface<'_, 's, 'e, E>) {
        code.push_value(self)
    }
}
impl<'s, 'e, E> WriteTo<'s, 'e, E> for LabelDef<'s>
where
    E: From<AssembleError>,
{
    fn write_to(self, code: &mut UnitPushingInterface<'_, 's, 'e, E>) {
        code.push_label(self)
    }
}

#[derive(Debug, Clone, Error)]
pub enum AssembleError {
    #[error("Label {0} was already in use")]
    DoubleDef(String),
    #[error("Unknow label {0}")]
    UnknowLabel(String),
    #[error("Special identifiers are not supported in const expession")]
    SpecialIdentifierInConstExpr(SpecialIdentifier),
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

        let mut vm = vm::ICMachineData::new(dbg!(&code));
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
