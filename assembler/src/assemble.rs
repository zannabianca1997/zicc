use std::{
    collections::{
        btree_map::Entry::{Occupied, Vacant},
        BTreeMap, BTreeSet,
    },
    mem,
};

use errors::{Accumulator, SourceError, Spanned};
use itertools::Itertools;
use thiserror::Error;

use crate::{
    ast::{Expression, File, IntsStm, LabelDef, LabelRef, Labelled, Statement},
    lexer::{Identifier, SpecialIdentifier},
};

pub struct Code<'s, 'e, E> {
    values: Vec<Expression<'s>>,
    labels: BTreeMap<Identifier<'s>, usize>,
    errors: &'e mut Accumulator<E>,
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
            Occupied(entry) => self
                .errors
                .push(AssembleError::DoubleDef(value.label.to_string())),
        }
    }

    pub fn new(errors: &'e mut Accumulator<E>) -> Self {
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
                    crate::ast::LabelRef::Identifier(id) => match self.labels.get(&id) {
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
                    crate::ast::LabelRef::SpecialIdentifier(SpecialIdentifier::Start) => Some(0),
                    crate::ast::LabelRef::SpecialIdentifier(SpecialIdentifier::End) => {
                        Some(code_end)
                    }
                    crate::ast::LabelRef::SpecialIdentifier(
                        SpecialIdentifier::UnitEnd | SpecialIdentifier::UnitStart,
                    ) => unreachable!(),
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
