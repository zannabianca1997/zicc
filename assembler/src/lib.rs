#![feature(char_indices_offset)]
pub mod tokens;

pub mod ast;

pub mod assemble {
    use std::{
        collections::{
            btree_map::{
                Entry::{Occupied, Vacant},
                OccupiedEntry,
            },
            BTreeMap, BTreeSet,
        },
        mem,
    };

    use errors::{Accumulator, SourceError, Spanned};
    use itertools::Itertools;
    use thiserror::Error;

    use crate::{
        ast::{Expression, File, IntsStm, LabelDef, Labelled, Statement},
        tokens::Identifier,
    };

    struct Code<'s> {
        value: Vec<(BTreeSet<LabelDef<'s>>, Expression<'s>)>,
        labels: BTreeSet<LabelDef<'s>>,
    }
    impl<'s> Code<'s> {
        fn push_value(&mut self, value: Expression<'s>) {
            self.value.push((mem::take(&mut self.labels), value))
        }
        fn push_label(&mut self, value: LabelDef<'s>) {
            self.labels.insert(value);
        }

        fn new() -> Code<'s> {
            Code {
                value: vec![],
                labels: BTreeSet::new(),
            }
        }

        fn codegen(self, errors: &mut Accumulator<impl From<AssembleError>>) -> Vec<vm::VMInt> {
            let mut labels: BTreeMap<&'s str, (usize, LabelDef<'s>)> = BTreeMap::new();
            let values = self
                .value
                .into_iter()
                .enumerate()
                .map(|(i, (lbls, expr))| {
                    for lbl_def @ LabelDef {
                        label: Identifier { value: name, .. },
                        ..
                    } in lbls
                    {
                        match labels.entry(name) {
                            Vacant(vacant) => {
                                vacant.insert((i, lbl_def));
                            }
                            Occupied(occupied) => {
                                let prev_def = occupied.get().1;
                                errors
                                    .push(AssembleError::DoubleDef(lbl_def.span(), prev_def.span()))
                            }
                        }
                    }
                    expr
                })
                .collect_vec();
            values
                .into_iter()
                .flat_map(|e| {
                    e.calculate(&mut |id| match labels.get(id.as_str()) {
                        Some((v, _)) => Some(
                            (*v).try_into()
                                .expect("The lenght should not overflow VMInt"),
                        ),
                        None => {
                            errors.push(AssembleError::UnknowLabel(id.span()));
                            None
                        }
                    })
                })
                .collect_vec()
        }
    }

    trait WriteTo<'s> {
        fn write_to(self, code: &mut Code<'s>);
    }

    impl<'s> WriteTo<'s> for File<'s> {
        fn write_to(self, code: &mut Code<'s>) {
            for s in self.statements {
                s.write_to(code)
            }
        }
    }

    impl<'s, T> WriteTo<'s> for Labelled<'s, T>
    where
        T: WriteTo<'s>,
    {
        fn write_to(self, code: &mut Code<'s>) {
            for def in self.labels {
                def.write_to(code)
            }
            self.content.write_to(code)
        }
    }

    impl<'s, T> WriteTo<'s> for Option<T>
    where
        T: WriteTo<'s>,
    {
        fn write_to(self, code: &mut Code<'s>) {
            if let Some(this) = self {
                this.write_to(code)
            }
        }
    }

    impl<'s> WriteTo<'s> for Statement<'s> {
        fn write_to(self, code: &mut Code<'s>) {
            match self {
                Statement::IntsStm(ints) => ints.write_to(code),
            }
        }
    }

    impl<'s> WriteTo<'s> for IntsStm<'s> {
        fn write_to(self, code: &mut Code<'s>) {
            for v in self.values {
                v.write_to(code)
            }
        }
    }
    impl<'s> WriteTo<'s> for Expression<'s> {
        fn write_to(self, code: &mut Code<'s>) {
            code.push_value(self)
        }
    }
    impl<'s> WriteTo<'s> for LabelDef<'s> {
        fn write_to(self, code: &mut Code<'s>) {
            code.push_label(self)
        }
    }

    #[derive(Debug, Clone, Error)]
    pub enum AssembleError {
        #[error("Label was already in use")]
        DoubleDef(std::ops::Range<usize>, std::ops::Range<usize>),
        #[error("Unknow label")]
        UnknowLabel(std::ops::Range<usize>),
    }
    impl Spanned for AssembleError {
        fn span(&self) -> std::ops::Range<usize> {
            match self {
                AssembleError::DoubleDef(s, _) | AssembleError::UnknowLabel(s) => s.clone(),
            }
        }
    }

    impl SourceError for AssembleError {
        fn severity(&self) -> errors::Severity {
            errors::Severity::Error
        }
    }

    pub fn assemble(
        file: File,
        errors: &mut Accumulator<impl From<AssembleError>>,
    ) -> Vec<vm::VMInt> {
        let mut code = Code::new();
        file.write_to(&mut code);
        code.codegen(errors)
    }
}
