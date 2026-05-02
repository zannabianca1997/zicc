#![doc = include_str!("../README.md")]

use std::{
    collections::{BTreeMap, BTreeSet},
    mem,
};

use snafu::Snafu;
use string_interner::DefaultStringInterner;
use zicc_assembler_ast::{
    expression::Expr,
    identifier::{Identifier, SpecialIdentifier},
};
use zicc_linker_program::{Program, ProgramInfo};
use zicc_value::Value;

pub mod cli;

/// Relocate a compilation unit from start
///
/// Rewrite a compilation unit to one that starts at `before`
fn relocate_unit_from_start(program: &mut Program, before: usize) {
    let before = Value::from(before);
    for value in program.content.iter_mut() {
        if let Expr::Offset {
            label: Identifier::Special(SpecialIdentifier::UnitStart),
            offset,
        } = &mut value.item
        {
            *offset += &before
        }
    }
}
/// Relocate a compilation unit from end
///
/// Rewrite a compilation unit to one that ends `after` values before the new
/// unit ending
fn relocate_unit_from_end(program: &mut Program, after: usize) {
    let after = Value::from(after);
    for value in program.content.iter_mut() {
        if let Expr::Offset {
            label: Identifier::Special(SpecialIdentifier::UnitEnd),
            offset,
        } = &mut value.item
        {
            *offset -= &after
        }
    }
}

/// Rewrite all the anonymous identifiers
///
/// Starts from the `start_at`, and move it forward to track the first free
fn reassign_anonymous(program: &mut Program, start_at: &mut u32) {
    let mut mapping = BTreeMap::new();
    let mut mapping = move |code| {
        *mapping.entry(code).or_insert_with(|| {
            let value = *start_at;
            *start_at += 1;
            value
        })
    };

    for value in program.content.iter_mut() {
        if let Expr::Offset {
            label: Identifier::Unnamed { code },
            ..
        } = &mut value.item
        {
            *code = mapping(*code);
        }

        value.labels = mem::take(&mut value.labels)
            .into_iter()
            .map(|l| {
                if let Identifier::Unnamed { code } = l {
                    Identifier::Unnamed {
                        code: mapping(code),
                    }
                } else {
                    l
                }
            })
            .collect()
    }
}

/// Add to all the identifier with a provenance another provenance, making them
/// separate from all others.
fn namespace_provenances(
    program: &mut Program,
    namespace: &[u8],
    interner: &mut DefaultStringInterner,
) {
    for value in program.content.iter_mut() {
        if let Expr::Offset {
            label:
                Identifier::Named {
                    provenance: Some(provenance),
                    ..
                },
            ..
        } = &mut value.item
        {
            *provenance = provenance.namespaced_to(interner, namespace);
        }

        value.labels = mem::take(&mut value.labels)
            .into_iter()
            .map(|mut l| {
                if let Identifier::Named {
                    provenance: Some(provenance),
                    ..
                } = &mut l
                {
                    *provenance = provenance.namespaced_to(interner, namespace);
                }
                l
            })
            .collect()
    }
}

fn join_info(infos: impl IntoIterator<Item = ProgramInfo>) -> ProgramInfo {
    infos.into_iter().next().unwrap()
}

/// Link multiple units together
///
/// Take multiple units and link them together in a single, larger unit. Will
/// keep public (without provenance) identifiers as they are, while relabelling
/// anonymous ones and adding new provenances to the private identifiers.
pub fn link(
    mut units: Vec<Program>,
    interner: &mut DefaultStringInterner,
) -> Result<Program, LinkError> {
    // Resolve identifier collisions
    let mut next_anonymous = 0;
    for (i, unit) in units.iter_mut().enumerate() {
        // Namespace all private identifiers
        namespace_provenances(unit, &i.to_be_bytes(), interner);
        // Reassign all anonymous to numerated ones
        reassign_anonymous(unit, &mut next_anonymous);
    }

    // Fix all units `unit_start`
    let mut before_unit = 0;
    for unit in units.iter_mut() {
        relocate_unit_from_start(unit, before_unit);
        before_unit += unit.len();
    }

    // Fix all units `unit_end`
    let mut after_unit = 0;
    for unit in units.iter_mut().rev() {
        relocate_unit_from_end(unit, after_unit);
        after_unit += unit.len();
    }

    debug_assert_eq!(before_unit, after_unit);

    let mut final_artifact = Vec::with_capacity(before_unit);
    let mut infos = Vec::with_capacity(units.len());

    for Program { info, mut content } in units {
        infos.push(info);
        final_artifact.append(&mut content);
    }

    Ok(Program {
        info: join_info(infos),
        content: final_artifact,
    })
}

#[derive(Debug, Snafu)]
pub enum LinkError {}

/// Generate the prelude
fn prelude(interner: &mut DefaultStringInterner) -> Program {
    // Prelude is:
    // ```
    // INB  #$end
    // CALL #main
    // HLT
    // ```
    // that assembles to:
    // ```
    //     INB #$end + 1           ; put RB at the start of the stack plus 1
    //     ADD #$4294967295 #0 @-1 ; put return address on the stack
    //     JEZ #0 #main            ; jump to main
    // $4294967295: HLT            ; halt
    //```
    // using `u32::MAX` as the anonymous label, as they where rewritten and it
    // would crash way before reaching that number
    Program::parse(
        b"
                         109, $end+1,
                       21101, $4294967295,    0, -1,
                        1106, 0,           main,
           $4294967295:   99
        ",
        interner,
    )
    .unwrap()
}

fn executable_info(
    ProgramInfo {
        name,
        author,
        metadata,
        ..
    }: ProgramInfo,
) -> zicc_vm_program::ProgramInfo {
    zicc_vm_program::ProgramInfo {
        name,
        author,
        metadata,
        ..Default::default()
    }
}

pub fn make_executable(
    program: Program,
    interner: &mut DefaultStringInterner,
) -> Result<zicc_vm_program::Program, MakeExecutableError> {
    let Program {
        info: _,
        content: mut prelude,
    } = prelude(interner);
    let prelude_len = prelude.len();
    let Program { info, mut content } = program;
    prelude.append(&mut content);
    let content = prelude;

    let labels: BTreeMap<_, _> = [
        (Identifier::Special(SpecialIdentifier::Start), prelude_len),
        (
            Identifier::Special(SpecialIdentifier::UnitStart),
            prelude_len,
        ),
        (Identifier::Special(SpecialIdentifier::End), content.len()),
        (
            Identifier::Special(SpecialIdentifier::UnitEnd),
            content.len(),
        ),
    ]
    .into_iter()
    .chain(
        content
            .iter()
            .enumerate()
            .flat_map(|(pos, value)| value.labels.iter().map(move |l| (*l, pos))),
    )
    .map(|(l, pos)| (l, Value::from(pos)))
    .collect();

    let mut undefined_labels = BTreeSet::new();
    let content = content
        .into_iter()
        .flat_map(|val| match val.item {
            Expr::Constant { value } => Some(Value::from(value)),
            Expr::Offset { label, offset } => {
                if let Some(defined_at) = labels.get(&label) {
                    Some(defined_at + &Value::from(offset))
                } else {
                    undefined_labels.insert(label);
                    None
                }
            }
        })
        .collect();

    if !undefined_labels.is_empty() {
        return Err(MakeExecutableError::UndefinedLabels {
            labels: undefined_labels,
        });
    }

    Ok(zicc_vm_program::Program {
        info: executable_info(info),
        content,
    })
}

#[derive(Debug, Snafu)]
pub enum MakeExecutableError {
    UndefinedLabels { labels: BTreeSet<Identifier> },
}
