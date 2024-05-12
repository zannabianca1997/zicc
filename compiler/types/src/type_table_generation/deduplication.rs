//! Deduplicate the discovered types

use std::{
    collections::{BTreeMap, BTreeSet},
    ops::Range,
};

use ast::tokens::Identifier;
use elsa::FrozenVec;
use itertools::Itertools;

use crate::{
    Type, TypeArray, TypeComposite, TypeData, TypeEntry, TypeFn, TypeId, TypeIdData, TypeInt,
    TypePointer, TypeUnknow, INT_ID, UNIT_ID, UNKNOW_ID,
};

use super::{name_resolution::ReferredType, size_calculation::Entry};

pub fn deduplicate<'d>(
    types: Vec<Entry<'d>>,
    names: BTreeMap<Identifier, ReferredType>,
) -> (FrozenVec<Box<TypeEntry>>, BTreeMap<Identifier, TypeId>) {
    // Map of ID to deduplicated IDs
    let mut id_map: Vec<usize> = (0..types.len()).collect();
    // Unique ids found
    let mut uniques: BTreeSet<usize> = BTreeSet::new();

    // While we find a id that is not inside uniques, add it to uniques...
    while let Some((pos, new_unique)) = id_map
        .iter()
        .copied()
        .enumerate()
        .rev() // start from the end
        .find(|(_, id)| uniques.insert(*id))
    {
        // now we must collect merge all the ids that match this one

        // Let's find all types that might merge into this one
        let possible_merges = id_map[..pos]
            .into_iter()
            .filter(|id|
                // this is is still unduplicated
                !uniques.contains(id)
                // the type of this id is at least superficially resemblant the one we are unduplicating
                && compatible(&types[**id], &types[new_unique]))
            .copied()
            .collect::<BTreeSet<_>>();

        // For all the unmerged indices
        for merged in possible_merges {
            // taking a snapshot so we can restore the state in case of failed merge
            let snapshot = id_map.clone();

            if merge_success(new_unique, merged, &mut id_map, &(0..pos), &types, &uniques) {
                // merged!
            } else {
                // restore the snapshot to before the merge
                id_map = snapshot
            }
        }
    }

    // Now all the ids are deduplicated if they are substituted with the ones in id_map.
    // Let's now remap them to sequential IDs, starting with 3 (0,1,2 are for the base types)

    // map from unique_ids to sequential ones
    let sequential_ids = uniques
        .iter()
        .enumerate()
        .map(|(seq, id)| (*id, seq + 3))
        .collect::<BTreeMap<_, _>>();

    let reference_to_type_id = |r: ReferredType| -> TypeId {
        match r {
            ReferredType::Base(b) => match b {
                crate::type_table_generation::BaseType::Int => INT_ID.into_id(),
                crate::type_table_generation::BaseType::Unknow => UNKNOW_ID,
                crate::type_table_generation::BaseType::Unit => UNIT_ID.into_id(),
            },
            ReferredType::Id(id) => {
                // first, we map it to the unique id it's been deduplicated to
                let unique = id_map[id];
                // then, we map it to the sequential id it will go
                TypeId(sequential_ids[&unique])
            }
        }
    };
    let reference_to_type_id_data = |r: ReferredType| -> TypeIdData {
        // This must be a datatype, as size calculation would have failed earlier
        TypeIdData(reference_to_type_id(r))
    };

    // moving the array into options, so we can remove and put them back at will
    let mut types: Vec<Option<Entry>> = types.into_iter().map(Some).collect();
    let mut final_types: Vec<Option<TypeEntry>> = vec![None; uniques.len() + 3];

    // adding the base types
    final_types[INT_ID.into_id().0] = Some(TypeEntry {
        typ: Type::Data(TypeData::Int(TypeInt)),
        name: None,
        size: Some(1),
    });
    final_types[UNKNOW_ID.0] = Some(TypeEntry {
        typ: Type::Unknow(TypeUnknow),
        name: None,
        size: None,
    });
    final_types[UNIT_ID.into_id().0] = Some(TypeEntry {
        typ: Type::Data(TypeData::Composite(TypeComposite {
            name: None,
            fields: BTreeMap::new(),
            size: 0,
        })),
        name: None,
        size: Some(0),
    });
    // adding the other type
    for unique_id in uniques {
        let ty = types[unique_id]
            .take()
            .expect("Uniques should guarantee no double take");
        use super::TypeEntry::*;
        let entry = match ty {
            Array {
                def: _,
                element,
                lenght,
                size,
            } => TypeEntry {
                typ: Type::Data(TypeData::Array(TypeArray {
                    element: reference_to_type_id_data(element),
                    lenght,
                })),
                name: None,
                size: Some(size),
            },
            Composite {
                def: _,
                name,
                fields,
                size,
            } => TypeEntry {
                typ: Type::Data(TypeData::Composite(TypeComposite {
                    name,
                    fields: fields
                        .into_iter()
                        .map(|(name, (offset, r))| (name, (offset, reference_to_type_id_data(r))))
                        .collect(),
                    size,
                })),
                name,
                size: Some(size),
            },
            Pointer {
                def: _,
                kind,
                pointee,
            } => TypeEntry {
                typ: Type::Data(TypeData::Pointer(TypePointer {
                    kind,
                    pointee: reference_to_type_id(pointee),
                })),
                name: None,
                size: Some(1),
            },
            Fn {
                def: _,
                inputs,
                output,
            } => TypeEntry {
                typ: Type::Fn(TypeFn {
                    inputs: inputs.into_iter().map(reference_to_type_id_data).collect(),
                    output: reference_to_type_id_data(output),
                }),
                name: None,
                size: Some(1),
            },
        };
        final_types[sequential_ids[&unique_id]] = Some(entry);
    }

    // unwrapping the final entries, boxing them and freezing
    let types = final_types
        .into_iter()
        .map(|o| Box::new(o.expect("All types should have been added")))
        .collect();

    // Finally, we map the names to the new ids
    let names = names
        .into_iter()
        .map(|(name, id)| (name, reference_to_type_id(id)))
        .collect();

    (types, names)
}

/// Try to merge the index `from` in the index `into`, by changing the `id_map` in the range `unduplicated`
/// Return `true` on successfull merge, `false` otherwise.
/// Non reentrant: on false `id_map` is modified in unspecified ways
fn merge_success(
    into: usize,
    from: usize,
    id_map: &mut [usize],
    unduplicated: &Range<usize>,
    types: &[Entry],
    uniques: &BTreeSet<usize>,
) -> bool {
    // Basic check on if the two types are even compatible
    if !compatible(&types[into], &types[from]) {
        return false;
    }

    // assuming now they are, merge them
    for id in &mut id_map[unduplicated.clone()] {
        if *id == from {
            *id = into
        }
    }

    let mut check_referred = move |into: ReferredType, from: ReferredType| -> bool {
        match (into, from) {
            (ReferredType::Base(a), ReferredType::Base(b)) => a == b,
            (ReferredType::Base(_), ReferredType::Id(_))
            | (ReferredType::Id(_), ReferredType::Base(_)) => false,
            (ReferredType::Id(a), ReferredType::Id(b)) if id_map[a] == id_map[b] => true,
            (ReferredType::Id(a), ReferredType::Id(b)) => {
                merge_success(id_map[a], id_map[b], id_map, unduplicated, types, uniques)
            }
        }
    };

    // now check if childs successfully merges
    match (&types[into], &types[from]) {
        // Array and pointers must only merge their element/pointee
        (Entry::Array { element: a, .. }, Entry::Array { element: b, .. })
        | (Entry::Pointer { pointee: a, .. }, Entry::Pointer { pointee: b, .. }) => {
            check_referred(*a, *b)
        }
        // Composites must merge their field type by name
        (Entry::Composite { fields: a, .. }, Entry::Composite { fields: b, .. }) => a
            .iter()
            .all(|(name, (_, a_ty))| check_referred(*a_ty, b[name].1)),
        // Functions must match inputs and output
        (
            Entry::Fn {
                inputs: a_inputs,
                output: a_output,
                ..
            },
            Entry::Fn {
                inputs: b_inputs,
                output: b_output,
                ..
            },
        ) => a_inputs
            .iter()
            .chain(Some(a_output))
            .zip(b_inputs.iter().chain(Some(b_output)))
            .all(|(a, b)| check_referred(*a, *b)),
        _ => unreachable!("Other combinations should fail the compatible test"),
    }
}

/// Non recursive type checks
fn compatible(into: &Entry, from: &Entry) -> bool {
    match (into, from) {
        // Array must match in lenght and size
        (
            Entry::Array {
                lenght: length_a,
                size: size_a,
                ..
            },
            Entry::Array {
                lenght: length_b,
                size: size_b,
                ..
            },
        ) => length_a == length_b && size_a == size_b,
        // Pointers must match in kind
        (Entry::Pointer { kind: kind_a, .. }, Entry::Pointer { kind: kind_b, .. }) => {
            kind_a == kind_b
        }
        // Composites must have the same name and size, and the named fields must be present in both and be at the same offset
        (
            Entry::Composite {
                name: name_a,
                fields: fields_a,
                size: size_a,
                ..
            },
            Entry::Composite {
                name: name_b,
                fields: fields_b,
                size: size_b,
                ..
            },
        ) => {
            name_a == name_b
                && size_a == size_b
                && fields_a
                    .keys()
                    .chain(fields_b.keys())
                    .unique()
                    .all(|field| {
                        if let (Some((off_a, _)), Some((off_b, _))) =
                            (fields_a.get(field), fields_b.get(field))
                        {
                            off_a == off_b
                        } else {
                            false
                        }
                    })
        }
        // Functions must match inputs lenght
        (
            Entry::Fn {
                inputs: a_inputs, ..
            },
            Entry::Fn {
                inputs: b_inputs, ..
            },
        ) => a_inputs.len() == b_inputs.len(),
        _ => false,
    }
}
