use std::{collections::BTreeMap, convert::identity, mem};

use ast::{expression::Expression, tokens::Identifier};
use elsa::FrozenVec;
use thiserror::Error;

use crate::type_table_generation::discover::IdOrName;

use super::{discover, BaseType, FieldOffsets, MappingFunctions, TypeEntry};

/// Reference to a type, either a base one or a id or named one
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ReferredType {
    Base(BaseType),
    Id(usize),
}
impl ReferredType {
    pub fn try_into_id(self) -> Result<usize, Self> {
        if let Self::Id(v) = self {
            Ok(v)
        } else {
            Err(self)
        }
    }
}

/// An entry just registered, with only the infos that can be deducted from the definition
pub type Entry<'d> = TypeEntry<
    'd,
    ReferredType,
    FieldOffsets<ReferredType>,
    FieldOffsets<ReferredType>,
    &'d Expression,
    (),
>;

#[derive(Debug, Clone, Error)]
#[error("Could not solve all names")]
pub struct NameResolveError {
    /// Identifiers that loops on themselves
    looping: Vec<Identifier>,
    /// Identifiers that are referred but not defined
    undefined: Vec<Identifier>,
}

pub(crate) fn resolve<'d>(
    types: Vec<discover::DiscoveredEntry<'d>>,
    mut names: BTreeMap<Identifier, discover::ReferredType>,
) -> Result<(Vec<Entry<'d>>, BTreeMap<Identifier, ReferredType>), NameResolveError> {
    // first, we resolve all the names, leaving loops behind
    let mut solved_names = BTreeMap::new();
    let mut undefined = FrozenVec::new();
    loop {
        let current_names = names.clone();
        names.retain(|name, reference| match reference {
            discover::ReferredType::IdOrName(IdOrName::Name(name)) => {
                if let Some(next_reference) = current_names.get(&*name) {
                    *reference = *next_reference;
                    true // keep expanding
                } else {
                    undefined.push(*name);
                    false // remove the error
                }
            }
            discover::ReferredType::IdOrName(IdOrName::Id(id)) => {
                solved_names.insert(*name, ReferredType::Id(*id));
                false
            }
            discover::ReferredType::Base(base) => {
                solved_names.insert(*name, ReferredType::Base(*base));
                false
            }
        });

        if current_names.len() == names.len() {
            break;
        }
    }

    // solving types
    let map_references = |r: discover::ReferredType| -> ReferredType {
        match r {
            discover::ReferredType::Base(base) => ReferredType::Base(base),
            discover::ReferredType::IdOrName(IdOrName::Id(id)) => ReferredType::Id(id),
            discover::ReferredType::IdOrName(IdOrName::Name(name)) => match solved_names.get(&name)
            {
                Some(r) => *r,
                None => {
                    undefined.push(name);
                    ReferredType::Base(BaseType::Unknow)
                }
            },
        }
    };
    let types = types
        .into_iter()
        .map(|entry| {
            entry.map(MappingFunctions {
                f_rt: map_references,
                f_fo: |offset: FieldOffsets<discover::ReferredType>| {
                    offset.map_references(&mut &map_references)
                },
                f_cs: |size: FieldOffsets<discover::ReferredType>| {
                    size.map_references(&mut &map_references)
                },
                f_al: identity,
                f_as: identity,
            })
        })
        .collect();

    // erroring if something is amiss
    if !undefined.is_empty() || !names.is_empty() {
        Err(NameResolveError {
            looping: names.keys().copied().collect(),
            undefined: mem::take(undefined.as_mut()),
        })
    } else {
        Ok((types, solved_names))
    }
}
