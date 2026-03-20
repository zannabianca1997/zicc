//! Implementation of the type table

use std::{collections::HashMap, sync::RwLock};

use crate::TypeId;

/// A type table
///
/// This is a type table, keeping a mapping between types, [`TypeId`]s, and
/// names.
pub struct TypeTable<Name> {
    table: RwLock<Vec<TypeEntry>>,
    names: RwLock<HashMap<Name, TypeId>>,
}

struct TypeEntry {}
