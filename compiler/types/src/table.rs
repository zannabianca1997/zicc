//! Implementation of the type table

use std::{borrow::Borrow, collections::HashMap, hash::Hash, num::NonZeroUsize, sync::RwLock};

use derive_more::Constructor;
use zicc_limits::Size;

use crate::{Array, Int, Pointer, SizedType, SizedTypeId, Type, TypeId};

#[cfg(test)]
mod tests;

/// A type table
///
/// This is a type table, keeping a mapping between types, [`TypeId`]s, and
/// names.
pub struct TypeTable<Name> {
    table: RwLock<Vec<TypeEntry>>,
    names: RwLock<HashMap<Name, TypeId>>,
}

#[derive(Debug, Clone, Constructor)]
struct TypeEntry {
    ty: Type,
    size: Option<Size>,
}

impl TypeEntry {
    /// Dummy type entry for the first entry in the table
    fn placeholder() -> Self {
        Self {
            ty: Type::unknown(),
            size: Some(0),
        }
    }
}

impl<Name> TypeTable<Name> {
    /// Create a new table
    ///
    /// The new table created this way can only define non recursive types.
    pub fn new() -> Self {
        Self {
            table: RwLock::new(table_prelude()),
            names: RwLock::new(HashMap::new()),
        }
    }

    /// Get the [`TypeId`] of a given [`Type`]
    pub fn id(&self, ty: &Type) -> TypeId {
        table_get_or_insert(
            &self.table,
            |e| &e.ty == ty,
            |table| TypeEntry::new(ty.clone(), size_of_type(table, ty)),
        )
    }

    /// Get the [`SizedTypeId`] of a given [`SizedType`]
    ///
    /// Sized version of [`Self::id`]
    pub fn id_sized(&self, ty: &SizedType) -> SizedTypeId {
        SizedTypeId(table_get_or_insert(
            &self.table,
            |e| e.ty.try_unwrap_sized_ref() == Ok(ty),
            |table| TypeEntry::new(ty.clone().into(), Some(size_of_sized_type(table, ty))),
        ))
    }

    /// Get the [`Type`] from a [`TypeId`]
    ///
    /// Due to internal locking, this cannot return a reference
    pub fn r#type(&self, id: TypeId) -> Type {
        self.table
            .read()
            .expect("Poisoned lock on the type table")
            .get(id.0.get())
            .expect("Invalid type id. Did you create two type tables?")
            .ty
            .clone()
    }

    /// Get the [`SizedType`] from a [`SizedTypeId`]
    ///
    /// Sized version of [`Self::r#type`]
    pub fn type_sized(&self, id: SizedTypeId) -> SizedType {
        self.r#type(id.into())
            .try_unwrap_sized()
            .expect("Sized type id points to unsized type. Did you create two type tables?")
    }

    /// Try to convert a [`TypeId`] into a [`SizedTypeId`]
    ///
    /// This will directly check the table and return [`Some`] if the type is
    /// sized
    pub fn try_unwrap_sized_id(&self, id: TypeId) -> Option<SizedTypeId> {
        self.table
            .read()
            .expect("Poisoned lock on the type table")
            .get(id.0.get())
            .expect("Invalid type id. Did you create two type tables?")
            .ty
            .try_unwrap_sized_ref()
            .ok()
            .map(|_| SizedTypeId(id))
    }

    /// Resolve a name
    ///
    /// The parameter may be any borrowed form of the name, but [`Hash`] and
    /// [`Eq`] on the borrowed form must match those for the name type.
    pub fn named<Q>(&self, name: &Q) -> Option<TypeId>
    where
        Q: Hash + Eq,
        Name: Hash + Eq + Borrow<Q>,
    {
        self.names
            .read()
            .expect("Poisoned lock on the name table")
            .get(name)
            .copied()
    }

    /// Define a name for a type
    ///
    /// After this function is called [`Self::named`] can be used to retrieve
    /// the type defined.
    pub fn define(&self, name: Name, id: TypeId) -> Result<(), (Name, TypeId)>
    where
        Name: Hash + Eq,
    {
        let mut names = self.names.write().expect("Poisoned lock on the name table");

        if let Some(occupied) = names.get(&name) {
            return Err((name, *occupied));
        }

        names.insert(name, id);

        Ok(())
    }

    /// Size of a type
    pub fn size_of(&self, id: TypeId) -> Option<Size> {
        size_of(
            &self.table.read().expect("Poisoned lock on the type table"),
            id,
        )
    }

    /// Size of a sized type
    pub fn size_of_sized(&self, id: SizedTypeId) -> Size {
        size_of_sized(
            &self.table.read().expect("Poisoned lock on the type table"),
            id,
        )
    }

    /// Size of a type
    pub fn size_of_type(&self, ty: &Type) -> Option<Size> {
        size_of_type(
            &self.table.read().expect("Poisoned lock on the type table"),
            ty,
        )
    }

    /// Size of a sized type
    pub fn size_of_sized_type(&self, ty: &SizedType) -> Size {
        size_of_sized_type(
            &self.table.read().expect("Poisoned lock on the type table"),
            ty,
        )
    }
}

/// Begin of the type table
///
/// Contain the placeholder to make the [`TypeId`]s one-based (for niche
/// optimizations) and the primitive types
fn table_prelude() -> Vec<TypeEntry> {
    vec![
        TypeEntry::placeholder(),
        TypeEntry::new(Type::int(), Some(1)),
        TypeEntry::new(Type::unknown(), None),
    ]
}

/// Get the id of an entry, or create it if missing
///
/// This will lock the table in write mode only if the requested entry does not
/// exists
fn table_get_or_insert(
    table: &RwLock<Vec<TypeEntry>>,
    mut predicate: impl FnMut(&TypeEntry) -> bool,
    default: impl FnOnce(&[TypeEntry]) -> TypeEntry,
) -> TypeId {
    // If this type is known, get the id
    let read_lock = table.read().expect("Poisoned lock on the type table");

    if let Some((id, _)) = read_lock
        .iter()
        .enumerate()
        .skip(1)
        .find(|e| predicate(e.1))
    {
        return TypeId(NonZeroUsize::new(id).unwrap());
    }

    let checked_at_read = read_lock.len();
    drop(read_lock);

    // New type, need to add it
    // Re-locking the table in write mode
    let mut write_lock = table.write().expect("Poisoned lock on the type table");

    // Check that the id was not inserted while we were re-locking the table
    if let Some((id, _)) = write_lock[checked_at_read..]
        .iter()
        .enumerate()
        .find(|e| predicate(e.1))
    {
        return TypeId(NonZeroUsize::new(checked_at_read + id).unwrap());
    }

    // Inserting the new entry

    let entry = default(&write_lock);

    let id = write_lock.len();
    write_lock.push(entry);

    TypeId(NonZeroUsize::new(id).unwrap())
}

// Implementations with the table already borrowed

fn size_of(table: &[TypeEntry], id: TypeId) -> Option<Size> {
    table
        .get(id.0.get())
        .expect("Invalid type id. Did you create two type tables?")
        .size
}
fn size_of_sized(table: &[TypeEntry], id: SizedTypeId) -> Size {
    size_of(table, id.into()).unwrap()
}
fn size_of_sized_type(table: &[TypeEntry], ty: &SizedType) -> Size {
    match ty {
        SizedType::Int(_) => Int::size(),
        SizedType::Pointer(_) => Pointer::size(),
        SizedType::Array(Array { element, length }) => length * size_of_sized(table, *element),
    }
}
fn size_of_type(table: &[TypeEntry], ty: &Type) -> Option<Size> {
    ty.try_unwrap_sized_ref()
        .ok()
        .map(|ty| size_of_sized_type(table, ty))
}
