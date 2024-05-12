//! Discovering all types defined

use std::collections::BTreeMap;

use ast::{
    ast_node::{AstNode, AstVisitor},
    expression::Expression,
    tokens::Identifier,
    typedef::{
        TypeDef, TypeDefArray, TypeDefData, TypeDefFn, TypeDefPointer, TypeDefStruct, TypeDefUnion,
    },
    File, ItemType,
};
use either::Either::{Left, Right};
use thiserror::Error;

use super::{BaseType, FieldOffsets, TypeEntry};

/// Reference to a type, either a base one or a id or named one
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ReferredType {
    Base(BaseType),
    IdOrName(IdOrName),
}
impl ReferredType {
    fn name(name: Identifier) -> Self {
        Self::IdOrName(IdOrName::Name(name))
    }

    fn id(id: usize) -> Self {
        Self::IdOrName(IdOrName::Id(id))
    }
}
/// Reference to a type (usize) or a name (identifier)
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum IdOrName {
    Id(usize),
    Name(Identifier),
}

/// An entry just registered, with only the infos that can be deducted from the definition
pub type DiscoveredEntry<'d> = TypeEntry<
    'd,
    ReferredType,
    FieldOffsets<ReferredType>,
    FieldOffsets<ReferredType>,
    &'d Expression,
    (),
>;

/// Walker that finds all defined names, and possible duplications
struct FindNames(BTreeMap<Identifier, Vec<Identifier>>);

impl FindNames {
    fn new() -> Self {
        Self(BTreeMap::new())
    }
}

impl AstVisitor<'_> for FindNames {
    type ChildVisitor = Self;

    type Result = ();

    fn enter(&mut self, node: &impl AstNode) -> Self::ChildVisitor {
        // Types can be defined in composites, or in item type
        if let Some(ItemType { ident: name, .. }) = node.as_item_type() {
            self.0.entry(*name).or_default().push(*name)
        }
        if let Some(TypeDefStruct {
            name: Some(name), ..
        }) = node.as_type_def_struct()
        {
            self.0.entry(*name).or_default().push(*name)
        }
        if let Some(TypeDefUnion {
            name: Some(name), ..
        }) = node.as_type_def_union()
        {
            self.0.entry(*name).or_default().push(*name)
        }

        Self::new()
    }

    fn exit(&mut self, _: &impl AstNode, child_visitor: Self::ChildVisitor) -> Self::Result {
        for (name, mut defs) in child_visitor.0 {
            self.0.entry(name).or_default().append(&mut defs)
        }
    }
}

/// Walker that finds all type definitions
struct Discoverer<'m, 'd> {
    types: &'m mut Vec<DiscoveredEntry<'d>>,
    names: &'m mut BTreeMap<Identifier, ReferredType>,
}
impl<'d> Discoverer<'_, 'd> {
    fn bind(&mut self, name: Identifier, reference: ReferredType) {
        debug_assert!(
            self.names.get(&name).is_none(),
            "Double definition are invalid and should be eliminated earlier than type registration"
        );
        self.names.insert(name, reference);
    }

    fn register_type_def(&mut self, def: &'d TypeDef) -> ReferredType {
        match def {
            TypeDef::Data(def) => self.register_data(def),
            TypeDef::Fn(def) => self.register_fn(def),
            TypeDef::Unknow(_) => ReferredType::Base(BaseType::Unknow),
        }
    }

    fn register_data(&mut self, def: &'d TypeDefData) -> ReferredType {
        match def {
            TypeDefData::Int(_) => ReferredType::Base(BaseType::Int),

            TypeDefData::Array(def) => self.register_array(def),
            TypeDefData::Struct(def) => self.register_struct(def),
            TypeDefData::Union(def) => self.register_union(def),
            TypeDefData::Pointer(def) => self.register_pointer(def),

            TypeDefData::Named(name) => ReferredType::name(*name),
        }
    }

    fn register_struct(&mut self, def: &'d TypeDefStruct) -> ReferredType {
        if def.fields.is_empty() {
            return ReferredType::Base(BaseType::Unit);
        }
        let mut fields = BTreeMap::new();
        let mut fields_sizes = vec![];
        for (name, _, ty) in def.fields.iter() {
            let ty = self.register_data(ty);
            if let Left(name) = name {
                fields.insert(
                    *name,
                    (FieldOffsets::sum_of_sizes(fields_sizes.iter().copied()), ty),
                );
            }
            fields_sizes.push(ty);
        }
        self.insert_entry(DiscoveredEntry::Composite {
            def: Left(def),
            name: def.name,
            fields,
            size: FieldOffsets::sum_of_sizes(fields_sizes),
        })
    }

    fn register_union(&mut self, def: &'d TypeDefUnion) -> ReferredType {
        if def.variants.is_empty() {
            return ReferredType::Base(BaseType::Unit);
        }
        let mut fields = BTreeMap::new();
        let mut variant_sizes = vec![];
        for (name, _, ty) in def.variants.iter() {
            let ty = self.register_data(ty);
            if let Left(name) = name {
                fields.insert(*name, (FieldOffsets::Value(0), ty));
            }
            variant_sizes.push(ty);
        }
        self.insert_entry(DiscoveredEntry::Composite {
            def: Right(def),
            name: def.name,
            fields,
            size: FieldOffsets::max_of_sizes(variant_sizes),
        })
    }

    fn register_fn(&mut self, def: &'d TypeDefFn) -> ReferredType {
        let inputs = def
            .inputs
            .iter()
            .map(|inp| self.register_data(inp))
            .collect();
        let output = def
            .output
            .as_ref()
            .map(|(_, out)| self.register_data(out))
            .unwrap_or(ReferredType::Base(BaseType::Unit));
        self.insert_entry(DiscoveredEntry::Fn {
            def,
            inputs,
            output,
        })
    }

    fn register_array(&mut self, def: &'d TypeDefArray) -> ReferredType {
        let element = self.register_data(&def.element);
        self.insert_entry(DiscoveredEntry::Array {
            def,
            element,
            lenght: &def.lenght,
            size: (),
        })
    }

    fn register_pointer(&mut self, def: &'d TypeDefPointer) -> ReferredType {
        let pointee = self.register_type_def(&def.pointee);
        self.insert_entry(DiscoveredEntry::Pointer {
            def,
            kind: def.kind.into(),
            pointee,
        })
    }

    fn insert_entry(&mut self, entry: DiscoveredEntry<'d>) -> ReferredType {
        self.types.push(entry);
        ReferredType::id(self.types.len() - 1)
    }
}

struct DiscovererShell<'m, 'd>(Option<Discoverer<'m, 'd>>);

impl<'d> AstVisitor<'d> for DiscovererShell<'_, 'd> {
    type ChildVisitor = Self;

    type Result = ();

    fn enter(&mut self, node: &'d impl ast::ast_node::AstNode) -> Self::ChildVisitor {
        let mut inner = self.0.take().unwrap();
        // Types can be defined in composites, or in item type
        if let Some(ItemType {
            ident: name, ty, ..
        }) = node.as_item_type()
        {
            let idx = inner.register_type_def(ty);
            inner.bind(*name, idx);
        }
        if let Some(
            ty @ TypeDefStruct {
                name: Some(name), ..
            },
        ) = node.as_type_def_struct()
        {
            let idx = inner.register_struct(ty);
            inner.bind(*name, idx);
        }
        if let Some(
            ty @ TypeDefUnion {
                name: Some(name), ..
            },
        ) = node.as_type_def_union()
        {
            let idx = inner.register_union(ty);
            inner.bind(*name, idx);
        }
        Self(Some(inner))
    }

    fn exit(
        &mut self,
        _: &'d impl ast::ast_node::AstNode,
        child: Self::ChildVisitor,
    ) -> Self::Result {
        // recovering the inner maps
        self.0 = Some(child.0.unwrap());
    }
}

#[derive(Debug, Clone, Error)]
#[error("Some identifiers are defined multiple times")]
pub struct DuplicateDefinitions(BTreeMap<Identifier, Vec<Identifier>>);

pub(super) fn discover<'d>(
    ast: &'d File,
) -> Result<(Vec<DiscoveredEntry<'d>>, BTreeMap<Identifier, ReferredType>), DuplicateDefinitions> {
    // Check no type is defined twice
    let mut find_names = FindNames::new();
    ast.visited_by(&mut find_names);
    find_names.0.retain(|_, occurrences| occurrences.len() > 1);
    if !find_names.0.is_empty() {
        return Err(DuplicateDefinitions(find_names.0));
    }

    // Discover all the types
    let mut types = Vec::new();
    let mut names = BTreeMap::new();
    ast.visited_by(&mut DiscovererShell(Some(Discoverer {
        types: &mut types,
        names: &mut names,
    })));
    Ok((types, names))
}
