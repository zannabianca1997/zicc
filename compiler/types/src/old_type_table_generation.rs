use std::collections::{BTreeMap, BTreeSet};

use either::Either::{self, Left, Right};
use itertools::Itertools;
use slotmap::{new_key_type, SecondaryMap, SlotMap, SparseSecondaryMap};
use thiserror::Error;

use super::{
    PointerKind, Type, TypeArray, TypeComposite, TypeData, TypeEntry, TypeFn, TypeId, TypeIdData,
    TypeInt, TypePointer, TypeTable, TypeUnknow, INT_ID, UNIT_ID, UNKNOW_ID,
};
use ast::{
    ast_node::{AstNode, AstVisitor},
    expression::{Expression, SizeExpressionSolver},
    tokens::Identifier,
    typedef::{
        TypeDef, TypeDefArray, TypeDefData, TypeDefFn, TypeDefPointer, TypeDefStruct, TypeDefUnion,
        TypeDefUnknow,
    },
    File, ItemType,
};
use vm::VMUInt;

#[derive(Debug, Error)]
pub enum TypeDeclareError {
    #[error("Named types with no definition")]
    TypesWithNoDefinition(BTreeSet<Identifier>),
    #[error(transparent)]
    RecursiveTypeSizeError(#[from] SizeError),
    #[error("Named type with different, non equivalent definitions")]
    DifferentDefinitions {
        name: Identifier,
        defs: Vec<TypeDef>,
    },
}

#[derive(Debug, Error)]
pub enum SizeError {
    #[error("Cannor find size of recursive type")]
    RecursiveTypeSizeError,
    #[error("Cannot resolve array lenght")]
    UnsolvableArrayLenght(#[source] Box<dyn std::error::Error>),
    #[error("Array with elements of size {element_size} and length {lenght} is too big to fit in the address space")]
    OverflowingArraySize {
        element_size: VMUInt,
        lenght: VMUInt,
    },
    #[error("A struct is too big to fit in the address space")]
    StructTooLarge,
    #[error("An array has a invalid negative size")]
    NegativeArrayLenght(#[source] std::num::TryFromIntError),
    #[error("A unsized type was used as a component for a data type")]
    UnsizedComponents(TypeDef),
}

new_key_type! {
    /// Key types refer TO
    struct OutTypeId;

    /// Keys that refer to types
    struct InTypeId;
}

#[derive(Debug, Clone)]
/// Defereed expression for type sizes
enum TypeSizeExpr {
    Max(Vec<OutTypeId>),
    Sum(Vec<OutTypeId>),
}

#[derive(Debug, Default)]
/// Data accumulated in the registering phase
struct RegisteringPhaseMaps<'d> {
    /// Mapping from type referred to types or names
    idxs_mapping: SlotMap<OutTypeId, Either<InTypeId, Identifier>>,
    /// Type definitions
    type_defs: SlotMap<InTypeId, Either<&'d TypeDef, &'d TypeDefData>>,

    /// Multiple bindings of the same name, needs to end up in the same concrete id
    bindings: BTreeMap<Identifier, BTreeSet<OutTypeId>>,

    /// Array elements
    array_elements: SparseSecondaryMap<InTypeId, OutTypeId>,
    /// Array lengts
    array_lenghts_exprs: SparseSecondaryMap<InTypeId, &'d Expression>,

    /// Composite names
    composite_names: SparseSecondaryMap<InTypeId, Option<Identifier>>,
    /// Composite fields
    composite_fields: SparseSecondaryMap<InTypeId, BTreeMap<Identifier, (TypeSizeExpr, OutTypeId)>>,
    /// Size of composite types
    composite_size: SparseSecondaryMap<InTypeId, TypeSizeExpr>,

    /// Function inputs
    fn_inputs: SparseSecondaryMap<InTypeId, Vec<OutTypeId>>,
    /// Function inputs
    fn_output: SparseSecondaryMap<InTypeId, Option<OutTypeId>>,

    /// Pointer kind
    pointer_kind: SparseSecondaryMap<InTypeId, PointerKind>,
    /// Pointer destinations
    pointer_pointee: SparseSecondaryMap<InTypeId, OutTypeId>,
}

impl<'d> RegisteringPhaseMaps<'d> {
    /// Bind a name to a definition
    fn bind(&mut self, name: Identifier, idx: OutTypeId) {
        self.bindings.entry(name).or_default().insert(idx);
    }

    /// Register a type definition
    fn register(&mut self, def: Either<&'d TypeDef, &'d TypeDefData>) -> OutTypeId {
        let reference = match def {
            Left(TypeDef::Data(def)) | Right(def) => {
                if let TypeDefData::Named(name) = def {
                    // Simple definition, insert it and return the type id
                    Right(*name)
                } else {
                    let idx = self.type_defs.insert(Right(def));
                    self.register_datatype_subtypes(def, idx);
                    Left(idx)
                }
            }
            Left(def @ TypeDef::Unknow(TypeDefUnknow { .. })) => {
                Left(self.type_defs.insert(Left(def)))
            }
            Left(def @ TypeDef::Fn(TypeDefFn { inputs, output, .. })) => {
                let idx = self.type_defs.insert(Left(def));

                let inputs_idxs = inputs
                    .iter()
                    .map(|typ| self.register(Right(typ)))
                    .collect_vec();
                let output_idx = output.as_ref().map(|(_, typ)| self.register(Right(typ)));

                self.fn_inputs.insert(idx, inputs_idxs);
                self.fn_output.insert(idx, output_idx);

                Left(idx)
            }
        };
        self.idxs_mapping.insert(reference)
    }

    /// register the subtypes of a datatype
    fn register_datatype_subtypes(&mut self, def: &'d TypeDefData, idx: InTypeId) {
        match def {
            TypeDefData::Int(_) => (),

            TypeDefData::Array(TypeDefArray {
                box element,
                lenght,
                ..
            }) => {
                // register the element
                let element = self.register(Right(element));
                self.array_elements.insert(idx, element);
                // register the unexpressed lenght
                self.array_lenghts_exprs.insert(idx, lenght);
            }
            TypeDefData::Struct(TypeDefStruct { name, fields, .. }) => {
                // registering the field types
                let mut composite_fields = BTreeMap::new();
                let mut offsets = vec![];
                for (field_name, _, field_type) in fields.iter() {
                    let field_idx = self.register(Right(field_type));

                    if let Left(name) = field_name {
                        let discarded = composite_fields
                            .insert(*name, (TypeSizeExpr::Sum(offsets.clone()), field_idx));
                        debug_assert!(discarded.is_none(), "Duplicate field {name:?}")
                    }

                    offsets.push(field_idx);
                }

                self.composite_names.insert(idx, *name);
                self.composite_fields.insert(idx, composite_fields);
                self.composite_size.insert(idx, TypeSizeExpr::Sum(offsets));
            }
            TypeDefData::Union(TypeDefUnion { name, variants, .. }) => {
                // registering the field types
                let mut composite_fields = BTreeMap::new();
                let mut variants_idxs = vec![];
                for (field_name, _, field_type) in variants.iter() {
                    let variant_idx = self.register(Right(field_type));

                    if let Left(name) = field_name {
                        let discarded = composite_fields
                            .insert(*name, (TypeSizeExpr::Sum(vec![]), variant_idx));
                        debug_assert!(discarded.is_none(), "Duplicate variant {name:?}")
                    }

                    variants_idxs.push(variant_idx);
                }

                self.composite_names.insert(idx, *name);
                self.composite_fields.insert(idx, composite_fields);
                self.composite_size
                    .insert(idx, TypeSizeExpr::Max(variants_idxs));
            }
            TypeDefData::Pointer(TypeDefPointer { kind, box pointee }) => {
                let pointee_idx = self.register(Left(pointee));

                self.pointer_kind.insert(idx, (*kind).into());
                self.pointer_pointee.insert(idx, pointee_idx);
            }

            TypeDefData::Named(_) => unreachable!(),
        }
    }
}

struct AstRegisteringVisitor<'m, 'd> {
    maps: Option<&'m mut RegisteringPhaseMaps<'d>>,
    parent_as_type_def_data: Option<&'d TypeDefData>,
}

impl<'m, 'd> AstRegisteringVisitor<'m, 'd> {
    fn new(maps: &'m mut RegisteringPhaseMaps<'d>) -> Self {
        Self {
            maps: Some(maps),
            parent_as_type_def_data: None,
        }
    }
}

impl<'d> AstVisitor<'d> for AstRegisteringVisitor<'_, 'd> {
    type ChildVisitor = Self;

    type Result = ();

    fn enter(&mut self, node: &'d impl ast::ast_node::AstNode) -> Self::ChildVisitor {
        let maps = self.maps.take().unwrap();

        /*
           This code could appear as a bunch of branching.
           Remember that all `as_*` and `is_*` function are effectively constants, and will be erased at compile time.
           At runtime, the specialized version of this function should be empty for anything that could not contain the
           searched nodes, and in case contains at maximum a single branch for the name of composites
        */

        // is this a `type A = B;` binding?
        if let Some(ItemType { ident, ty, .. }) = node.as_item_type() {
            // register the binding
            let idx = maps.register(Left(ty));
            maps.bind(*ident, idx);
        }
        // is it a composite with a name?
        if let Some(TypeDefStruct {
            name: Some(ident), ..
        }) = node.as_type_def_struct()
        {
            let idx = maps.register(Right(
                self.parent_as_type_def_data
                    .expect("Struct are child only of TypeDefData"),
            ));
            maps.bind(*ident, idx);
        }
        if let Some(TypeDefUnion {
            name: Some(ident), ..
        }) = node.as_type_def_union()
        {
            let idx = maps.register(Right(
                self.parent_as_type_def_data
                    .expect("Struct are child only of TypeDefData"),
            ));
            maps.bind(*ident, idx);
        }

        /*
           Those should be all possible points where a type alias is being defined.
           If in future there are other nodes in the AST defining a type alias
           they MUST be added here
        */

        // the mutable maps is passed down
        Self {
            maps: Some(maps),
            parent_as_type_def_data: node.as_type_def_data(),
        }
    }

    fn exit(
        &mut self,
        _: &'d impl ast::ast_node::AstNode,
        Self { maps, .. }: Self::ChildVisitor,
    ) -> Self::Result {
        // recovering the maps handler
        self.maps = Some(maps.unwrap());
        ()
    }
}

#[derive(Debug)]
/// Data needed for the size calculation
struct SizeCalculationMaps<'m, 'd, Solver> {
    /// Mapping from type referred to types or names
    merged_idxs: &'m SecondaryMap<OutTypeId, InTypeId>,
    /// Type definitions
    type_defs: &'m SlotMap<InTypeId, Either<&'d TypeDef, &'d TypeDefData>>,

    /// Multiple bindings of the same name, needs to end up in the same concrete id
    bindings: &'m BTreeMap<Identifier, BTreeSet<OutTypeId>>,

    /// Array elements
    array_elements: &'m SparseSecondaryMap<InTypeId, OutTypeId>,
    /// Array lengts
    array_lenghts_exprs: &'m SparseSecondaryMap<InTypeId, &'d Expression>,

    /// Size of composite types
    composite_size: &'m SparseSecondaryMap<InTypeId, TypeSizeExpr>,

    /// Solver for the length expressions
    expr_solver: &'m Solver,

    /// Recursion guards
    recurring_on: SecondaryMap<InTypeId, bool>,

    /// Sizes of the types
    type_sizes: SecondaryMap<InTypeId, Option<VMUInt>>,

    /// Lenght of the arrays
    array_lenghts: SparseSecondaryMap<InTypeId, VMUInt>,
}

impl<'d, Solver> SizeCalculationMaps<'_, 'd, Solver>
where
    Solver: SizeExpressionSolver<SizeError>,
{
    fn calculate_sizes(&mut self) -> Result<(), SizeError> {
        for (in_idx, def) in self.type_defs {
            match def {
                Left(_) => {
                    self.type_sizes.insert(in_idx, None);
                }
                Right(def) => {
                    self.calculate_size_in(in_idx, def)?;
                }
            }
        }
        Ok(())
    }

    fn calculate_size_in(
        &mut self,
        in_idx: InTypeId,
        def: &'d TypeDefData,
    ) -> Result<VMUInt, SizeError> {
        if let Some(size) = self.type_sizes.get(in_idx) {
            return Ok(size.unwrap());
        }

        if let Some(true) = self.recurring_on.insert(in_idx, true) {
            // Recursive type detected!
            return Err(SizeError::RecursiveTypeSizeError);
        }

        let size = match def {
            TypeDefData::Int(_) | TypeDefData::Pointer(_) => 1,

            TypeDefData::Array(_) => {
                let lenght = self.calculate_lenght(in_idx)?;
                let element_size = self.calculate_size_out(self.array_elements[in_idx])?;

                VMUInt::checked_mul(lenght, element_size).ok_or(
                    SizeError::OverflowingArraySize {
                        lenght,
                        element_size,
                    },
                )?
            }

            TypeDefData::Struct(_) | TypeDefData::Union(_) => match &self.composite_size[in_idx] {
                TypeSizeExpr::Max(types) => types
                    .iter()
                    .map(|out_idx| self.calculate_size_out(*out_idx))
                    .try_fold(0, |a, b| b.map(|b| VMUInt::max(a, b))),
                TypeSizeExpr::Sum(types) => types
                    .iter()
                    .map(|out_idx| self.calculate_size_out(*out_idx))
                    .try_fold(0, |a, b| {
                        b.and_then(|b| VMUInt::checked_add(a, b).ok_or(SizeError::StructTooLarge))
                    }),
            }?,

            TypeDefData::Named(_) => unreachable!(),
        };

        self.type_sizes.insert(in_idx, Some(size));

        self.recurring_on.insert(in_idx, false);

        Ok(size)
    }

    fn calculate_size_out(&mut self, out_idx: OutTypeId) -> Result<VMUInt, SizeError> {
        let in_idx = self.merged_idxs[out_idx];
        let def = self.type_defs[in_idx];
        match def {
            Right(def) => self.calculate_size_in(in_idx, def),
            Left(def) => Err(SizeError::UnsizedComponents(def.clone())),
        }
    }

    /// Calculate lenght of the array
    fn calculate_lenght(&mut self, in_idx: InTypeId) -> Result<VMUInt, SizeError> {
        if let Some(len) = self.array_lenghts.get(in_idx) {
            return Ok(*len);
        }
        let len = VMUInt::try_from(
            self.expr_solver
                .solve(self.array_lenghts_exprs[in_idx], &mut |def| {
                    self.calculate_size_def(def)
                })
                .map_err(|err| SizeError::UnsolvableArrayLenght(Box::new(err)))?,
        )
        .map_err(SizeError::NegativeArrayLenght)?;
        self.array_lenghts.insert(in_idx, len);
        Ok(len)
    }

    fn calculate_size_def(&mut self, def: &'d TypeDefData) -> Result<VMUInt, SizeError> {
        match def {
            TypeDefData::Int(_) | TypeDefData::Pointer(_) => Ok(1),

            // named type continue the recursion, so they are calculated only once
            TypeDefData::Struct(TypeDefStruct {
                name: Some(name), ..
            })
            | TypeDefData::Union(TypeDefUnion {
                name: Some(name), ..
            })
            | TypeDefData::Named(name) => {
                let in_idx = self.merged_idxs[*self.bindings[name].first().unwrap()];
                self.calculate_size_in(in_idx, def)
            }

            TypeDefData::Array(TypeDefArray {
                box element,
                lenght,
                ..
            }) => {
                let element_size = self.calculate_size_def(element)?;
                let lenght = VMUInt::try_from(
                    self.expr_solver
                        .solve(lenght, &mut |def| self.calculate_size_def(def))
                        .map_err(|err| SizeError::UnsolvableArrayLenght(Box::new(err)))?,
                )
                .map_err(SizeError::NegativeArrayLenght)?;

                VMUInt::checked_mul(lenght, element_size).ok_or(SizeError::OverflowingArraySize {
                    lenght,
                    element_size,
                })
            }

            TypeDefData::Struct(TypeDefStruct {
                name: None, fields, ..
            }) => fields
                .iter()
                .map(|(_, _, def)| self.calculate_size_def(def))
                .try_fold(0, |a, b| {
                    b.and_then(|b| VMUInt::checked_add(a, b).ok_or(SizeError::StructTooLarge))
                }),

            TypeDefData::Union(TypeDefUnion {
                name: None,
                variants,
                ..
            }) => variants
                .iter()
                .map(|(_, _, def)| self.calculate_size_def(def))
                .try_fold(0, |a, b| b.map(|b| VMUInt::max(a, b))),
        }
    }
}

// Deduplicating
struct DeduplicationMaps<'m, 'd> {
    /// Mapping from type referred to types or names
    merged_idxs: &'m mut SecondaryMap<OutTypeId, InTypeId>,

    /// Type definitions
    type_defs: &'m SlotMap<InTypeId, Either<&'d TypeDef, &'d TypeDefData>>,

    /// Array elements
    array_elements: &'m SparseSecondaryMap<InTypeId, OutTypeId>,
    /// Array lengts
    array_lenghts: &'m SparseSecondaryMap<InTypeId, VMUInt>,

    /// Composite names
    composite_names: &'m SparseSecondaryMap<InTypeId, Option<Identifier>>,
    /// Composite fields
    composite_fields: &'m SparseSecondaryMap<InTypeId, BTreeMap<Identifier, (VMUInt, OutTypeId)>>,

    /// Function inputs
    fn_inputs: &'m SparseSecondaryMap<InTypeId, Vec<OutTypeId>>,
    /// Function inputs
    fn_output: &'m SparseSecondaryMap<InTypeId, Option<OutTypeId>>,

    /// Pointer kind
    pointer_kind: &'m SparseSecondaryMap<InTypeId, PointerKind>,
    /// Pointer destinations
    pointer_pointee: &'m SparseSecondaryMap<InTypeId, OutTypeId>,

    /// Sizes of the types
    type_sizes: &'m SecondaryMap<InTypeId, Option<VMUInt>>,
}

impl<'d> DeduplicationMaps<'_, 'd> {
    fn deduplicate_all(&mut self) -> BTreeSet<InTypeId> {
        let mut deduplicated: BTreeSet<InTypeId> = BTreeSet::new();

        'dedup: while let Some(new) = self
            .merged_idxs
            .values()
            .find(|v| deduplicated.contains(v))
            .copied()
        {
            for old in &deduplicated {
                if self.try_merge::<true>(new, *old) {
                    // it was not new
                    continue 'dedup;
                }
            }
            // a new unique!
            deduplicated.insert(new);
        }

        deduplicated
    }

    /// Try to merge two ids, and return wheter the merge is successfull
    /// If do_rollback == false, the caller is responsible for rolling back the changes
    fn try_merge<const DO_ROLLBACK: bool>(&mut self, a: InTypeId, b: InTypeId) -> bool {
        // preliminary checks
        if self.type_sizes[a] != self.type_sizes[b]
            || !match (self.type_defs[a], self.type_defs[b]) {
                // functions must at least match the number of arguments
                (Left(TypeDef::Fn(_)), Left(TypeDef::Fn(_))) => {
                    self.fn_inputs[a].len() == self.fn_inputs[b].len()
                }
                // unknows matches
                (Left(TypeDef::Unknow(_)), Left(TypeDef::Unknow(_))) => true,
                // data is stored as Right
                (Left(TypeDef::Data(_)), _) | (_, Left(TypeDef::Data(_))) => unreachable!(),
                // Fn/unknow does not match
                (Left(_), Left(_)) => false,
                // Data and not data does not match
                (Left(_), Right(_)) | (Right(_), Left(_)) => false,
                // Data/Data matching
                (Right(def_a), Right(def_b)) => match (def_a, def_b) {
                    (TypeDefData::Int(_), TypeDefData::Int(_)) => true,
                    (TypeDefData::Array(_), TypeDefData::Array(_)) => {
                        self.array_lenghts[a] == self.array_lenghts[b]
                    }

                    // Composite must match in name and  name and position of the fields
                    (
                        TypeDefData::Struct(_) | TypeDefData::Union(_),
                        TypeDefData::Struct(_) | TypeDefData::Union(_),
                    ) => {
                        self.composite_names[a] == self.composite_names[b] && {
                            let a_fields = &self.composite_fields[a];
                            let b_fields = &self.composite_fields[b];
                            a_fields.iter().all(|(name, (a_offset, _))| {
                                b_fields
                                    .get(name)
                                    .is_some_and(|(b_offset, _)| a_offset == b_offset)
                            })
                        }
                    }

                    // pointers must match in kind
                    (TypeDefData::Pointer(_), TypeDefData::Pointer(_)) => {
                        self.pointer_kind[a] == self.pointer_kind[b]
                    }

                    (TypeDefData::Named(_), _) | (_, TypeDefData::Named(_)) => {
                        unreachable!()
                    }
                    // any other case does not match
                    (_, _) => false,
                },
            }
        {
            return false;
        }

        /*
        Now, we know the two types MIGHT be the same. We start by merging them,
        then check if the subtypes can be merged (assuming therefore the original ones are the same),
        finally we eventually rollback the merge
        */

        // merge b into a
        let old_idxs = DO_ROLLBACK.then(|| self.merged_idxs.clone());
        for in_id in self.merged_idxs.values_mut() {
            if *in_id == b {
                *in_id = a
            }
        }

        // check merging of subtypes
        // IMPORTANT: DO_ROLLBACK == FALSE is only sound if we rollback at any false
        let subtype_merged = match self.type_defs[a] {
            Left(TypeDef::Fn(_)) => {
                // First, match the inputs
                self.fn_inputs[a]
                    .iter()
                    .zip(&self.fn_inputs[b])
                    .all(|(a, b)| {
                        self.try_merge::<false>(self.merged_idxs[*a], self.merged_idxs[*b])
                    })
                    && {
                        // then the output
                        match (self.fn_output[a], self.fn_output[b]) {
                            (None, None) => true,
                            (None, Some(out)) | (Some(out), None) => {
                                // It must be a fieldless, nameless, 0-size composite
                                let out = self.merged_idxs[out];
                                matches!(
                                    self.type_defs[out],
                                    Right(TypeDefData::Struct(_) | TypeDefData::Union(_))
                                ) && self.composite_names[out].is_none()
                                    && self.composite_fields[out].is_empty()
                                    && self.type_sizes[out] == Some(0)
                            }
                            (Some(a), Some(b)) => {
                                self.try_merge::<false>(self.merged_idxs[a], self.merged_idxs[b])
                            }
                        }
                    }
            }
            Left(TypeDef::Unknow(_)) => true,
            Left(TypeDef::Data(_)) => unreachable!(),
            Right(def) => match def {
                TypeDefData::Int(_) => true,
                TypeDefData::Struct(_) | TypeDefData::Union(_) => {
                    self.composite_fields[a].iter().all(|(name, (_, field_a))| {
                        let field_b = self.composite_fields[b][name].1;
                        self.try_merge::<false>(
                            self.merged_idxs[*field_a],
                            self.merged_idxs[field_b],
                        )
                    })
                }
                TypeDefData::Array(_) => self.try_merge::<false>(
                    self.merged_idxs[self.array_elements[a]],
                    self.merged_idxs[self.array_elements[b]],
                ),
                TypeDefData::Pointer(_) => self.try_merge::<false>(
                    self.merged_idxs[self.pointer_pointee[a]],
                    self.merged_idxs[self.pointer_pointee[b]],
                ),
                TypeDefData::Named(_) => unreachable!(),
            },
        };

        if subtype_merged {
            // Success!
            // We cannot purge the type data cause a rollback can still happen
            true
        } else {
            // Nope. Rolling back if needed
            if DO_ROLLBACK {
                *self.merged_idxs = old_idxs.unwrap();
            }
            false
        }
    }
}

pub(crate) fn generate<Solver: SizeExpressionSolver<SizeError>>(
    ast: &File,
    solver: Solver,
) -> Result<TypeTable<Solver>, TypeDeclareError> {
    // Inserting all types recursively
    let mut maps = RegisteringPhaseMaps::default();

    ast.visited_by(&mut AstRegisteringVisitor::new(&mut maps));

    let RegisteringPhaseMaps {
        idxs_mapping,
        type_defs,
        bindings,
        array_elements,
        array_lenghts_exprs,
        composite_names,
        composite_fields,
        composite_size,
        fn_inputs,
        fn_output,
        pointer_kind,
        pointer_pointee,
    } = maps;

    let mut merged_idxs = SecondaryMap::new();
    // Solving names to uniques type id
    for (out_id, in_id) in &idxs_mapping {
        if merged_idxs.contains_key(out_id) {
            // this type is already solved
            continue;
        }
        let in_id = match *in_id {
            Left(in_id) => in_id,
            Right(name) => 'search: {
                let mut aliases = vec![name];
                let mut expanded = BTreeSet::new();

                let mut out_idxs = BTreeSet::from([out_id]);

                while let Some(alias) = aliases.pop() {
                    for out_id in &bindings[&alias] {
                        if out_idxs.insert(*out_id) {
                            match idxs_mapping[*out_id] {
                                Left(in_id) => {
                                    for out_id in out_idxs {
                                        merged_idxs.insert(out_id, in_id);
                                    }
                                    break 'search in_id;
                                }
                                Right(next_alias) => {
                                    if expanded.insert(next_alias) {
                                        aliases.push(next_alias)
                                    }
                                }
                            }
                        }
                    }
                }

                // A forced loop!
                return Err(TypeDeclareError::TypesWithNoDefinition(expanded));
            }
        };
        merged_idxs.insert(out_id, in_id);
    }

    let mut maps = SizeCalculationMaps {
        merged_idxs: &merged_idxs,
        type_defs: &type_defs,
        bindings: &bindings,
        array_elements: &array_elements,
        array_lenghts_exprs: &array_lenghts_exprs,
        composite_size: &composite_size,
        expr_solver: &solver,
        recurring_on: SecondaryMap::new(),
        type_sizes: SecondaryMap::new(),
        array_lenghts: SparseSecondaryMap::new(),
    };
    maps.calculate_sizes()?;
    let SizeCalculationMaps {
        type_sizes,
        array_lenghts,
        ..
    } = maps;

    // Calculating all the offsets
    let composite_fields: SparseSecondaryMap<InTypeId, BTreeMap<Identifier, (VMUInt, OutTypeId)>> =
        composite_fields
            .into_iter()
            .map(|(idx, fields)| {
                (
                    idx,
                    fields
                        .into_iter()
                        .map(|(name, (offset, typ))| {
                            (
                                name,
                                (
                                    match offset {
                                        TypeSizeExpr::Max(types) => types
                                            .into_iter()
                                            .map(|out_id| type_sizes[merged_idxs[out_id]].unwrap())
                                            .max()
                                            .unwrap_or(0),
                                        TypeSizeExpr::Sum(types) => types
                                            .into_iter()
                                            .map(|out_id| type_sizes[merged_idxs[out_id]].unwrap())
                                            .sum(),
                                    },
                                    typ,
                                ),
                            )
                        })
                        .collect(),
                )
            })
            .collect();

    let unique_ids = DeduplicationMaps {
        merged_idxs: &mut merged_idxs,
        type_defs: &type_defs,
        array_elements: &array_elements,
        array_lenghts: &array_lenghts,
        composite_names: &composite_names,
        composite_fields: &composite_fields,
        fn_inputs: &fn_inputs,
        fn_output: &fn_output,
        pointer_kind: &pointer_kind,
        pointer_pointee: &pointer_pointee,
        type_sizes: &type_sizes,
    }
    .deduplicate_all();

    // check aliases
    let bindings: BTreeMap<Identifier, InTypeId> = bindings
        .into_iter()
        .map(|(name, targets)| {
            let targets = targets
                .into_iter()
                .map(|t| merged_idxs[t])
                .collect::<BTreeSet<_>>();
            if targets.len() > 1 {
                // Different InTypeId!!
                Err(TypeDeclareError::DifferentDefinitions {
                    name,
                    defs: targets
                        .into_iter()
                        .map(|t| match type_defs[t] {
                            Left(t) => t.clone(),
                            Right(t) => TypeDef::Data(t.clone()),
                        })
                        .collect_vec(),
                })
            } else {
                Ok((name, targets.into_iter().next().unwrap()))
            }
        })
        .try_collect()?;

    // Building the actual tables
    let (type_ids, num_ids) = {
        let mut ids = SecondaryMap::new();

        // finding the reserved ids
        if let Some(int_id) = unique_ids
            .iter()
            .copied()
            .find(|id| matches!(&type_defs[*id], Right(TypeDefData::Int(_))))
        {
            ids[int_id] = INT_ID.into_id()
        }
        if let Some(unknow_id) = unique_ids
            .iter()
            .copied()
            .find(|id| matches!(&type_defs[*id], Left(TypeDef::Unknow(_))))
        {
            ids[unknow_id] = UNKNOW_ID
        }
        if let Some(unit_id) = unique_ids.iter().copied().find(|id| {
            matches!(
                &type_defs[*id],
                Right(TypeDefData::Struct(_) | TypeDefData::Union(_))
            ) && composite_names[*id] == None
                && type_sizes[*id] == Some(0)
                && composite_fields[*id].is_empty()
        }) {
            ids[unit_id] = UNIT_ID.into_id()
        }

        let mut counter = 3; // first three ids are reserved for int, _, struct {}
        for in_id in unique_ids.iter() {
            if !ids.contains_key(*in_id) {
                ids.insert(*in_id, TypeId(counter));
                counter += 1;
            }
        }
        (ids, counter)
    };

    let out_id_to_type_id = |out_id| type_ids[merged_idxs[out_id]];
    let out_id_to_type_data_id = |out_id| TypeIdData(type_ids[merged_idxs[out_id]]);

    let mut types = vec![None; num_ids];
    types[0] = Some(TypeEntry {
        typ: Type::Data(TypeData::Int(TypeInt)),
        name: None,
        size: Some(1),
    });
    types[1] = Some(TypeEntry {
        typ: Type::Unknow(TypeUnknow),
        name: None,
        size: None,
    });
    types[2] = Some(TypeEntry {
        typ: Type::Data(TypeData::Composite(TypeComposite {
            name: None,
            fields: BTreeMap::new(),
            size: 0,
        })),
        name: None,
        size: Some(0),
    });
    for id in unique_ids {
        let mut name = None;
        let typ = match type_defs[id] {
            Left(TypeDef::Fn(_)) => Type::Fn(TypeFn {
                inputs: fn_inputs[id]
                    .iter()
                    .copied()
                    .map(out_id_to_type_data_id)
                    .collect_vec(),
                output: fn_output[id].map(out_id_to_type_data_id).unwrap_or(UNIT_ID),
            }),
            Left(TypeDef::Unknow(_)) => {
                debug_assert_eq!(type_ids[id], UNKNOW_ID);
                continue;
            }
            Left(TypeDef::Data(_)) => unreachable!(),
            Right(def) => Type::Data(match def {
                TypeDefData::Int(_) => {
                    debug_assert_eq!(type_ids[id], INT_ID.into_id());
                    continue;
                }

                TypeDefData::Array(_) => TypeData::Array(TypeArray {
                    element: out_id_to_type_data_id(array_elements[id]),
                    lenght: array_lenghts[id],
                }),
                TypeDefData::Struct(_) | TypeDefData::Union(_) => {
                    name = composite_names[id];
                    let fields = &composite_fields[id];
                    let size = type_sizes[id].unwrap();
                    if name.is_none() && fields.is_empty() && size == 0 {
                        debug_assert_eq!(type_ids[id], UNIT_ID.into_id());
                        continue;
                    }
                    TypeData::Composite(TypeComposite {
                        name,
                        fields: fields
                            .into_iter()
                            .map(|(name, (offset, id))| {
                                (*name, (*offset, out_id_to_type_data_id(*id)))
                            })
                            .collect(),
                        size,
                    })
                }
                TypeDefData::Pointer(_) => TypeData::Pointer(TypePointer {
                    kind: pointer_kind[id],
                    pointee: out_id_to_type_id(pointer_pointee[id]),
                }),

                TypeDefData::Named(_) => unreachable!(),
            }),
        };
        let size = type_sizes[id];
        types[type_ids[id].0] = Some(TypeEntry { typ, name, size });
    }

    let types = types
        .into_iter()
        .map(|entry| Box::new(entry.unwrap()))
        .collect();
    let names = bindings
        .into_iter()
        .map(|(name, id)| (name, type_ids[id]))
        .collect();

    Ok(TypeTable {
        types,
        names,
        solver,
    })
}
