use std::{
    cell::{OnceCell, RefCell},
    collections::{btree_map::Entry, BTreeMap, BTreeSet},
    hash::Hash,
    mem,
    ops::{Deref, DerefMut},
    sync::atomic::AtomicBool,
};

use either::Either::{self, Left, Right};
use generational_arena::{Arena, Index};
use itertools::Itertools;
use thiserror::Error;
use vm::{VMUInt, VM};

use crate::ast::{
    expression::Expression,
    tokens::Identifier,
    typedef::{
        PointerKindDef, TypeDef, TypeDefArray, TypeDefData, TypeDefFn, TypeDefInt, TypeDefPointer,
        TypeDefStruct, TypeDefUnion, TypeDefUnknow,
    },
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeId(usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeIdData(TypeId);

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Type {
    Data(TypeData),
    Fn(TypeFn),
    Unknow(TypeUnknow),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TypeData {
    Int(TypeInt),
    Array(TypeArray),
    Composite(TypeComposite),
    Pointer(TypePointer),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeInt;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeArray {
    pub element: TypeIdData,
    pub lenght: vm::VMUInt,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeComposite {
    pub name: Option<Identifier>,
    pub fields: BTreeMap<Identifier, (vm::VMUInt, TypeIdData)>,
    pub size: vm::VMUInt,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypePointer {
    pub kind: PointerKind,
    pub pointee: TypeId,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum PointerKind {
    Stack,
    Static,
}

impl From<PointerKindDef> for PointerKind {
    fn from(value: PointerKindDef) -> Self {
        match value {
            PointerKindDef::Stack(_) => Self::Stack,
            PointerKindDef::Static(_) => Self::Static,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeFn {
    pub inputs: Vec<TypeIdData>,
    pub output: TypeIdData,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeUnknow;

#[derive(Debug, Clone)]
struct TypeEntry {
    /// The type
    typ: Type,
    /// If a datatype, the size of the type
    size: Option<usize>,
    /// Optional specific name of the type, if declared
    name: Option<Identifier>,
}

#[derive(Debug, Clone)]
pub struct TypeTable {
    types: Vec<TypeEntry>,
    names: BTreeMap<Identifier, TypeId>,
}

#[derive(Debug, Clone, Error)]
pub enum TypeDeclareError<E> {
    #[error("Type was already declared")]
    Redeclared {
        original: Identifier,
        redeclared: Identifier,
    },
    #[error(transparent)]
    TypeSolveError(#[from] TypeSolveError),
    #[error("Cannot resolve array lenght")]
    UnsolvableArrayLenght(#[source] E),
}

#[derive(Debug, Clone, Error)]
pub enum TypeSolveError {
    #[error("Cannot solve a named type")]
    UnknowName(Identifier),
}

impl TypeTable {
    /// Create a table with all the declared names
    pub fn new<'d, E>(
        declared: impl Iterator<Item = (Identifier, &'d TypeDef)>,
        used: impl Iterator<Item = Either<&'d TypeDef, &'d TypeDefData>>,
        expr_solver: impl Fn(&Expression, fn(&TypeDefData) -> vm::VMUInt) -> Result<vm::VMInt, E>,
    ) -> Result<Self, TypeDeclareError<E>> {
        type IdentifierOrIndex = Either<Identifier, Index>;

        /// Type intermediate representation
        enum TypeIR<'d> {
            Unknow,
            Fn {
                def: &'d TypeDefFn,
                inputs: Vec<IdentifierOrIndex>,
                output: IdentifierOrIndex,
            },

            // Data
            Int,
            Unit,
            Array {
                def: &'d TypeDefArray,
                element: IdentifierOrIndex,
                lenght: OnceCell<VMUInt>,
                size: OnceCell<VMUInt>,
            },
            Pointer {
                def: &'d TypeDefPointer,
                kind: PointerKind,
                pointee: IdentifierOrIndex,
            },
            Composite {
                def: Either<&'d TypeDefStruct, &'d TypeDefUnion>,
                name: Option<Identifier>,
                fields: BTreeMap<
                    Identifier,
                    (Either<VMUInt, Vec<IdentifierOrIndex>>, IdentifierOrIndex),
                >,
                size: Either<VMUInt, CompositeSize>,
            },
        }

        enum CompositeSize {
            Sum(Vec<IdentifierOrIndex>),
            Max(Vec<IdentifierOrIndex>),
        }

        struct TableIR<'d> {
            /// Types we registered
            types: Arena<TypeIR<'d>>,

            /// name resolution
            names: Either<
                BTreeMap<Identifier, BTreeSet<IdentifierOrIndex>>,
                (
                    BTreeMap<Identifier, Index>,
                    BTreeMap<Identifier, BTreeSet<Index>>,
                ),
            >,

            /// index of the int type
            int_idx: Index,
            /// index of the unknow type
            unknow_idx: Index,
            /// index of the unit type
            unit_idx: Index,
        }

        impl<'d> TableIR<'d> {
            fn new() -> Self {
                let mut types = Arena::new();

                let int_idx = types.insert(TypeIR::Int);
                let unknow_idx = types.insert(TypeIR::Unknow);
                let unit_idx = types.insert(TypeIR::Unit);

                Self {
                    types,
                    names: Left(BTreeMap::new()),
                    int_idx,
                    unknow_idx,
                    unit_idx,
                }
            }

            /// Declare a type, bounding it to a name
            fn declare(&mut self, name: Identifier, def: Either<&'d TypeDef, &'d TypeDefData>) {
                let idx = self.register(def);
                self.bind(name, idx);
            }

            /// Register a type and its subtypes
            fn register(&mut self, def: Either<&'d TypeDef, &'d TypeDefData>) -> IdentifierOrIndex {
                let mut bound_to = None;

                let ir = match def {
                    Right(def) | Left(TypeDef::Data(def)) => match def {
                        TypeDefData::Array(def @ TypeDefArray { element, .. }) => TypeIR::Array {
                            def,
                            element: self.register(Right(element)),
                            lenght: OnceCell::new(),
                            size: OnceCell::new(),
                        },
                        TypeDefData::Struct(
                            def @ TypeDefStruct {
                                name,
                                fields: struct_fields,
                                ..
                            },
                        ) => {
                            let mut fields = BTreeMap::new();
                            let mut offset = vec![];

                            for (name, _, typ) in struct_fields.iter() {
                                let typ = self.register(Right(typ));
                                if let Left(name) = name {
                                    fields.insert(*name, (Right(offset.clone()), typ));
                                }
                                offset.push(typ)
                            }

                            bound_to = *name;

                            TypeIR::Composite {
                                def: Left(def),
                                name: *name,
                                fields,
                                size: Right(CompositeSize::Sum(offset)),
                            }
                        }
                        TypeDefData::Union(def @ TypeDefUnion { name, variants, .. }) => {
                            let mut fields = BTreeMap::new();
                            let mut offset = vec![];

                            for (name, _, typ) in variants.iter() {
                                let typ = self.register(Right(typ));
                                if let Left(name) = name {
                                    fields.insert(*name, (Left(0), typ));
                                }
                                offset.push(typ)
                            }

                            bound_to = *name;

                            TypeIR::Composite {
                                def: Right(def),
                                name: *name,
                                fields,
                                size: Right(CompositeSize::Max(offset)),
                            }
                        }
                        TypeDefData::Pointer(def @ TypeDefPointer { kind, box pointee }) => {
                            TypeIR::Pointer {
                                def,
                                kind: (*kind).into(),
                                pointee: self.register(Left(pointee)),
                            }
                        }

                        // special returns
                        TypeDefData::Int(_) => return Right(self.int_idx),
                        TypeDefData::Named(name) => return Left(*name),
                    },
                    Left(TypeDef::Unknow(def)) => return Right(self.unknow_idx),
                    Left(TypeDef::Fn(
                        def @ TypeDefFn {
                            inputs: inputs_def,
                            output,
                            ..
                        },
                    )) => {
                        let mut inputs = vec![];
                        for inp in inputs_def.iter() {
                            inputs.push(self.register(Right(inp)))
                        }
                        let output = if let Some((_, output)) = output {
                            self.register(Right(output))
                        } else {
                            Right(self.unit_idx)
                        };
                        TypeIR::Fn {
                            def,
                            inputs,
                            output,
                        }
                    }
                };

                let idx = self.types.insert(ir);

                if let Some(bound_to) = bound_to {
                    self.bind(bound_to, Right(idx))
                }

                Right(idx)
            }

            /// Bind a identifier to a index
            fn bind(&mut self, name: Identifier, idx: IdentifierOrIndex) {
                self.bindings_mut()
                    .expect("No bind should happen after alias resolving")
                    .entry(name)
                    .or_default()
                    .insert(idx);
            }

            fn bindings(&self) -> Option<&BTreeMap<Identifier, BTreeSet<IdentifierOrIndex>>> {
                self.names.as_ref().left()
            }
            fn bindings_mut(
                &mut self,
            ) -> Option<&mut BTreeMap<Identifier, BTreeSet<IdentifierOrIndex>>> {
                self.names.as_mut().left()
            }

            fn names(&self) -> Option<&BTreeMap<Identifier, Index>> {
                self.names.as_ref().right().map(|(n, a)| n)
            }
            fn names_mut(&mut self) -> Option<&mut BTreeMap<Identifier, Index>> {
                self.names.as_mut().right().map(|(n, a)| n)
            }

            fn aliases(&self) -> Option<&BTreeMap<Identifier, BTreeSet<Index>>> {
                self.names.as_ref().right().map(|(n, a)| a)
            }
            fn aliases_mut(&mut self) -> Option<&mut BTreeMap<Identifier, BTreeSet<Index>>> {
                self.names.as_mut().right().map(|(n, a)| a)
            }

            /// Fully resolve aliases
            fn solve_aliases(&mut self) -> Result<(), !> {
                let bindings = mem::take(
                    self.bindings_mut()
                        .expect("`solve_aliases` should be called only once"),
                );

                let mut bindings_no_loops: BTreeMap<Identifier, BTreeSet<Index>> = BTreeMap::new();
                for (name, binds_to) in &bindings {
                    bindings_no_loops
                        .entry(*name)
                        .or_default()
                        .extend(binds_to.iter().filter_map(|v| v.right()));

                    let mut do_not_extend = BTreeSet::from([*name]);
                    let mut aliases = binds_to
                        .iter()
                        .filter_map(|v| {
                            v.left()
                                .and_then(|n| (!do_not_extend.contains(&n)).then_some(n))
                        })
                        .collect_vec();

                    while let Some(extend) = aliases.pop() {
                        let Some(binds_to) = bindings.get(&extend) else {
                            panic!("Undefined name {extend:?}")
                        };

                        do_not_extend.insert(extend);

                        bindings_no_loops
                            .entry(*name)
                            .or_default()
                            .extend(binds_to.iter().filter_map(|v| v.right()));
                        aliases.extend(binds_to.iter().filter_map(|v| {
                            v.left()
                                .and_then(|n| (!do_not_extend.contains(&n)).then_some(n))
                        }))
                    }
                }

                self.names = Right((
                    bindings_no_loops
                        .iter()
                        .map(|(n, a)| (*n, *a.first().unwrap()))
                        .collect(),
                    bindings_no_loops,
                ));

                Ok(())
            }

            /// Check that every name has at least one concrete definition
            fn check_names(&self) -> Result<(), Identifier> {
                todo!()
            }
        }

        // create intermediate table
        let mut table = TableIR::new();
        // declare all types
        for (name, def) in declared {
            table.declare(name, Left(def))
        }
        // register all types used
        for def in used {
            table.register(def);
        }

        todo!()
    }
}

fn sorted_couple<T: Ord>(t1: T, t2: T) -> [T; 2] {
    if t1 < t2 {
        [t1, t2]
    } else {
        [t2, t1]
    }
}
