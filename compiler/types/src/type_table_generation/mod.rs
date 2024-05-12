//! Generation of the recursive part of the type table

use std::collections::{BTreeMap, BTreeSet};

use ast::{
    expression::SizeExpressionSolver,
    tokens::Identifier,
    typedef::{
        TypeDefArray, TypeDefFn, TypeDefInt, TypeDefPointer, TypeDefStruct, TypeDefUnion,
        TypeDefUnknow,
    },
    File,
};
use either::Either;
use thiserror::Error;
use vm::VMUInt;

use crate::{type_table_generation::deduplication::deduplicate, PointerKind, TypeTable};

/// Data collected about a given definition

#[derive(Debug, Clone)]
enum TypeEntry<
    // Lifetime of the AST definition
    'd,
    // Reference to another type
    ReferredType,
    // Composite types memory offsets
    FieldOffsets,
    // Composite sizes
    CompositeSize,
    // Array lenght expression
    ArrayLenght,
    // Measured size of the array
    ArraySize,
> {
    Array {
        def: &'d TypeDefArray,
        element: ReferredType,
        lenght: ArrayLenght,
        size: ArraySize,
    },
    Composite {
        def: Either<&'d TypeDefStruct, &'d TypeDefUnion>,
        name: Option<Identifier>,
        fields: BTreeMap<Identifier, (FieldOffsets, ReferredType)>,
        size: CompositeSize,
    },
    Pointer {
        def: &'d TypeDefPointer,
        kind: PointerKind,
        pointee: ReferredType,
    },

    Fn {
        def: &'d TypeDefFn,
        inputs: Vec<ReferredType>,
        output: ReferredType,
    },
}

pub struct MappingFunctions<F_RT, F_FO, F_CS, F_AL, F_AS> {
    f_rt: F_RT,
    f_fo: F_FO,
    f_cs: F_CS,
    f_al: F_AL,
    f_as: F_AS,
}

impl<'d, RT, FO, CS, AL, AS> TypeEntry<'d, RT, FO, CS, AL, AS> {
    fn map<NewRT, NewFO, NewCS, NewAL, NewAS>(
        self,
        MappingFunctions {
            mut f_rt,
            mut f_fo,
            f_cs,
            f_al,
            f_as,
        }: MappingFunctions<
            impl FnMut(RT) -> NewRT,
            impl FnMut(FO) -> NewFO,
            impl FnOnce(CS) -> NewCS,
            impl FnOnce(AL) -> NewAL,
            impl FnOnce(AS) -> NewAS,
        >,
    ) -> TypeEntry<'d, NewRT, NewFO, NewCS, NewAL, NewAS> {
        match self {
            TypeEntry::Array {
                def,
                element,
                lenght,
                size,
            } => TypeEntry::Array {
                def,
                element: f_rt(element),
                lenght: f_al(lenght),
                size: f_as(size),
            },
            TypeEntry::Composite {
                def,
                name,
                fields,
                size,
            } => TypeEntry::Composite {
                def,
                name,
                fields: fields
                    .into_iter()
                    .map(|(n, (o, ty))| (n, (f_fo(o), f_rt(ty))))
                    .collect(),
                size: f_cs(size),
            },
            TypeEntry::Pointer { def, kind, pointee } => TypeEntry::Pointer {
                def,
                kind,
                pointee: f_rt(pointee),
            },
            TypeEntry::Fn {
                def,
                inputs,
                output,
            } => TypeEntry::Fn {
                def,
                inputs: inputs.into_iter().map(|inp| f_rt(inp)).collect(),
                output: f_rt(output),
            },
        }
    }
}

/// An expression for field offsets
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum FieldOffsets<ReferredType> {
    /// Constant value
    Value(VMUInt),
    /// Size of a type
    SizeOf(ReferredType),
    /// Max of a group of expressions, or 0 if the group is empty
    MaxOr0(BTreeSet<FieldOffsets<ReferredType>>),
    /// Sum of a group of expressions, with coefficients
    Poly(BTreeMap<FieldOffsets<ReferredType>, VMUInt>),
}

impl<ReferredType> FieldOffsets<ReferredType>
where
    ReferredType: Ord,
{
    pub fn max_of_sizes(types: impl IntoIterator<Item = ReferredType>) -> Self {
        Self::MaxOr0(types.into_iter().map(Self::SizeOf).collect())
    }

    pub fn sum_of_sizes(types: impl IntoIterator<Item = ReferredType>) -> Self {
        let mut poly = BTreeMap::new();
        for ty in types {
            *poly.entry(FieldOffsets::SizeOf(ty)).or_insert(0 as VMUInt) += 1;
        }
        Self::Poly(poly)
    }

    fn const_propagation(self, solver: &impl Fn(&ReferredType) -> Option<VMUInt>) -> Self {
        match self {
            // not so much to do here
            FieldOffsets::Value(v) => FieldOffsets::Value(v),

            // possible solve
            FieldOffsets::SizeOf(ty) => {
                if let Some(val) = solver(&ty) {
                    FieldOffsets::Value(val)
                } else {
                    FieldOffsets::SizeOf(ty)
                }
            }

            // recurse and special cases
            FieldOffsets::MaxOr0(childs) => {
                let mut childs: BTreeSet<_> = childs
                    .into_iter()
                    .map(|c| c.const_propagation(solver))
                    .collect();
                // conglomerate all `Values` in a single one
                if let Some(max_value) = childs
                    .extract_if(|child| matches!(child, FieldOffsets::Value(_)))
                    .map(|c| {
                        let FieldOffsets::Value(c) = c else {
                            unreachable!()
                        };
                        c
                    })
                    .reduce(VMUInt::max)
                {
                    childs.insert(FieldOffsets::Value(max_value));
                }
                // Drop the indirection if no child or single child
                match childs.len() {
                    0 => FieldOffsets::Value(0),
                    1 => childs.pop_first().unwrap(),
                    _ => FieldOffsets::MaxOr0(childs),
                }
            }
            FieldOffsets::Poly(childs) => {
                let mut childs: BTreeMap<_, _> = {
                    let mut new = BTreeMap::new();
                    for (child, coeff) in childs.into_iter() {
                        let child = child.const_propagation(solver);
                        // Expand all the poly in the childrens
                        if let FieldOffsets::Poly(sub_poly) = child {
                            for (child, sub_coeff) in sub_poly {
                                *new.entry(child).or_insert(0) += coeff * sub_coeff;
                            }
                        } else {
                            *new.entry(child).or_insert(0) += coeff
                        }
                    }
                    new
                };
                // Coagulate all `Values` summing them
                if let Some(const_value) = childs
                    .extract_if(|child, _| matches!(child, FieldOffsets::Value(_)))
                    .map(|(child, coeff)| {
                        let FieldOffsets::Value(child) = child else {
                            unreachable!()
                        };
                        child * coeff
                    })
                    .reduce(|a, b| a + b)
                {
                    childs.insert(FieldOffsets::Value(const_value), 1);
                }
                // Drop the indirection if no child or single child with coefficient 1
                match childs.len() {
                    // Empty poly
                    0 => FieldOffsets::Value(0),
                    // Single term, coefficients == 1
                    1 => match childs.pop_first().unwrap() {
                        (child, 1) => child,
                        (FieldOffsets::Value(v), c) => FieldOffsets::Value(v * c),
                        (child, coeff) => {
                            childs.insert(child, coeff);
                            FieldOffsets::Poly(childs)
                        }
                    },
                    // Still complex
                    _ => FieldOffsets::Poly(childs),
                }
            }
        }
    }
}
impl<RT> FieldOffsets<RT> {
    fn as_value(&self) -> Option<&VMUInt> {
        if let Self::Value(v) = self {
            Some(v)
        } else {
            None
        }
    }
    fn try_into_value(self) -> Result<VMUInt, Self> {
        if let Self::Value(v) = self {
            Ok(v)
        } else {
            Err(self)
        }
    }
    fn map_references<NewRT>(self, fun: &mut impl FnMut(RT) -> NewRT) -> FieldOffsets<NewRT>
    where
        NewRT: Ord,
    {
        match self {
            FieldOffsets::Value(v) => FieldOffsets::Value(v),
            FieldOffsets::SizeOf(ty) => FieldOffsets::SizeOf(fun(ty)),
            FieldOffsets::MaxOr0(set) => FieldOffsets::MaxOr0(
                set.into_iter()
                    .map(|child| child.map_references(fun))
                    .collect(),
            ),
            FieldOffsets::Poly(poly) => {
                let mut new_poly = BTreeMap::new();
                for (term, coeff) in poly {
                    *new_poly.entry(term.map_references(fun)).or_insert(0) += coeff;
                }
                FieldOffsets::Poly(new_poly)
            }
        }
    }
    fn for_all_references(&self, fun: &mut impl FnMut(&RT)) {
        match self {
            FieldOffsets::Value(v) => (),
            FieldOffsets::SizeOf(ty) => fun(ty),
            FieldOffsets::MaxOr0(set) => {
                for child in set {
                    child.for_all_references(fun)
                }
            }
            FieldOffsets::Poly(poly) => {
                for child in poly.keys() {
                    child.for_all_references(fun)
                }
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
/// Scalar type without childs
enum BaseType {
    Int,
    Unknow,
    Unit,
}
impl BaseType {
    fn size(&self) -> Option<VMUInt> {
        match self {
            BaseType::Int => Some(1),
            BaseType::Unknow => None,
            BaseType::Unit => Some(0),
        }
    }
}

mod discover;

mod name_resolution;

mod size_calculation;
pub use size_calculation::SizeError;

mod deduplication;

#[derive(Debug, Error)]
pub enum TypeDeclareError<SolverError> {
    #[error(transparent)]
    DuplicateDefinition(#[from] discover::DuplicateDefinitions),
    #[error(transparent)]
    AliasLoop(#[from] name_resolution::NameResolveError),
    #[error(transparent)]
    SizeError(#[from] size_calculation::SizeError<SolverError>),
}

pub(crate) fn generate<Solver: SizeExpressionSolver<!>>(
    ast: &File,
    solver: Solver,
) -> Result<TypeTable<Solver>, TypeDeclareError<Solver::Error>> {
    // type discovery
    let (types, names) = discover::discover(ast)?;

    // name resolution
    let (types, names) = name_resolution::resolve(types, names)?;

    // size calculations
    let types = size_calculation::calculate(types, &solver, &names)?;

    // deduplication and final id assignation
    let (types, names) = deduplicate(types, names);

    Ok(TypeTable {
        types,
        names,
        solver,
    })
}
