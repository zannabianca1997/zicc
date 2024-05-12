//! Calculate the size of all types, even recursive ones

use std::{
    cell::{RefCell, RefMut},
    collections::{BTreeMap, BTreeSet},
    convert::identity,
    mem,
};

use ast::{
    ast_node::{AstNode, AstVisitor},
    expression::{Expression, ExpressionSizeOf, SizeExpressionSolver},
    tokens::Identifier,
    typedef::{TypeDef, TypeDefData, TypeDefPointer},
};
use either::Either::{self, Left, Right};
use elsa::FrozenVec;
use thiserror::Error;
use vm::VMUInt;

use crate::PointerKind;

use super::{
    name_resolution::{self, ReferredType},
    BaseType, FieldOffsets, MappingFunctions, TypeEntry,
};

impl FieldOffsets<name_resolution::ReferredType> {
    fn solve_basic_types(self) -> FieldOffsets<usize> {
        self.const_propagation(&mut |r| match r {
            ReferredType::Base(base) => Some(base.size().unwrap()),
            ReferredType::Id(_) => None,
        })
        .map_references(&mut |r| match r {
            ReferredType::Base(_) => unreachable!(),
            ReferredType::Id(id) => id,
        })
    }
}

type CalculatingEntry<'d> = TypeEntry<
    'd,
    ReferredType,
    RefCell<FieldOffsets<usize>>,
    RefCell<FieldOffsets<usize>>,
    RefCell<Either<&'d Expression, VMUInt>>,
    RefCell<Option<VMUInt>>,
>;
impl CalculatingEntry<'_> {
    // Check if the size has been calculated
    fn size(&self) -> Option<Option<VMUInt>> {
        Some(match self {
            TypeEntry::Array { size, .. } => Some((*size.try_borrow().ok()?)?),
            TypeEntry::Composite { size, .. } => Some(*size.try_borrow().ok()?.as_value()?),
            TypeEntry::Pointer { .. } => Some(1),
            TypeEntry::Fn { .. } => None,
        })
    }
}
pub type Entry<'d> = TypeEntry<'d, ReferredType, VMUInt, VMUInt, VMUInt, VMUInt>;

/// Goal of a calculation
enum Target<'t, 'd> {
    /// Reduce the offset in the RefCell into a value
    Offset(&'t RefCell<FieldOffsets<usize>>),
    /// Expand the offset, knowing the types are already solved
    OffsetWithSolvedTypes(RefMut<'t, FieldOffsets<usize>>),
    /// Transform the expression in the RefCell into a value
    Expression(&'t RefCell<Either<&'d Expression, VMUInt>>),
    /// Expand the expression, knowing all types inside have been solved
    ExpressionWithSolvedTypes(RefMut<'t, Either<&'d Expression, VMUInt>>),
    /// Calculate the size of an array
    ArraySize {
        element: ArrayElementStages,
        lenght: ArrayLenghtStages<'t, 'd>,
        size: MaybeCalculating<'t, Option<VMUInt>>,
    },
    /// Calculate all the size params in the given type (id)
    Entry(usize),
}

#[derive(Debug, Clone, Copy)]
enum ArrayElementStages {
    Unexamined(ReferredType),
    Calculated(usize),
    Done(VMUInt),
}
#[derive(Debug)]
enum ArrayLenghtStages<'t, 'd> {
    Unexamined(&'t RefCell<Either<&'d Expression, VMUInt>>),
    Calculated(&'t RefCell<Either<&'d Expression, VMUInt>>),
}

enum MaybeCalculating<'t, T> {
    Unexamined(&'t RefCell<T>),
    Calculating(RefMut<'t, T>),
}

#[derive(Debug, Error)]
pub enum SizeError<ConstExprError> {
    #[error("Error in evaluating const expression")]
    ConstExprError(#[from] ConstExprError),
    #[error("A lenght const expression evaluated to negative")]
    NegativeLenght(Expression),
}

pub(crate) fn calculate<'d, Solver: SizeExpressionSolver<!>>(
    types: Vec<name_resolution::Entry<'d>>,
    solver: Solver,
    names: &BTreeMap<Identifier, name_resolution::ReferredType>,
) -> Result<Vec<Entry<'d>>, SizeError<Solver::Error>> {
    // Wrapping all the targets in RefCell, permitting multiple references into the array
    // Also moving to a FrozenVec, so it is possible to add types while keeping a reference to the type themselves
    // Finally we solve all the reference to basic types
    let types: FrozenVec<Box<CalculatingEntry>> = types
        .into_iter()
        .map(|entry| {
            Box::new(entry.map(MappingFunctions {
                f_rt: identity,
                f_fo: |fo: FieldOffsets<ReferredType>| RefCell::new(fo.solve_basic_types()),
                f_cs: |cs: FieldOffsets<ReferredType>| RefCell::new(cs.solve_basic_types()),
                f_al: |al| RefCell::new(Left(al)),
                f_as: |()| RefCell::new(None),
            }))
        })
        .collect();
    {
        let mut stack_bottom = (0..types.len()).map(|id| Target::Entry(id));
        let mut stack = vec![];

        'goals: while let Some(target) = stack.pop().or_else(|| stack_bottom.next()) {
            match target {
                Target::Offset(offset) => {
                    let Ok(offset) = offset.try_borrow_mut() else {
                        todo!("Recursion detected!")
                    };
                    let mut types_needed = BTreeSet::new();
                    offset.for_all_references(&mut |r| {
                        types_needed.insert(*r);
                    });
                    // add the goal of solving the offset
                    stack.push(Target::OffsetWithSolvedTypes(offset));
                    // add all types eventually used in size_of to the goals
                    for idx in types_needed {
                        stack.push(Target::Entry(idx))
                    }
                }
                Target::OffsetWithSolvedTypes(mut offset) => {
                    let offset_expr = mem::replace(&mut *offset, FieldOffsets::Value(0));
                    let offset_expr = offset_expr
                        .const_propagation(&|id| Some(types[*id].size().unwrap().unwrap()));
                    debug_assert!(matches!(offset_expr, FieldOffsets::Value(_)));

                    *offset = offset_expr;
                }

                // -- EXPRESSIONS CALCULATIONS
                Target::Expression(expression) => {
                    let Ok(expression) = expression.try_borrow_mut() else {
                        todo!("Recursion detected!")
                    };
                    let types_needed: BTreeSet<_> = match expression.as_ref() {
                        Left(expr) => collect_sizeofs(expr)
                            .into_iter()
                            .filter_map(|ty| {
                                add_type_def_data_to_table(&types, ty, names)
                                    .try_into_id()
                                    .ok()
                            })
                            .collect(),
                        Either::Right(_) => {
                            // already solved!
                            continue 'goals;
                        }
                    };
                    // add the goal of solving the expression
                    stack.push(Target::ExpressionWithSolvedTypes(expression));
                    // add all types eventually used in size_of to the goals
                    for idx in types_needed {
                        stack.push(Target::Entry(idx))
                    }
                }
                Target::ExpressionWithSolvedTypes(mut expression) => {
                    let expr = mem::replace(&mut *expression, Right(0)).unwrap_left();
                    let value = solver.solve(expr, &mut |ty| -> Result<VMUInt, !> {
                        let idx = add_type_def_data_to_table(&types, ty, names);
                        Ok(match idx {
                            ReferredType::Base(base) => base.size().unwrap(),
                            ReferredType::Id(idx) => types[idx]
                                .size()
                                .expect("All size_of types should have been resolved eagerly")
                                .unwrap(),
                        })
                    })?;
                    match VMUInt::try_from(value) {
                        Ok(val) => *expression = Right(val),
                        Err(_) => return Err(SizeError::NegativeLenght(expr.clone())),
                    }
                }

                // -- ARRAY SIZE CALCULATIONS
                Target::ArraySize {
                    element: ArrayElementStages::Unexamined(element),
                    lenght: ArrayLenghtStages::Unexamined(lenght),
                    size: MaybeCalculating::Unexamined(size),
                } => {
                    let Ok(size) = size.try_borrow_mut() else {
                        todo!("Recursion detected!")
                    };
                    let element_size = match element {
                        ReferredType::Base(base) => base.size().unwrap(),
                        ReferredType::Id(id) => match types[id].size() {
                            Some(size) => size.unwrap(),
                            None => {
                                stack.push(Target::ArraySize {
                                    element: ArrayElementStages::Calculated(id),
                                    lenght: ArrayLenghtStages::Unexamined(lenght),
                                    size: MaybeCalculating::Calculating(size),
                                });
                                stack.push(Target::Entry(id));
                                continue 'goals;
                            }
                        },
                    };
                    // already solved. skipping recursion.
                    stack.push(Target::ArraySize {
                        element: ArrayElementStages::Done(element_size),
                        lenght: ArrayLenghtStages::Unexamined(lenght),
                        size: MaybeCalculating::Calculating(size),
                    })
                }
                Target::ArraySize {
                    element: ArrayElementStages::Calculated(id),
                    lenght: ArrayLenghtStages::Unexamined(lenght),
                    size: MaybeCalculating::Calculating(size),
                } => stack.push(Target::ArraySize {
                    element: ArrayElementStages::Done(
                        types[id]
                            .size()
                            .expect("The size should have been calculated")
                            .unwrap(),
                    ),
                    lenght: ArrayLenghtStages::Unexamined(lenght),
                    size: MaybeCalculating::Calculating(size),
                }),
                Target::ArraySize {
                    element: ArrayElementStages::Done(element_size),
                    lenght: ArrayLenghtStages::Unexamined(length),
                    size: MaybeCalculating::Calculating(size),
                } => {
                    stack.push(Target::ArraySize {
                        element: ArrayElementStages::Done(element_size),
                        lenght: ArrayLenghtStages::Calculated(length),
                        size: MaybeCalculating::Calculating(size),
                    });
                    stack.push(Target::Expression(length));
                }
                Target::ArraySize {
                    element: ArrayElementStages::Done(element_size),
                    lenght: ArrayLenghtStages::Calculated(lenght),
                    size: MaybeCalculating::Calculating(mut size),
                } => {
                    // we know it's now done
                    let lenght = lenght.borrow().right().unwrap();
                    // ending the method
                    *size = Some(element_size * lenght);
                }
                Target::ArraySize { .. } => {
                    unreachable!("All the precedent branches should flow one into another")
                }

                // -- ENTRIES CALCULATIONS
                Target::Entry(id) => {
                    // check the entry is still unsolved
                    if types[id].size().is_some() {
                        continue 'goals;
                    }

                    match &types[id] {
                        TypeEntry::Array {
                            element,
                            lenght,
                            size,
                            ..
                        } => stack.push(Target::ArraySize {
                            element: ArrayElementStages::Unexamined(*element),
                            lenght: ArrayLenghtStages::Unexamined(lenght),
                            size: MaybeCalculating::Unexamined(size),
                        }),
                        // for composite, we need to know all the offsets
                        TypeEntry::Composite { fields, size, .. } => stack.extend(
                            fields
                                .values()
                                .map(|(o, _)| o)
                                .chain(Some(size))
                                .map(Target::Offset),
                        ),
                        // pointer and functions have(n't) trivial size
                        TypeEntry::Pointer { .. } | TypeEntry::Fn { .. } => (),
                    }
                }
            }
        }

        // dropping stack, and assuring with it all references to `types`
    }

    let mut types = types;

    // now we can unwrap everything
    Ok(types
        .as_mut()
        .drain(..)
        .map(|box entry| {
            entry.map(MappingFunctions {
                f_rt: identity,
                f_fo: |o: RefCell<FieldOffsets<usize>>| o.into_inner().try_into_value().unwrap(),
                f_cs: |s: RefCell<FieldOffsets<usize>>| s.into_inner().try_into_value().unwrap(),
                f_al: |l: RefCell<Either<&Expression, VMUInt>>| l.into_inner().unwrap_right(),
                f_as: |s: RefCell<Option<VMUInt>>| s.into_inner().unwrap(),
            })
        })
        .collect())
}

fn add_type_def_data_to_table<'d>(
    types: &FrozenVec<Box<CalculatingEntry<'d>>>,
    ty: &'d TypeDefData,
    names: &BTreeMap<Identifier, name_resolution::ReferredType>,
) -> ReferredType {
    // given all named types where found, here we can recurse fearlessly
    match ty {
        TypeDefData::Int(_) => ReferredType::Base(BaseType::Int),
        TypeDefData::Array(_) => todo!(),
        TypeDefData::Struct(_) => todo!(),
        TypeDefData::Union(_) => todo!(),
        TypeDefData::Pointer(def @ TypeDefPointer { kind, box pointee }) => {
            let pointee = add_type_def_to_table(types, pointee, names);
            ReferredType::Id(
                match  types
                    .iter()
                    .enumerate()
                    .find_map(|(id, entry)| {
                        (matches!(entry, CalculatingEntry::Pointer { kind: entry_kind, pointee: entry_pointee, .. } if PointerKind::from(*kind) == *entry_kind && pointee == *entry_pointee))
                            .then_some(id)
                    }) {
                        Some(id) => id,
                        None => {types.push(Box::new(CalculatingEntry::Pointer { def, kind: (*kind).into(), pointee })); types.len() -1},
                    },
            )
        }
        TypeDefData::Named(name) => names[name],
    }
}
fn add_type_def_to_table<'d>(
    types: &FrozenVec<Box<CalculatingEntry<'d>>>,
    ty: &'d TypeDef,
    names: &BTreeMap<Identifier, name_resolution::ReferredType>,
) -> ReferredType {
    // given all named types where found, here we can recurse fearlessly
    let entry = match ty {
        TypeDef::Data(ty) => return add_type_def_data_to_table(types, ty, names),
        TypeDef::Fn(_) => todo!(),
        TypeDef::Unknow(_) => todo!(),
    };
}

struct SizeOfCollector<'m, 'd>(Option<&'m mut Vec<&'d ExpressionSizeOf>>);

impl<'d> AstVisitor<'d> for SizeOfCollector<'_, 'd> {
    type ChildVisitor = Self;

    type Result = ();

    fn enter(&mut self, node: &'d impl ast::ast_node::AstNode) -> Self::ChildVisitor {
        let inner = self.0.take().unwrap();
        if let Some(node) = node.as_expression_size_of() {
            inner.push(node)
        }
        Self(Some(inner))
    }

    fn exit(
        &mut self,
        _: &'d impl ast::ast_node::AstNode,
        mut child_visitor: Self::ChildVisitor,
    ) -> Self::Result {
        self.0 = Some(child_visitor.0.take().unwrap())
    }
}

fn collect_sizeofs(expr: &Expression) -> impl IntoIterator<Item = &TypeDefData> {
    let mut sizeofs = vec![];
    expr.visited_by(&mut SizeOfCollector(Some(&mut sizeofs)));
    sizeofs.into_iter().map(|ExpressionSizeOf { ty, .. }| ty)
}
