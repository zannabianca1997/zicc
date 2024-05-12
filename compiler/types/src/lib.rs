#![feature(box_patterns)]
#![feature(iter_intersperse)]
#![feature(btree_extract_if)]
#![feature(if_let_guard)]
#![feature(never_type)]

use std::{
    collections::BTreeMap,
    fmt::{Debug, Display, Write as _},
};

use display_context::{display_with_any_context, DisplayWithContext};
use elsa::FrozenVec;

use ast::{
    expression::{const_expr::ConstExpressionSolver, SizeExpressionSolver},
    tokens::{Identifier, KeywordInt, PunctAmpersand, PunctAt, PunctUnderscore},
    typedef::PointerKindDef,
    File,
};
use indenter::indented;
use itertools::Itertools;
use vm::VMUInt;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeId(usize);

impl<S, IC: ?Sized> DisplayWithContext<TypeDisplayContext<'_, S, IC>> for TypeId
where
    Identifier: DisplayWithContext<IC>,
{
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        context: &TypeDisplayContext<'_, S, IC>,
    ) -> std::fmt::Result {
        match context.pushed() {
            Some(pushed_c) => {
                let ty = pushed_c.table.type_(*self);
                if !context.expand_composites
                    && ty.as_data().is_some_and(|data| data.is_composite())
                {
                    // do not show complex types
                    write!(f, "<{}>", self.0)
                } else {
                    DisplayWithContext::fmt(ty, f, &pushed_c)
                }
            }
            None => write!(f, "<{}>", self.0),
        }
    }
}

pub const UNKNOW_ID: TypeId = TypeId(1);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeIdData(TypeId);

impl TypeIdData {
    pub fn id(&self) -> &TypeId {
        &self.0
    }
    pub fn into_id(self) -> TypeId {
        self.0
    }
}

impl<S, IC: ?Sized> DisplayWithContext<TypeDisplayContext<'_, S, IC>> for TypeIdData
where
    Identifier: DisplayWithContext<IC>,
{
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        context: &TypeDisplayContext<'_, S, IC>,
    ) -> std::fmt::Result {
        DisplayWithContext::fmt(&self.into_id(), f, &context)
    }
}

impl From<TypeIdData> for TypeId {
    fn from(value: TypeIdData) -> Self {
        value.0
    }
}

impl std::ops::Deref for TypeIdData {
    type Target = TypeId;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

pub const INT_ID: TypeIdData = TypeIdData(TypeId(0));
pub const UNIT_ID: TypeIdData = TypeIdData(TypeId(2));

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Type {
    Data(TypeData),
    Fn(TypeFn),
    Unknow(TypeUnknow),
}

impl<S, IC: ?Sized> DisplayWithContext<TypeDisplayContext<'_, S, IC>> for Type
where
    Identifier: DisplayWithContext<IC>,
{
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        context: &TypeDisplayContext<'_, S, IC>,
    ) -> std::fmt::Result {
        match self {
            Type::Data(ty) => DisplayWithContext::fmt(ty, f, context),
            Type::Fn(ty) => DisplayWithContext::fmt(ty, f, context),
            Type::Unknow(ty) => DisplayWithContext::fmt(ty, f, context),
        }
    }
}

impl Type {
    /// Returns `true` if the type is [`Data`].
    ///
    /// [`Data`]: Type::Data
    #[must_use]
    pub fn is_data(&self) -> bool {
        matches!(self, Self::Data(..))
    }

    pub fn as_data(&self) -> Option<&TypeData> {
        if let Self::Data(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub fn try_into_data(self) -> Result<TypeData, Self> {
        if let Self::Data(v) = self {
            Ok(v)
        } else {
            Err(self)
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TypeData {
    Int(TypeInt),
    Array(TypeArray),
    Composite(TypeComposite),
    Pointer(TypePointer),
}

impl TypeData {
    /// Returns `true` if the type data is [`Composite`].
    ///
    /// [`Composite`]: TypeData::Composite
    #[must_use]
    pub fn is_composite(&self) -> bool {
        matches!(self, Self::Composite(..))
    }
}

impl<S, IC: ?Sized> DisplayWithContext<TypeDisplayContext<'_, S, IC>> for TypeData
where
    Identifier: DisplayWithContext<IC>,
{
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        context: &TypeDisplayContext<'_, S, IC>,
    ) -> std::fmt::Result {
        match self {
            TypeData::Int(ty) => DisplayWithContext::fmt(ty, f, context),
            TypeData::Array(ty) => DisplayWithContext::fmt(ty, f, context),
            TypeData::Composite(ty) => DisplayWithContext::fmt(ty, f, context),
            TypeData::Pointer(ty) => DisplayWithContext::fmt(ty, f, context),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeInt;

impl Display for TypeInt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&KeywordInt::new(), f)
    }
}
display_with_any_context! {TypeInt}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeArray {
    pub element: TypeIdData,
    pub lenght: vm::VMUInt,
}
impl<S, IC: ?Sized> DisplayWithContext<TypeDisplayContext<'_, S, IC>> for TypeArray
where
    Identifier: DisplayWithContext<IC>,
{
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        context: &TypeDisplayContext<'_, S, IC>,
    ) -> std::fmt::Result {
        write!(
            f,
            "[{}; {}]",
            self.element.with_context(&context),
            self.lenght
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeComposite {
    pub name: Option<Identifier>,
    pub fields: BTreeMap<Identifier, (vm::VMUInt, TypeIdData)>,
    pub size: vm::VMUInt,
}
impl<S, IC: ?Sized> DisplayWithContext<TypeDisplayContext<'_, S, IC>> for TypeComposite
where
    Identifier: DisplayWithContext<IC>,
{
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        context: &TypeDisplayContext<'_, S, IC>,
    ) -> std::fmt::Result {
        // is a plain union?
        if self.fields.is_empty() {
            write!(f, "struct {{}}")
        } else if self.fields.values().all(|(offset, _)| *offset == 0) {
            write!(f, "union")?;
            if let Some(name) = self.name {
                write!(f, " {}", name.with_context(context.ident_context))?;
            }
            {
                let mut f = indented(f);
                writeln!(f, "{{")?;
                for (name, (_, ty)) in &self.fields {
                    writeln!(
                        f,
                        "{}: {},",
                        name.with_context(context.ident_context),
                        ty.with_context(context)
                    )?;
                }
                // padding if size is off
                if self
                    .fields
                    .values()
                    .map(|(_, ty)| context.table.size_of(*ty))
                    .max()
                    .unwrap()
                    < self.size
                {
                    writeln!(f, "_: [int; {}],", self.size)?;
                }
            }
            write!(f, "}}")
        } else {
            let mut fields = self
                .fields
                .iter()
                .map(|(name, (start, ty))| {
                    (*name, *start..*start + context.table.size_of(*ty), *ty)
                })
                .collect_vec();
            // are all fields disjoint?
            if fields
                .iter()
                .enumerate()
                .flat_map(|(pos, (_, r1, _))| fields[..pos].iter().map(move |(_, r2, _)| (r1, r2)))
                .all(|(r1, r2)| r1.start >= r2.end || r2.start >= r1.end)
            {
                // sort the fields by order of apparition
                fields.sort_by(|(_, r1, _), (_, r2, _)| r1.start.cmp(&r2.start));

                write!(f, "struct")?;
                if let Some(name) = self.name {
                    write!(f, " {}", name.with_context(context.ident_context))?;
                }
                {
                    writeln!(f, " {{")?;
                    let mut f = indented(f);
                    let mut pos = 0;
                    for (name, range, ty) in fields {
                        if pos < range.start {
                            // padding
                            writeln!(f, "_: [int; {}],", range.start - pos)?;
                        }
                        writeln!(
                            f,
                            "{}: {},",
                            name.with_context(context.ident_context),
                            ty.with_context(context)
                        )?;
                        pos = range.end;
                    }
                    // ending padding
                    if pos < self.size {
                        writeln!(f, "_: [int; {}],", self.size - pos)?;
                    }
                }
                write!(f, "}}")
            } else {
                // complex composite with overlapping, non-start field. Not really recostruible, but possible only with unnamed structs and union
                unimplemented!("Complex composite printing")
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypePointer {
    pub kind: PointerKind,
    pub pointee: TypeId,
}
impl<S, IC: ?Sized> DisplayWithContext<TypeDisplayContext<'_, S, IC>> for TypePointer
where
    Identifier: DisplayWithContext<IC>,
{
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        context: &TypeDisplayContext<'_, S, IC>,
    ) -> std::fmt::Result {
        DisplayWithContext::fmt(&self.kind, f, context)?;
        DisplayWithContext::fmt(&self.pointee, f, context)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum PointerKind {
    Stack,
    Static,
}
impl Display for PointerKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PointerKind::Stack => Display::fmt(&PunctAt::new(), f),
            PointerKind::Static => Display::fmt(&PunctAmpersand::new(), f),
        }
    }
}
display_with_any_context! {PointerKind}

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
impl<S, IC: ?Sized> DisplayWithContext<TypeDisplayContext<'_, S, IC>> for TypeFn
where
    Identifier: DisplayWithContext<IC>,
{
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        context: &TypeDisplayContext<'_, S, IC>,
    ) -> std::fmt::Result {
        write!(f, "fn(")?;
        for item in Iterator::intersperse(self.inputs.iter().map(Some), None) {
            match item {
                Some(inp) => DisplayWithContext::fmt(inp, f, context),
                None => write!(f, ", "),
            }?
        }
        write!(f, ") -> ")?;
        DisplayWithContext::fmt(&self.output, f, context)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeUnknow;
impl Display for TypeUnknow {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&PunctUnderscore::new(), f)
    }
}
display_with_any_context! {TypeUnknow}

#[derive(Debug, Clone)]
struct TypeEntry {
    /// The type
    typ: Type,
    /// Optional specific name of the type, if declared
    name: Option<Identifier>,
    /// Size of the type
    size: Option<VMUInt>,
}

#[derive(Clone)]
pub struct TypeTable<Solver> {
    types: FrozenVec<Box<TypeEntry>>,
    names: BTreeMap<Identifier, TypeId>,
    pub solver: Solver,
}

impl<S> TypeTable<S> {
    /// Get the id of a type
    pub fn type_id(&self, searching: Type) -> TypeId {
        // first, let's check if we have it
        for (id, TypeEntry { typ, .. }) in self.types.iter().enumerate() {
            if typ == &searching {
                return TypeId(id);
            }
        }
        // Nope. Need to add it.
        // This should be rare, only when the compiler generate new type
        self.types.push(Box::new(TypeEntry {
            size: self.calculate_size(&searching),
            typ: searching,
            name: None,
        }));
        TypeId(self.types.len() - 1)
    }

    /// Get a type from a id
    pub fn type_(&self, searching: TypeId) -> &Type {
        &self.types[searching.0].typ
    }

    /// Get the id of a datatype
    pub fn type_id_data(&self, searching: TypeData) -> TypeIdData {
        TypeIdData(self.type_id(Type::Data(searching)))
    }

    /// Get a type from a id
    pub fn type_data(&self, searching: TypeIdData) -> &TypeData {
        self.type_(searching.into_id())
            .as_data()
            .expect("The table should create TypeIdData only for actual datatypes")
    }

    /// Change the expression solver
    ///
    /// This is sould only if the new expression solver resolve MORE expressions than the older one, giving results where the old one would have failed
    /// Is a logic error to give a new solver that return different values for the same expression
    pub fn with_solver<NS>(self, solver: NS) -> TypeTable<NS> {
        let Self {
            types,
            names,
            solver: _,
        } = self;
        TypeTable {
            types,
            names,
            solver,
        }
    }

    /// Return the size of a datatype
    pub fn size_of(&self, id: TypeIdData) -> VMUInt {
        self.types[id.0 .0]
            .size
            .expect("All datatypes should have a size")
    }

    pub fn is_data(&self, id: TypeId) -> bool {
        matches!(self.types[id.0].typ, Type::Data(_))
    }

    pub fn as_data(&self, id: TypeId) -> Option<TypeIdData> {
        self.is_data(id).then_some(TypeIdData(id))
    }

    /// Helper to calculate the size of a type
    fn calculate_size(&self, typ: &Type) -> Option<VMUInt> {
        match typ {
            Type::Data(typ) => Some(match typ {
                TypeData::Int(_) | TypeData::Pointer(_) => 1,
                TypeData::Array(TypeArray { element, lenght }) => self.size_of(*element) * lenght,
                TypeData::Composite(TypeComposite { size, .. }) => *size,
            }),
            Type::Fn(_) | Type::Unknow(_) => None,
        }
    }

    pub fn report_all_types<'s, C: ?Sized>(&'s self, context: &'s C) -> impl Display + 's
    where
        Identifier: DisplayWithContext<C>,
    {
        ReportAllTypes(self, context)
    }
}

impl TypeTable<ConstExpressionSolver> {
    /// Build a typetable from a AST
    pub fn build(ast: &File) -> Result<Self, TypeDeclareError> {
        type_table_generation::generate(ast, ConstExpressionSolver)
    }
}

mod type_table_generation;
pub use type_table_generation::SizeError;
pub type TypeDeclareError = type_table_generation::TypeDeclareError<
    <ConstExpressionSolver as SizeExpressionSolver<!>>::Error,
>;

#[derive(Clone, Copy)]
pub struct ReportAllTypes<'s, S, C: ?Sized>(&'s TypeTable<S>, &'s C);
impl<'s, S, C: ?Sized> Display for ReportAllTypes<'s, S, C>
where
    Identifier: DisplayWithContext<C>,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let ReportAllTypes(table, context) = *self;

        for (n, TypeEntry { typ, name, size }) in table.types.iter().enumerate() {
            // id
            write!(f, "{n}")?;

            // human name
            if let Some(name) = name {
                write!(f, "\t{}", name.with_context(context))?;
            } else {
                write!(f, "\t_")?;
            }

            // aliases
            let mut aliases = table
                .names
                .iter()
                .filter_map(|(alias, TypeId(id))| (*id == n).then_some(alias))
                .peekable();
            if aliases.peek().is_some() {
                write!(f, "\t(")?;
                for item in Iterator::intersperse(aliases.map(Some), None) {
                    match item {
                        Some(alias) => write!(f, "{}", alias.with_context(context)),
                        None => write!(f, ", "),
                    }?
                }
                write!(f, ")")?;
            }

            writeln!(f)?;
            {
                let mut f = indented(f);

                // type
                write!(f, "type = ")?;
                writeln!(
                    f,
                    "{}",
                    typ.with_context(&TypeDisplayContext {
                        table,
                        ident_context: context,
                        depth: 5,
                        expand_composites: true
                    })
                )?;

                // size
                if let Some(size) = size {
                    write!(f, "size = {size}")?;
                } else {
                    write!(f, "size = <unsized>")?;
                }
            }
            writeln!(f)?;
        }
        Ok(())
    }
}

pub struct TypeDisplayContext<'t, S, IC: ?Sized> {
    /// The type table for id resolution
    pub table: &'t TypeTable<S>,
    /// The context for displaying identifiers
    pub ident_context: &'t IC,
    /// Depth for recursing types
    pub depth: usize,
    /// If composites should be expanded
    pub expand_composites: bool,
}

impl<'t, S, IC: ?Sized> TypeDisplayContext<'t, S, IC> {
    /// Context with a reduced depth
    fn pushed(&self) -> Option<Self> {
        match self.depth {
            0 => None,
            _ => Some(Self {
                depth: self.depth - 1,
                expand_composites: false,
                ..*self
            }),
        }
    }
}
