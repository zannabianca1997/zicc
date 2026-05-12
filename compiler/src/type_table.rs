use snafu::{OptionExt, ResultExt, Snafu};
use zicc_compiler_ast::{
    Item,
    type_def::{
        ArrayTypeDef, IntTypeDef, ItemTypeDef, NamedTypeDef, PointerKindDef, PointerTypeDef,
        TypeDef, UnknownTypeDef,
    },
};
use zicc_compiler_lexer::{identifiers::Identifier, int_literal::IntLiteral};
use zicc_compiler_program::Program;
use zicc_compiler_types::{PointerKind, Type, TypeId, TypeTable};
use zicc_limits::{CastValueToIntError, Size, Value};

#[derive(Debug, Snafu)]
pub enum Error {
    UnknowType {
        name: Identifier,
    },
    DoublyDefined {
        name: Identifier,
    },
    ArrayMustHaveSizedElements,
    ArrayLengthMustFitSize {
        length: IntLiteral,
        source: CastValueToIntError,
    },
}

pub(crate) fn fill(program: &Program) -> Result<TypeTable<Identifier>, Error> {
    let mut table = TypeTable::new();

    for item in &program.content.items {
        if let Item::TypeDef(ItemTypeDef { ident, def, .. }) = item {
            let id = register(&mut table, def)?;
            table
                .define(*ident, id)
                .map_err(|(name, _)| Error::DoublyDefined { name })?;
        }
    }

    Ok(table)
}

fn register(table: &mut TypeTable<Identifier>, def: &TypeDef) -> Result<TypeId, Error> {
    Ok(match def {
        TypeDef::Named(NamedTypeDef { name }) => {
            table.named(name).context(UnknowTypeSnafu { name: *name })?
        }
        TypeDef::Int(IntTypeDef { .. }) => table.id(&Type::int()),
        TypeDef::Unknown(UnknownTypeDef { .. }) => table.id(&Type::unknown()),
        TypeDef::Array(boxed_array_type_def) => {
            let ArrayTypeDef {
                element, length, ..
            } = &**boxed_array_type_def;

            let element = register(table, element)?;
            let element = table
                .try_unwrap_sized_id(element)
                .context(ArrayMustHaveSizedElementsSnafu)?;

            let length = Size::try_from(Value::from(length.clone())).with_context(|_| {
                ArrayLengthMustFitSizeSnafu {
                    length: length.clone(),
                }
            })?;

            table.id(&Type::array(element, length))
        }
        TypeDef::Pointer(boxed_pointer_type_def) => {
            let PointerTypeDef { kind, pointed } = &**boxed_pointer_type_def;
            let pointed = register(table, pointed)?;
            let kind = match kind {
                PointerKindDef::Absolute { .. } => PointerKind::Absolute,
                PointerKindDef::Relative { .. } => PointerKind::Relative,
            };
            table.id(&Type::pointer(kind, pointed))
        }
    })
}
