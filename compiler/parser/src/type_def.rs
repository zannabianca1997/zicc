use chumsky::{Parser, input::ValueInput, prelude::recursive, span::SimpleSpan};
use zicc_compiler_ast::type_def::{
    ArrayTypeDef, IntTypeDef, ItemTypeDef, NamedTypeDef, PointerKindDef, PointerTypeDef, TypeDef,
    UnknownTypeDef,
};
use zicc_compiler_lexer::{InvalidToken, Token};

use crate::{
    ParserExtra,
    atoms::{identifier, int_literal, keyword, punctuator},
};

/// Parse a type definition item
pub(crate) fn item_type_def<
    's,
    I: ValueInput<'s, Token = Result<Token, InvalidToken>, Span = SimpleSpan>,
>() -> impl Parser<'s, I, ItemTypeDef, ParserExtra<'s>> {
    keyword()
        .then(identifier())
        .then(punctuator())
        .then(type_def())
        .then(punctuator())
        .map(|((((k_type, ident), p_eq), def), p_semi)| ItemTypeDef {
            k_type,
            ident,
            p_eq,
            def,
            p_semi,
        })
}

/// Parse a type definition
pub(crate) fn type_def<
    's,
    I: ValueInput<'s, Token = Result<Token, InvalidToken>, Span = SimpleSpan>,
>() -> impl Parser<'s, I, TypeDef, ParserExtra<'s>> {
    recursive(|type_def| {
        named_type_def()
            .map(TypeDef::Named)
            .or(int_type_def().map(TypeDef::Int))
            .or(unknown_type_def().map(TypeDef::Unknown))
            .or(array_type_def(type_def.clone()).map(|a| TypeDef::Array(Box::new(a))))
            .or(pointer_type_def(type_def).map(|p| TypeDef::Pointer(Box::new(p))))
    })
}

pub(crate) fn named_type_def<
    's,
    I: ValueInput<'s, Token = Result<Token, InvalidToken>, Span = SimpleSpan>,
>() -> impl Parser<'s, I, NamedTypeDef, ParserExtra<'s>> + Clone {
    identifier().map(|name| NamedTypeDef { name })
}

pub(crate) fn int_type_def<
    's,
    I: ValueInput<'s, Token = Result<Token, InvalidToken>, Span = SimpleSpan>,
>() -> impl Parser<'s, I, IntTypeDef, ParserExtra<'s>> + Clone {
    keyword().map(|k_int| IntTypeDef { k_int })
}

fn array_type_def<'s, I: ValueInput<'s, Token = Result<Token, InvalidToken>, Span = SimpleSpan>>(
    type_def: impl Parser<'s, I, TypeDef, ParserExtra<'s>> + Clone,
) -> impl Parser<'s, I, ArrayTypeDef, ParserExtra<'s>> + Clone {
    punctuator()
        .then(type_def)
        .then(punctuator())
        .then(int_literal())
        .then(punctuator())
        .map(
            |((((p_bracket_open, element), p_semicolon), length), p_bracket_close)| ArrayTypeDef {
                p_bracket_open,
                element,
                p_semicolon,
                length,
                p_bracket_close,
            },
        )
}
pub(crate) fn unknown_type_def<
    's,
    I: ValueInput<'s, Token = Result<Token, InvalidToken>, Span = SimpleSpan>,
>() -> impl Parser<'s, I, UnknownTypeDef, ParserExtra<'s>> + Clone {
    punctuator().map(|p_underscore| UnknownTypeDef { p_underscore })
}
fn pointer_type_def<
    's,
    I: ValueInput<'s, Token = Result<Token, InvalidToken>, Span = SimpleSpan>,
>(
    type_def: impl Parser<'s, I, TypeDef, ParserExtra<'s>> + Clone,
) -> impl Parser<'s, I, PointerTypeDef, ParserExtra<'s>> + Clone {
    pointer_kind_def()
        .then(type_def)
        .map(|(kind, pointed)| PointerTypeDef { kind, pointed })
}

fn pointer_kind_def<
    's,
    I: ValueInput<'s, Token = Result<Token, InvalidToken>, Span = SimpleSpan>,
>() -> impl Parser<'s, I, PointerKindDef, ParserExtra<'s>> + Clone {
    punctuator()
        .map(|p_at| PointerKindDef::Relative { p_at })
        .or(punctuator().map(|p_ampersand| PointerKindDef::Absolute { p_ampersand }))
}
