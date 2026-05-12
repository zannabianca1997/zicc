use chumsky::{Parser, input::ValueInput, span::SimpleSpan};
use zicc_compiler_ast::function::{Argument, ItemFunction, ReturnType};
use zicc_compiler_lexer::{InvalidToken, Token};

use crate::{
    ParserExtra,
    atoms::{identifier, keyword, punctuator},
    punctuated::punctuated,
    type_def::type_def,
};

/// Parse a function item
pub fn item_function<'s, I>() -> impl Parser<'s, I, ItemFunction, ParserExtra<'s>>
where
    I: ValueInput<'s, Token = Result<Token, InvalidToken>, Span = SimpleSpan>,
{
    keyword()
        .then(identifier())
        .then(punctuator())
        .then(punctuated(argument(), punctuator()))
        .then(punctuator())
        .then(return_type().or_not())
        .then(punctuator())
        .then(punctuator())
        .map(
            |(
                (
                    (
                        ((((k_fn, ident), p_parentheses_open), arg_list), p_parentheses_close),
                        return_type,
                    ),
                    p_brace_open,
                ),
                p_brace_close,
            )| ItemFunction {
                k_fn,
                ident,
                p_parentheses_open,
                arg_list,
                p_parentheses_close,
                return_type,
                p_brace_open,
                p_brace_close,
            },
        )
}

fn argument<'s, I>() -> impl Parser<'s, I, Argument, ParserExtra<'s>> + Clone
where
    I: ValueInput<'s, Token = Result<Token, InvalidToken>, Span = SimpleSpan>,
{
    identifier()
        .then(punctuator())
        .then(type_def())
        .map(|((ident, p_colon), type_def)| Argument {
            ident,
            p_colon,
            type_def,
        })
}

fn return_type<'s, I>() -> impl Parser<'s, I, ReturnType, ParserExtra<'s>> + Clone
where
    I: ValueInput<'s, Token = Result<Token, InvalidToken>, Span = SimpleSpan>,
{
    punctuator()
        .then(type_def())
        .map(|(p_right_arrow, type_def)| ReturnType {
            p_right_arrow,
            type_def,
        })
}
