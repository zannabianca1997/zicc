use chumsky::{
    Parser,
    input::ValueInput,
    label::LabelError,
    prelude::{any, just},
    span::SimpleSpan,
};
use zicc_compiler_lexer::{
    InvalidToken, Token, identifiers::Identifier, int_literal::IntLiteral, keywords::Keyword,
    punctuators::Punctuator,
};

use crate::ParserExtra;

/// Parse a specific keyword
pub(crate) fn keyword<'s, K, I>() -> impl Parser<'s, I, K, ParserExtra<'s>> + Clone
where
    K: Default + Into<Keyword>,
    I: ValueInput<'s, Token = Result<Token, InvalidToken>, Span = SimpleSpan>,
{
    just(Ok(Token::Keyword(K::default().into()))).map(|_| K::default())
}

/// Parse a specific punctuator
pub(crate) fn punctuator<'s, P, I>() -> impl Parser<'s, I, P, ParserExtra<'s>> + Clone
where
    P: Default + Into<Punctuator>,
    I: ValueInput<'s, Token = Result<Token, InvalidToken>, Span = SimpleSpan>,
{
    just(Ok(Token::Punctuator(P::default().into()))).map(|_| P::default())
}

/// Parse an identifier
pub(crate) fn identifier<'s, I>() -> impl Parser<'s, I, Identifier, ParserExtra<'s>> + Clone
where
    I: ValueInput<'s, Token = Result<Token, InvalidToken>, Span = SimpleSpan>,
{
    any().try_map(|t, span| {
        if let Ok(Token::Identifier(ident)) = t {
            Ok(ident)
        } else {
            Err(LabelError::<I, _>::expected_found(
                ["identifier"],
                Some(t.into()),
                span,
            ))
        }
    })
}

/// Parse a int literal
pub(crate) fn int_literal<'s, I>() -> impl Parser<'s, I, IntLiteral, ParserExtra<'s>> + Clone
where
    I: ValueInput<'s, Token = Result<Token, InvalidToken>, Span = SimpleSpan>,
{
    any().try_map(|t, span| {
        if let Ok(Token::IntLiteral(int)) = t {
            Ok(int)
        } else {
            Err(LabelError::<I, _>::expected_found(
                ["int literal"],
                Some(t.into()),
                span,
            ))
        }
    })
}
