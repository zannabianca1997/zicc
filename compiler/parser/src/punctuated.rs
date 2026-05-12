use chumsky::{Parser, input::ValueInput, span::SimpleSpan};
use zicc_compiler_ast::punctuated::Punctuated;
use zicc_compiler_lexer::{InvalidToken, Token};

use crate::ParserExtra;

/// Parse a punctuated list
pub fn punctuated<'s, P, T, I>(
    item: impl Parser<'s, I, T, ParserExtra<'s>> + Clone,
    punct: impl Parser<'s, I, P, ParserExtra<'s>> + Clone,
) -> impl Parser<'s, I, Punctuated<T, P>, ParserExtra<'s>>
where
    I: ValueInput<'s, Token = Result<Token, InvalidToken>, Span = SimpleSpan>,
{
    // A simpler equivalent parser would be `(item punct)* item?` but that
    // would parse item twice at the end. This assumes that punctuators are
    // simpler to parse than items (often they are a single punctuators) and
    // never backtrack on an item.
    item.clone()
        .map(|first| {
            let mut punctuated = Punctuated::new();
            punctuated.push_item(first);
            punctuated
        })
        .foldl(
            punct.clone().then(item).repeated(),
            |mut punctuated, (punct, item)| {
                punctuated.push_punctuator(punct);
                punctuated.push_item(item);
                punctuated
            },
        )
        .then(punct.or_not())
        .map(|(mut punctuated, trailing)| {
            if let Some(trailing) = trailing {
                punctuated.push_punctuator(trailing);
            }
            punctuated
        })
        .or_not()
        .map(|punctuated| punctuated.unwrap_or_default())
}
