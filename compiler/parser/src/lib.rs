//! Parser for the `zicc` ast

use chumsky::{
    IterParser as _, ParseResult, Parser,
    error::Rich,
    extra,
    input::{Input, Stream, ValueInput},
    span::SimpleSpan,
};
use string_interner::DefaultStringInterner;
use zicc_compiler_ast::{File, Item};
use zicc_compiler_lexer::{InvalidToken, Token, lex};

use crate::type_def::item_type_def;

pub type ParserError<'s> = Rich<'s, Result<Token, InvalidToken>>;
pub type ParserExtra<'s> = extra::Err<ParserError<'s>>;

mod atoms;
mod type_def;

/// Parse a zicc file
fn file<'s, I: ValueInput<'s, Token = Result<Token, InvalidToken>, Span = SimpleSpan>>()
-> impl Parser<'s, I, File, ParserExtra<'s>> {
    item().repeated().collect().map(|items| File { items })
}

/// Parse a zicc item
fn item<'s, I: ValueInput<'s, Token = Result<Token, InvalidToken>, Span = SimpleSpan>>()
-> impl Parser<'s, I, Item, ParserExtra<'s>> {
    item_type_def().map(Item::TypeDef)
}

pub fn parse<'s>(
    source: &'s str,
    interner: &'s mut DefaultStringInterner,
) -> ParseResult<File, ParserError<'s>> {
    // Create the lex stream
    let tokens = lex(source, interner);
    // Adapt the lex stream to a parser input with the right spans
    let input = Stream::from_iter(tokens).map(SimpleSpan::splat(source.len()), |(t, s)| {
        (t, SimpleSpan::new(s.start, s.end))
    });
    // Parse the result
    let result = file().parse(input);

    result
}
