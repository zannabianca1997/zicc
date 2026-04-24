use std::mem;

use chumsky::{
    IterParser, ParseResult, Parser,
    error::Rich,
    extra::{self, SimpleState},
    text::inline_whitespace,
};
use string_interner::DefaultStringInterner;
use zicc_assembler_ast::{File, Line, labelled::Labelled};

use crate::{
    directive::directive, instruction::instruction, labelled::labelled, misc::line_separator,
};

pub type ParserError<'s> = Rich<'s, char>;
type ParserExtra<'s> = extra::Full<ParserError<'s>, SimpleState<ParseState<'s>>, ()>;

struct ParseState<'s> {
    interner: &'s mut DefaultStringInterner,
}

mod directive;
mod identifier;
mod instruction;
mod labelled;
mod misc;
mod expr;

/// Parse a IntCode Assembly file
fn file<'s>() -> impl Parser<'s, &'s str, File, ParserExtra<'s>> {
    line()
        .separated_by(line_separator())
        .allow_trailing()
        .collect()
        // Merge the lines together, moving labels to the next line if the line is empty
        .map(|lines: Vec<_>| {
            let lines_len = lines.len();
            lines.into_iter().fold(
                File {
                    lines: Vec::with_capacity(lines_len),
                    trailing: Labelled::unlabelled(()),
                },
                |mut file, line| {
                    match line.try_unwrap() {
                        Ok(mut full_line) => {
                            full_line.merge(mem::take(&mut file.trailing));
                            file.lines.push(full_line);
                        }
                        Err(empty_line_labels) => file.trailing.merge(empty_line_labels),
                    }
                    file
                },
            )
        })
}

/// Parse a IntCode Assembly line
fn line<'s>() -> impl Parser<'s, &'s str, Labelled<Option<Line>>, ParserExtra<'s>> {
    labelled(
        instruction()
            .map(Line::Instruction)
            .or(directive().map(Line::Directive))
            .or_not(),
    )
    .padded_by(inline_whitespace())
    .labelled("line")
}

pub fn parse<'s>(
    source: &'s str,
    interner: &'s mut DefaultStringInterner,
) -> ParseResult<File, ParserError<'s>> {
    file().parse_with_state(source, &mut SimpleState(ParseState { interner }))
}
