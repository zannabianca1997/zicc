use std::mem;

use chumsky::{
    IterParser, Parser,
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

mod misc {
    use chumsky::{
        Parser,
        prelude::{any, just},
        text::newline,
    };

    use crate::ParserExtra;

    /// Separator between two lines
    pub(crate) fn line_separator<'s>() -> impl Parser<'s, &'s str, (), ParserExtra<'s>> {
        // End of line comment: `;` followed by any char that is not a newline
        just(";")
            .then(any().and_is(newline().not()).repeated())
            .or_not()
            .then(newline())
            .ignored()
    }
}
mod identifier {
    use chumsky::{Parser, prelude::todo};
    use zicc_assembler_ast::identifier::Identifier;

    use crate::ParserExtra;

    pub(crate) fn identifier<'s, T>() -> impl Parser<'s, &'s str, Identifier, ParserExtra<'s>> {
        todo()
    }
}
mod labelled {
    use chumsky::{Parser, prelude::todo};
    use zicc_assembler_ast::labelled::Labelled;

    use crate::ParserExtra;

    pub(crate) fn labelled<'s, T>(
        parser: impl Parser<'s, &'s str, T, ParserExtra<'s>>,
    ) -> impl Parser<'s, &'s str, Labelled<T>, ParserExtra<'s>> {
        todo()
    }
}
mod instruction {
    use chumsky::{Parser, prelude::todo};
    use zicc_assembler_ast::instruction::Instruction;

    use crate::ParserExtra;

    /// A single instruction
    pub(crate) fn instruction<'s>() -> impl Parser<'s, &'s str, Instruction, ParserExtra<'s>> {
        todo()
    }
}
mod directive {
    use chumsky::{Parser, prelude::todo};
    use zicc_assembler_ast::directive::Directive;

    use crate::ParserExtra;

    /// A single directive
    pub(crate) fn directive<'s>() -> impl Parser<'s, &'s str, Directive, ParserExtra<'s>> {
        todo()
    }
}

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
