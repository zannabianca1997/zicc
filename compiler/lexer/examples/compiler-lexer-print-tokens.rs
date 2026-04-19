use ariadne::{Color, ColorGenerator, Label, Report, ReportKind, Source};
use clap::Parser;
use std::{
    borrow::Cow,
    collections::HashMap,
    fs,
    io::{self, Read, stdin},
    path::{Path, PathBuf},
};
use string_interner::StringInterner;
use zicc_compiler_lexer::lex;

/// Tokenize a file, and parse the result
#[derive(Debug, Parser)]
struct Cli {
    /// File to lex
    file: Option<PathBuf>,
}

fn main() -> io::Result<()> {
    let Cli { file } = Cli::parse();
    let text = if let Some(file) = &file {
        fs::read_to_string(file)?
    } else {
        let mut buf = String::new();
        stdin().read_to_string(&mut buf)?;
        buf
    };

    let mut interner = StringInterner::new();
    let tokens: Vec<_> = lex(&text, &mut interner).collect();

    let source = file
        .as_deref()
        .map(Path::to_string_lossy)
        .unwrap_or(Cow::Borrowed("<stdin>"));

    let text = Source::from(text);

    let mut color_generator = ColorGenerator::new();
    let mut colors = HashMap::new();

    for (_, s) in tokens
        .iter()
        .filter_map(|(t, s)| t.as_ref().err().map(|e| (e, s)))
    {
        Report::build(ReportKind::Error, (&*source, s.clone()))
            .with_message(format!("Tokenization error in {source}"))
            .with_label(
                Label::new((&*source, s.clone()))
                    .with_message("Invalid token")
                    .with_color(Color::Red),
            )
            .finish()
            .print((&*source, &text))?;
    }

    Report::build(
        ReportKind::Custom("Tokenization", ariadne::Color::Green),
        (&*source, 0..source.len()),
    )
    .with_message(format!("{source} was split into {} tokens", tokens.len()))
    .with_labels(tokens.into_iter().filter_map(|(t, s)| {
        t.ok().map(|t| {
            Label::new((&*source, s))
                .with_message(format!("{t:?}"))
                .with_color(
                    colors
                        .entry(t)
                        .or_insert_with(|| color_generator.next())
                        .clone(),
                )
        })
    }))
    .finish()
    .print((&*source, text))?;
    Ok(())
}
