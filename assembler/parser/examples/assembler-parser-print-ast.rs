use ariadne::{Color, Label, Report, ReportKind, Source};
use clap::Parser;
use std::{
    borrow::Cow,
    fs,
    io::{self, Read, stdin},
    path::{Path, PathBuf},
};
use string_interner::DefaultStringInterner;
use zicc_assembler_parser::parse;

/// Parse an IntCode Assembly file and print the resulting AST
#[derive(Debug, Parser)]
struct Cli {
    /// File to parse
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

    let mut interner = DefaultStringInterner::new();
    let (ast, errors) = parse(&text, &mut interner).into_output_errors();

    let source = file
        .as_deref()
        .map(Path::to_string_lossy)
        .unwrap_or(Cow::Borrowed("<stdin>"));

    let ariadne_source = Source::from(&text);

    for err in errors {
        let span = err.span().clone();
        let msg = format!("{}", err.reason());
        Report::build(ReportKind::Error, (&*source, span.start..span.end))
            .with_message(format!("Parse error in {source}"))
            .with_label(
                Label::new((&*source, span.start..span.end))
                    .with_message(msg)
                    .with_color(Color::Red),
            )
            .finish()
            .print((&*source, &ariadne_source))?;
    }

    if let Some(ast) = ast {
        println!("{:#?}", ast);
    }

    Ok(())
}
