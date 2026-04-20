use ariadne::{Color, Label, Report, ReportKind, Source};
use clap::Parser;
use std::{
    borrow::Cow,
    cell::RefCell,
    fs,
    io::{self, Read, stdin},
    path::{Path, PathBuf},
};
use string_interner::StringInterner;
use zicc_compiler_lexer::{InvalidToken, display::Displayable};
use zicc_compiler_parser::parse;

/// Parse a file and print the resulting AST
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

    let interner = RefCell::new(StringInterner::new());
    let (ast, errors) = parse(&text, &interner).into_output_errors();

    let source = file
        .as_deref()
        .map(Path::to_string_lossy)
        .unwrap_or(Cow::Borrowed("<stdin>"));

    let ariadne_source = Source::from(&text);

    for err in errors {
        let span = err.span().clone();
        let err = err.map_token(|t| match t {
            Ok(t) => Cow::Owned(t.display(&*interner.borrow()).to_string()),
            Err(InvalidToken) => Cow::Borrowed("invalid token"),
        });
        Report::build(ReportKind::Error, (&*source, span.start..span.end))
            .with_message(format!("Parse error in {source}"))
            .with_label(
                Label::new((&*source, span.start..span.end))
                    .with_message(format!("{}", err.reason().to_string()))
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
