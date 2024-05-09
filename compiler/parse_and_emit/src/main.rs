//! Parse a `.zicc` source file and reemit it, with additional info
#![feature(error_reporter)]

use std::{
    cell::RefCell,
    error::Report,
    fmt::{self, Write as _},
    io::{self, stdin, stdout, Read as _, Write as _},
    rc::Rc,
};

use ast::ParseError;
use display_context::DisplayWithContext;
use errors::Multiple;
use indenter::indented;
use string_interner::DefaultStringInterner;
use thiserror::Error;
use types::TypeTable;

#[derive(Debug, Error)]
enum MainError {
    #[error(transparent)]
    IO(#[from] io::Error),
    #[error(transparent)]
    Fmt(#[from] fmt::Error),
    #[error("Multiple errors in parsing")]
    Parse(Multiple<ParseError>),
}

struct Io2Fmt<T>(T);

impl<T> std::fmt::Write for Io2Fmt<T>
where
    T: std::io::Write,
{
    fn write_str(&mut self, s: &str) -> std::fmt::Result {
        self.0
            .write(s.as_bytes())
            .map(|_| ())
            .map_err(|_| std::fmt::Error)
    }
}

fn main() -> Result<(), Report<MainError>> {
    main_unreported()?;
    Ok(())
}

fn main_unreported() -> Result<(), MainError> {
    let mut input = String::new();
    stdin().read_to_string(&mut input)?;
    let input = input.into_boxed_str();

    let interner = Rc::new(RefCell::new(DefaultStringInterner::new()));

    let ast = match ast::parse(&input, None, interner.clone()) {
        Ok(ast) => ast,
        Err(errs) => {
            for err in &errs.childs {
                eprintln!("{}", err)
            }
            return Err(MainError::Parse(errs));
        }
    };

    let type_table = TypeTable::build(ast);

    // unparse
    let mut out = stdout();

    writeln!(out, "Source:")?;
    writeln!(out)?;
    {
        let mut out = Io2Fmt(&mut out);
        let mut out = indented(&mut out);
        writeln!(out, "{}", ast.with_context(&*interner.borrow()))?;
    }
    writeln!(out)?;
    writeln!(out, "Types:")?;
    writeln!(out)?;
    {
        let mut out = Io2Fmt(&mut out);
        let mut out = indented(&mut out);
        writeln!(out, "")?;
    }

    Ok(())
}
