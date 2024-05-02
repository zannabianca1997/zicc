#![feature(unwrap_infallible)]

use std::{
    fmt::Display,
    io::{self, stderr, stdin, stdout, Read, StdinLock, StdoutLock, Write},
    num::NonZeroUsize,
    str::FromStr,
};

use anyhow::{bail, Context};
use assembler::{AssembleError, Code};
use clap::{Args, Parser, ValueEnum};
use errors::{Accumulator, Multiple, RootAccumulator};
use path_or_dash::{FileOrStdin, FileOrStdout, PathOrDash};
use utf8_chars::BufReadCharsExt;
use vm::VMInt;
mod path_or_dash;

const MAGIC: [u8; 5] = *b"ZICC\xFF";

/// Format of the file
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, ValueEnum)]
enum Format {
    /// Source file
    Source,
    /// Binary compact format
    Binary,
    /// Json
    Json,
    /// Yaml
    Yaml,
}

/// Possible filetypes
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, ValueEnum)]
enum Filetype {
    /// Incode C sources
    ICC,
    /// Intcode Assembly sources
    ICA,
    /// Intcode Objects
    ICO,
    /// Intcode programs
    INTS,
}

/// Possible engine io formats
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, ValueEnum)]
enum IOFormat {
    /// Ascii numbers separated by spaces, commas or comments (like INTS sources)
    INTS,
    /// Numbers are interpreted as bytes
    BYTES,
}

/// Assemble the given file (.ica) in intcode object file (.ico)
#[derive(Debug, Clone, Args)]
pub struct Assemble {
    /// Input file to process
    #[arg(default_value = "-")]
    input: PathOrDash,

    /// Type of the input file
    #[arg(long)]
    input_type: Option<Filetype>,
    /// Format of the input file
    #[arg(long)]
    input_format: Option<Format>,
    /// Output file to generate
    #[arg(long, short)]
    output: Option<PathOrDash>,
    /// Format of the output file
    #[arg(short = 'f', long, default_value = "binary")]
    output_format: Format,
}

mod guess;

mod parsers {
    pub mod ica {
        use std::str::from_utf8;

        use anyhow::Context;
        use assembler::parser::{ast::File, ParseError};
        use errors::Accumulator;

        use crate::MAGIC;

        pub fn source<'s>(
            input: &'s [u8],
            mut errors: impl Accumulator<Error = anyhow::Error>,
        ) -> Option<File<'s>> {
            let input = errors.handle(from_utf8(input).context("Source is invalid utf-8"))?;
            assembler::parser::parse(
                input,
                &mut errors.as_mapped(|e: ParseError| anyhow::Error::new(e)),
            )
        }

        pub fn binary<'s>(input: &'s [u8]) -> anyhow::Result<File<'s>> {
            let input = input.strip_prefix(&MAGIC).and_then(|i| i.strip_prefix(b"ICA")).context("The binary format for a .ica must start with the magic number followed by `ICA`")?;
            bincode::borrow_decode_from_slice(input, bincode::config::standard())
                .map(|(f, _)| f)
                .context("Cannot decode binary format")
        }

        pub(crate) fn json<'s>(input_content: &'s [u8]) -> anyhow::Result<File<'s>> {
            serde_json::from_slice(input_content).context("Invalid json ast")
        }

        pub(crate) fn yaml<'s>(input_content: &'s [u8]) -> anyhow::Result<File<'s>> {
            serde_yaml::from_slice(input_content).context("Invalid yaml ast")
        }
    }

    pub mod ico {
        use anyhow::Context;
        use assembler::Unit;

        use crate::MAGIC;

        pub fn binary<'s>(input: &'s [u8]) -> anyhow::Result<Unit<'s>> {
            let input = input.strip_prefix(&MAGIC).and_then(|i| i.strip_prefix(b"ICO")).context("The binary format for a .ico must start with the magic number followed by `ICO`")?;
            bincode::borrow_decode_from_slice(input, bincode::config::standard())
                .map(|(f, _)| f)
                .context("Cannot decode binary format")
        }

        pub(crate) fn json<'s>(input_content: &'s [u8]) -> anyhow::Result<Unit<'s>> {
            serde_json::from_slice(input_content).context("Invalid json object file")
        }

        pub(crate) fn yaml<'s>(input_content: &'s [u8]) -> anyhow::Result<Unit<'s>> {
            serde_yaml::from_slice(input_content).context("Invalid yaml object file")
        }
    }

    pub mod ints {
        use std::str::from_utf8;

        use anyhow::Context;
        use lazy_regex::regex_captures;
        use vm::VMInt;

        use crate::MAGIC;

        pub fn source(input: &[u8]) -> anyhow::Result<Vec<VMInt>> {
            let mut input = from_utf8(input).context("Source is not valid utf8")?;
            let mut res = vec![];
            while let Some((full, num)) = regex_captures!(
                r"(?:/\*[^*]*\*+(?:[^/*][^*]*\*+)*/|//[^\n$]*(?:\n|$)|-[^\d]|[^\d-])*(-?\d+)",
                input
            ) {
                res.push(num.parse().context("Integer overflow")?);
                input = &input[full.len()..]
            }
            Ok(res)
        }

        pub fn binary(input: &[u8]) -> anyhow::Result<Vec<VMInt>> {
            let input = input.strip_prefix(&MAGIC).and_then(|i| i.strip_prefix(b"INTS")).context("The binary format for a .ints must start with the magic number followed by `INTS`")?;
            bincode::borrow_decode_from_slice(input, bincode::config::standard())
                .map(|(f, _)| f)
                .context("Cannot decode binary format")
        }

        pub(crate) fn json(input: &[u8]) -> anyhow::Result<Vec<VMInt>> {
            serde_json::from_slice(input).context("Invalid json object file")
        }

        pub(crate) fn yaml(input: &[u8]) -> anyhow::Result<Vec<VMInt>> {
            serde_yaml::from_slice(input).context("Invalid yaml object file")
        }
    }
}
mod emitters {
    pub mod ico {
        use std::io;

        use anyhow::Context;
        use assembler::Unit;

        use crate::MAGIC;

        pub fn binary(unit: Unit, mut output: impl io::Write) -> anyhow::Result<()> {
            output.write_all(&MAGIC)?;
            output.write_all(b"ICO")?;
            bincode::encode_into_std_write(unit, &mut output, bincode::config::standard())
                .context("Cannot encode unit")?;
            Ok(())
        }
        pub fn json(unit: Unit, output: impl io::Write) -> anyhow::Result<()> {
            serde_json::to_writer(output, &unit)?;
            Ok(())
        }
        pub fn yaml(unit: Unit, output: impl io::Write) -> anyhow::Result<()> {
            serde_yaml::to_writer(output, &unit)?;
            Ok(())
        }
    }
    pub mod ints {
        use std::io;

        use anyhow::Context;
        use itertools::Itertools;
        use vm::VMInt;

        use crate::MAGIC;

        pub fn source(code: Vec<VMInt>, mut output: impl io::Write) -> anyhow::Result<()> {
            write!(output, "{}", code.into_iter().format(", "))?;
            Ok(())
        }

        pub fn binary(code: Vec<VMInt>, mut output: impl io::Write) -> anyhow::Result<()> {
            output.write_all(&MAGIC)?;
            output.write_all(b"INTS")?;
            bincode::encode_into_std_write(code, &mut output, bincode::config::standard())
                .context("Cannot encode code")?;
            Ok(())
        }
        pub fn json(code: Vec<VMInt>, output: impl io::Write) -> anyhow::Result<()> {
            serde_json::to_writer(output, &code)?;
            Ok(())
        }
        pub fn yaml(code: Vec<VMInt>, output: impl io::Write) -> anyhow::Result<()> {
            serde_yaml::to_writer(output, &code)?;
            Ok(())
        }
    }
}

fn print_errs(errs: Multiple<impl Display>) {
    for err in errs {
        eprintln!("{err}")
    }
}

fn assemble(
    Assemble {
        input: input_path,
        input_format,
        input_type,
        output,
        output_format,
    }: Assemble,
) -> anyhow::Result<()> {
    // reading input
    let input_content = FileOrStdin::open(&input_path)
        .and_then(|mut f| {
            let mut buf = vec![];
            f.read_to_end(&mut buf)?;
            Ok(buf)
        })
        .context("Error in reading input file")?;

    let (input_type, input_format) = match (input_type, input_format) {
        (None, None) => guess::type_and_format(&input_path, &input_content)?,
        (None, Some(f)) => (guess::r#type(&input_path, &input_content)?, f),
        (Some(t), None) => (t, guess::format(t, &input_content)?),
        (Some(t), Some(f)) => (t, f),
    };

    let ast = match (input_type, input_format) {
        (Filetype::ICC, _) => todo!(),
        (Filetype::ICA, Format::Source) => {
            let mut errors = RootAccumulator::new();
            let ast = parsers::ica::source(&input_content, &mut errors);
            if let Err(errs) = errors.finish_with(()) {
                print_errs(errs);
                bail!("Error during parsing")
            }
            ast.unwrap()
        }
        (Filetype::ICA, Format::Binary) => parsers::ica::binary(&input_content)?,
        (Filetype::ICA, Format::Json) => parsers::ica::json(&input_content)?,
        (Filetype::ICA, Format::Yaml) => parsers::ica::yaml(&input_content)?,
        (Filetype::ICO | Filetype::INTS, _) => bail!("The file is already assembled"),
    };

    let mut errors = RootAccumulator::<AssembleError>::new();
    let unit = assembler::Unit::assemble(ast, &mut errors);
    let unit = match errors.finish_with(unit) {
        Ok(unit) => unit,
        Err(errs) => {
            print_errs(errs);
            bail!("Errors during assembling")
        }
    };

    let output = output.unwrap_or_else(|| input_path.with_extension("ico"));
    let output = FileOrStdout::create(&output).context("Cannot create output file")?;
    match output_format {
        Format::Source => bail!(".ico files does not have a source representation"),
        Format::Binary => {
            emitters::ico::binary(unit, output).context("Cannot write binary representation")?
        }
        Format::Json => {
            emitters::ico::json(unit, output).context("Cannot write json representation")?
        }
        Format::Yaml => {
            emitters::ico::yaml(unit, output).context("Cannot write yaml representation")?
        }
    }
    Ok(())
}

/// Link the given files (.ico)
#[derive(Debug, Clone, Args)]
pub struct Link {
    /// Input files to process
    inputs: Vec<PathOrDash>,

    /// Type of the input files, if omogeneus
    #[arg(long)]
    input_type: Option<Filetype>,
    /// Format of the input files, if omogeneus
    #[arg(long)]
    input_format: Option<Format>,
    /// Output file to generate
    #[arg(short, long)]
    output: Option<PathOrDash>,
    /// Format of the output file
    #[arg(short = 'f', long, default_value = "binary")]
    output_format: Format,
}

fn link(
    Link {
        inputs,
        input_type,
        input_format,
        output,
        output_format,
    }: Link,
) -> anyhow::Result<()> {
    if inputs
        .iter()
        .filter(|i| matches!(i, PathOrDash::Dash))
        .count()
        > 1
    {
        bail!("Cannot read stdin more than once")
    }
    let mut output = output.or_else(|| {
        // if is a single file, we can guess the name
        let [inp] = &inputs[..] else {
            return None;
        };
        Some(inp.with_extension("ints"))
    });
    let mut errors = RootAccumulator::<anyhow::Error>::new();
    let sources: Vec<_> = errors
        .handle_iter(
            inputs
                .into_iter()
                .map(|i| -> anyhow::Result<(PathOrDash, Vec<u8>)> {
                    let mut file = FileOrStdin::open(&i).context(format!("While reading {i}"))?;
                    let mut buf = vec![];
                    file.read_to_end(&mut buf)
                        .context(format!("While reading {i}"))?;
                    Ok((i, buf))
                }),
        )
        .flatten()
        .collect();
    let mut ass_errors = RootAccumulator::<AssembleError>::new();
    let units: Vec<_> = sources
        .iter()
        .map(|(i, c)| {
            source_to_unit(i, input_type, input_format, c, &mut errors, &mut ass_errors)
                .map(|u| (i, u))
        })
        .flatten()
        .collect();
    if let Err(errs) = errors.checkpoint() {
        print_errs(errs);
        bail!("Errors while reading inputs")
    }

    let mut code = Code::new();
    for (path, unit) in units {
        if output.is_none() && unit.is_entry() {
            output = Some(path.with_extension("ints"))
        }
        code.push_unit(unit, &mut ass_errors)
    }
    let code = code.emit(&mut ass_errors);
    let code = match ass_errors.finish_with(code) {
        Ok(code) => code,
        Err(errs) => {
            print_errs(errs);
            bail!("Errors during assembly")
        }
    };

    // printing to output
    let Some(output) = output else {
        bail!("Cannot determine output name")
    };
    let output = FileOrStdout::create(&output).context("Cannot create output file")?;
    match output_format {
        Format::Source => {
            emitters::ints::source(code, output).context("Cannot write source representation")?
        }
        Format::Binary => {
            emitters::ints::binary(code, output).context("Cannot write binary representation")?
        }
        Format::Json => {
            emitters::ints::json(code, output).context("Cannot write json representation")?
        }
        Format::Yaml => {
            emitters::ints::yaml(code, output).context("Cannot write yaml representation")?
        }
    }
    Ok(())
}

fn source_to_unit<'s>(
    input_path: &PathOrDash,
    input_type: Option<Filetype>,
    input_format: Option<Format>,
    input_content: &'s [u8],
    mut errors: &mut impl Accumulator<Error = anyhow::Error>,
    ass_errors: &mut impl Accumulator<Error = AssembleError<'s>>,
) -> Option<assembler::Unit<'s>> {
    // guessing the file type and format
    let (input_type, input_format) = match (input_type, input_format) {
        (None, None) => errors.handle(guess::type_and_format(&input_path, &input_content))?,
        (None, Some(f)) => (
            errors.handle(guess::r#type(&input_path, &input_content))?,
            f,
        ),
        (Some(t), None) => (t, errors.handle(guess::format(t, &input_content))?),
        (Some(t), Some(f)) => (t, f),
    };

    match (input_type, input_format) {
        (Filetype::ICC, _) => todo!(),
        (Filetype::ICA, _) => {
            let ast = match input_format {
                Format::Source => parsers::ica::source(input_content, &mut errors),
                Format::Binary => errors.handle(parsers::ica::binary(input_content)),
                Format::Json => errors.handle(parsers::ica::json(input_content)),
                Format::Yaml => errors.handle(parsers::ica::yaml(input_content)),
            }?;
            let unit = assembler::Unit::assemble(ast, ass_errors);
            Some(unit)
        }
        (Filetype::ICO, Format::Source) => {
            errors.push(anyhow::Error::msg(format!(
                "{input_path}: ICO files does not have a source format"
            )));
            None
        }
        (Filetype::ICO, Format::Binary) => errors.handle(parsers::ico::binary(input_content)),
        (Filetype::ICO, Format::Json) => errors.handle(parsers::ico::json(input_content)),
        (Filetype::ICO, Format::Yaml) => errors.handle(parsers::ico::yaml(input_content)),
        (Filetype::INTS, _) => {
            errors.push(anyhow::Error::msg(format!(
                "The file {input_path} is already linked"
            )));
            None
        }
    }
}

/// Run the given file (.ints)
#[derive(Debug, Clone, Args)]
pub struct Run {
    /// .ints file to run
    #[arg(default_value = "-")]
    executable: PathOrDash,
    /// Format of the running file
    #[arg(long)]
    format: Option<Format>,
    /// Format of the input stream
    #[arg(long, short, default_value = "bytes")]
    input: IOFormat,
    /// Format of the output stream
    #[arg(long, short, default_value = "bytes")]
    output: IOFormat,
    /// How often, if any, debug on stdout
    #[arg(long, short)]
    debug_rate: Option<Option<NonZeroUsize>>,
    /// Arguments for the debug printout
    #[command(flatten)]
    debug_args: DebugArgs,
}

/// Arguments for the debug printout
#[derive(Debug, Clone, Copy, Args)]
struct DebugArgs {
    /// How large is the window around pc
    #[arg(long, default_value = "8")]
    pc_window: DashOrT<NonZeroUsize>,
    /// How large is the window around pc
    #[arg(long, default_value = "10")]
    rb_window: DashOrT<NonZeroUsize>,
}

/// Arguments for the debug printout
#[derive(Debug, Clone, Copy)]
enum DashOrT<T> {
    Dash,
    T(T),
}
impl<T: FromStr> FromStr for DashOrT<T> {
    type Err = T::Err;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s == "-" {
            Ok(Self::Dash)
        } else {
            T::from_str(s).map(Self::T)
        }
    }
}

mod format_functions {
    use std::io::{self, ErrorKind, StdoutLock, Write};

    use vm::VMInt;

    pub fn inp_ints(
        stream: &mut impl Iterator<Item = io::Result<char>>,
    ) -> io::Result<Option<VMInt>> {
        #[derive(Debug, Clone, Copy)]
        enum SM {
            Ignoring,
            Slash,
            Dash,
            MultilineComment,
            MultilineCommentStar,
            InlineComment,
            Number(bool, VMInt),
        }

        let mut sm = SM::Ignoring;
        while let Some(ch) = stream.next().transpose()? {
            sm = match (sm, ch) {
                // Detection of the possible starts
                (SM::Ignoring | SM::Dash, '/') => SM::Slash,
                (SM::Ignoring | SM::Slash | SM::Dash, '-') => SM::Dash,
                (SM::Slash, '*') => SM::MultilineComment,
                (SM::Slash, '/') => SM::InlineComment,

                // Number starts
                (SM::Ignoring | SM::Slash, digit @ '0'..='9') => {
                    SM::Number(false, digit.to_digit(10).unwrap().into())
                }
                (SM::Dash, digit @ '0'..='9') => {
                    SM::Number(true, digit.to_digit(10).unwrap().into())
                }

                // false allarms
                (SM::Ignoring | SM::Slash | SM::Dash, _) => SM::Ignoring,

                // end of multiline
                (SM::MultilineComment | SM::MultilineCommentStar, '*') => SM::MultilineCommentStar,
                (SM::MultilineCommentStar, '/') => SM::Ignoring,
                (SM::MultilineComment | SM::MultilineCommentStar, _) => SM::MultilineComment,

                // end of inline
                (SM::InlineComment, '\n') => SM::Ignoring,
                (SM::InlineComment, _) => SM::InlineComment,

                // read number
                (SM::Number(sign, num), digit @ '0'..='9') => SM::Number(sign, {
                    num.checked_mul(10)
                        .and_then(|n| n.checked_add(digit.to_digit(10).unwrap().into()))
                        .ok_or_else(|| io::Error::new(ErrorKind::Other, "Overflow in integer"))?
                }),

                // end number
                (SM::Number(..), '-') => return Err(io::Error::new(
                    ErrorKind::Other,
                    "Numbers must be separated by at least a non number char (`3-2` is invalid)",
                )),
                (SM::Number(..), _) => break,
            }
        }
        // eof
        if let SM::Number(sign, n) = sm {
            if sign {
                n.checked_neg()
                    .map(Some)
                    .ok_or_else(|| io::Error::new(ErrorKind::Other, "Overflow in negative integer"))
            } else {
                Ok(Some(n))
            }
        } else {
            Ok(None)
        }
    }

    pub fn inp_bytes(
        stream: &mut impl Iterator<Item = io::Result<char>>,
    ) -> io::Result<Option<VMInt>> {
        stream
            .next()
            .transpose()
            .map(|o| o.map(|n| VMInt::from(n as u32)))
    }

    pub fn out_ints(lock: &mut StdoutLock, n: VMInt) -> io::Result<()> {
        write!(lock, "{n}, ")
    }
    pub fn out_bytes(lock: &mut StdoutLock, n: VMInt) -> io::Result<()> {
        let ch = u32::try_from(n)
            .map_err(|err| io::Error::new(ErrorKind::Other, err))
            .and_then(|n| char::try_from(n).map_err(|err| io::Error::new(ErrorKind::Other, err)))?;
        write!(lock, "{ch}")
    }
}

struct IO<'a, 'b> {
    inp_chars: utf8_chars::Chars<'a, StdinLock<'b>>,
    inp_fun: fn(&mut utf8_chars::Chars<'a, StdinLock<'b>>) -> io::Result<Option<VMInt>>,
    out_locks: &'a mut StdoutLock<'b>,
    out_fun: fn(&mut StdoutLock, VMInt) -> io::Result<()>,
}
impl<'a, 'b> IO<'a, 'b> {
    fn new(
        stdin: &'a mut StdinLock<'b>,
        in_format: IOFormat,
        stdout: &'a mut StdoutLock<'b>,
        out_format: IOFormat,
    ) -> Self {
        Self {
            inp_chars: stdin.chars(),
            inp_fun: match in_format {
                IOFormat::INTS => format_functions::inp_ints,
                IOFormat::BYTES => format_functions::inp_bytes,
            },
            out_locks: stdout,
            out_fun: match out_format {
                IOFormat::INTS => format_functions::out_ints,
                IOFormat::BYTES => format_functions::out_bytes,
            },
        }
    }

    fn input(&mut self) -> io::Result<Option<VMInt>> {
        self.out_locks.flush()?;
        (self.inp_fun)(&mut self.inp_chars)
    }
    fn output(&mut self, n: VMInt) -> io::Result<()> {
        (self.out_fun)(&mut self.out_locks, n)
    }
}

fn run(
    Run {
        executable,
        format,
        input,
        output,
        debug_rate,
        debug_args: debug,
    }: Run,
) -> anyhow::Result<()> {
    let content = {
        let mut buf = vec![];
        FileOrStdin::open(&executable)?.read_to_end(&mut buf)?;
        buf
    };
    let input_format = if let Some(input_format) = format {
        input_format
    } else {
        guess::format(Filetype::INTS, &content).context("Cannot guess format for the ints file")?
    };
    let code = match input_format {
        Format::Source => parsers::ints::source(&content)?,
        Format::Binary => parsers::ints::binary(&content)?,
        Format::Json => parsers::ints::json(&content)?,
        Format::Yaml => parsers::ints::yaml(&content)?,
    };

    let mut vm = vm::VM::new(code);
    let mut stdin = stdin().lock();
    let mut stdout = stdout().lock();

    let mut io = IO::new(&mut stdin, input, &mut stdout, output);
    if let Some(debug_rate) = debug_rate {
        let debug_rate = debug_rate.unwrap_or(NonZeroUsize::new(1).unwrap());
        loop {
            for _ in 0..debug_rate.get() {
                match vm.step().context("Runtime error")? {
                    vm::State::Stopped(vm::StopState::NeedInput(need_input)) => need_input.give(
                        io.input()
                            .context("Error during input")?
                            .context("Unexpected EOF")?,
                    ),
                    vm::State::Stopped(vm::StopState::HasOutput(has_output)) => {
                        io.output(has_output.get()).context("Error during output")?
                    }
                    vm::State::Stopped(vm::StopState::Halted) => return Ok(()),
                    vm::State::Running => (),
                }
            }
            debug::debug(stderr(), vm.memory(), &debug).context("Cannot write to stderr")?
        }
    } else {
        loop {
            match vm.run().context("Runtime error")? {
                vm::StopState::NeedInput(need_input) => need_input.give(
                    io.input()
                        .context("Error during input")?
                        .context("Unexpected EOF")?,
                ),
                vm::StopState::HasOutput(has_output) => {
                    io.output(has_output.get()).context("Error during output")?
                }
                vm::StopState::Halted => return Ok(()),
            }
        }
    }
}

mod debug {
    use std::io;
    use std::{fmt::Write, iter::repeat};

    use super::DebugArgs;
    use arrayvec::ArrayVec;
    use itertools::{Itertools, Position};

    pub(crate) fn debug(
        mut fout: impl io::Write,
        memory: &vm::Memory,
        DebugArgs {
            pc_window,
            rb_window,
        }: &DebugArgs,
    ) -> io::Result<()> {
        let pc = memory.pc();
        let rb = memory.rb();
        let memory = memory.memory();

        let pc_window = match pc_window {
            crate::DashOrT::Dash => 0..memory.len(),
            crate::DashOrT::T(l) => pc.saturating_sub(l.get())..(pc + l.get()).min(memory.len()),
        };
        let rb_window = match rb_window {
            crate::DashOrT::Dash => 0..memory.len(),
            crate::DashOrT::T(l) => {
                usize::try_from(rb - l.get() as isize).unwrap_or(0)
                    ..usize::try_from(rb + l.get() as isize)
                        .unwrap_or(0)
                        .min(memory.len())
            }
        };

        let mut ranges = ArrayVec::<_, 5>::new();
        if pc_window.start <= rb_window.end && rb_window.start <= pc_window.end {
            let common_window = usize::min(pc_window.start, rb_window.start)
                ..usize::max(pc_window.end, rb_window.end);
            if common_window.start > 0 {
                ranges.push((false, 0..common_window.start));
            }
            ranges.push((true, common_window.clone()));
            if common_window.end < memory.len() {
                ranges.push((false, common_window.end..memory.len()));
            }
        } else {
            let (w1, w2) = if pc_window.start < rb_window.start {
                (pc_window, rb_window)
            } else {
                (rb_window, pc_window)
            };
            debug_assert!(w1.end < w2.end);

            if w1.start > 0 {
                ranges.push((false, 0..w1.start));
            }
            ranges.push((true, w1.clone()));
            ranges.push((false, w1.end..w2.start));
            ranges.push((true, w2.clone()));
            if w2.end < memory.len() {
                ranges.push((false, w2.end..memory.len()));
            }
        }

        let rb = usize::try_from(rb).ok();

        // print memory

        let mut rb_marker = None;
        let mut pc_marker = None;
        let mut buf = String::with_capacity(
            ranges
                .iter()
                .map(|(show, range)| {
                    if *show {
                        range.len() * ("11101, 11, 11, 11".len() / 4)
                    } else {
                        " ... ".len()
                    }
                })
                .sum(),
        );
        for (show, range) in ranges {
            if !show {
                buf.push_str(" ... ");
            } else {
                for (position, i) in range.with_position() {
                    let start = buf.len();
                    write!(buf, "{}", memory[i]).unwrap();
                    let mut end = buf.len();

                    let is_pc = i == pc;
                    let is_rb = rb.is_some_and(|rb| i == rb);

                    if is_pc && is_rb && end - start == 1 {
                        buf.push(' ');
                        end += 1;
                    }
                    if is_pc {
                        pc_marker = Some(start..end)
                    }
                    if is_rb {
                        rb_marker = Some(start..end)
                    }

                    if let Position::First | Position::Middle = position {
                        write!(buf, ", ").unwrap();
                    }
                }
            }
        }
        buf.push('\n');
        fout.write_all(buf.as_bytes())?;
        buf.clear();

        // print pointers
        match (rb_marker, pc_marker) {
            (None, None) => (),
            (None, Some(pc)) => {
                for _ in 0..pc.start {
                    buf.push(' ')
                }
                for _ in pc {
                    buf.push('#')
                }
            }
            (Some(rb), None) => {
                for _ in 0..rb.start {
                    buf.push(' ')
                }
                for _ in rb {
                    buf.push('@')
                }
            }
            (Some(m1), Some(m2)) if m1 == m2 => {
                for _ in 0..m1.start {
                    buf.push(' ')
                }
                for ch in repeat(['#', '@']).flatten().take(m1.len()) {
                    buf.push(ch)
                }
            }
            (Some(rb), Some(pc)) if rb.start < pc.start => {
                for _ in 0..rb.start {
                    buf.push(' ')
                }
                for _ in rb.clone() {
                    buf.push('@')
                }
                for _ in rb.end..pc.start {
                    buf.push(' ')
                }
                for _ in pc {
                    buf.push('#')
                }
            }
            (Some(rb), Some(pc)) => {
                for _ in 0..pc.start {
                    buf.push(' ')
                }
                for _ in pc.clone() {
                    buf.push('#')
                }
                for _ in pc.end..rb.start {
                    buf.push(' ')
                }
                for _ in rb {
                    buf.push('@')
                }
            }
        }
        buf.push('\n');
        fout.write_all(buf.as_bytes())?;
        Ok(())
    }
}

#[derive(Debug, Clone, Parser)]
#[command(author, version, about, long_about = None)]
pub enum Cli {
    /// Assemble an assembly source file
    Assemble(Assemble),
    /// Link multiple object files in a single executable
    Link(Link),
    /// Run the given intcode file
    Run(Run),
}

pub fn cli(cli: Cli) -> anyhow::Result<()> {
    match cli {
        Cli::Assemble(cli) => assemble(cli),
        Cli::Link(cli) => link(cli),
        Cli::Run(cli) => run(cli),
    }
}
