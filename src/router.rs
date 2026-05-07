use std::cell::RefCell;
use std::path::{Path, PathBuf};
use std::str::FromStr;

use clap_stdin::FileOrStdout;
use snafu::Snafu;
use string_interner::DefaultStringInterner;

pub mod assemble;
pub mod classify;
pub mod compile;
pub mod link;
pub mod run;

#[derive(Debug, Snafu)]
pub enum Error {
    #[snafu(transparent)]
    Classify { source: classify::ClassifyError },
    #[snafu(transparent)]
    Compile { source: compile::CompilePhaseError },
    #[snafu(transparent)]
    Assemble {
        source: assemble::AssemblePhaseError,
    },
    #[snafu(transparent)]
    Link { source: link::LinkPhaseError },
    #[snafu(transparent)]
    Run { source: run::RunError },
}

fn find_kinds<'p>(
    inputs: &'p [PathBuf],
    kinds: &[classify::InputKind],
    kind: classify::InputKind,
) -> Vec<&'p Path> {
    inputs
        .iter()
        .zip(kinds.iter())
        .filter_map(|(p, k)| (*k == kind).then_some(&**p))
        .collect()
}

/// Route artifacts through the toolchain
///
/// Will autodetect what to do based on the file extension.
///
/// This entry point is simple on purpose, as the user is expected to use the
/// more specific functions for custom compilation steps.
pub fn route(
    inputs: Vec<PathBuf>,
    output: Option<FileOrStdout>,
    link_only: bool,
) -> Result<(), Error> {
    let effective_output = if link_only {
        Some(output.unwrap_or_else(|| FileOrStdout::from_str("-").unwrap()))
    } else {
        output
    };

    let (kinds, target) = classify::classify(&inputs, &effective_output, link_only)?;

    if kinds.iter().all(|k| *k == classify::InputKind::Executable)
        && target == classify::Stage::Run
    {
        return Ok(run::run_file(&inputs[0])?);
    }

    let interner = RefCell::new(DefaultStringInterner::new());

    let ic_files = find_kinds(&inputs, &kinds, classify::InputKind::Source);
    let compiled_asm = compile::compile(&ic_files, &interner)?;

    let ica_files = find_kinds(&inputs, &kinds, classify::InputKind::AsmSource);
    let assembled = assemble::assemble(&ica_files, compiled_asm, &mut interner.borrow_mut())?;

    let icob_files = find_kinds(&inputs, &kinds, classify::InputKind::Object);
    let linked = link::load_and_link(&icob_files, assembled, &mut interner.borrow_mut())?;

    match target {
        classify::Stage::Link => {
            run::write_object(
                &linked,
                effective_output
                    .as_ref()
                    .expect("output should be set at Link stage")
                    .clone(),
                &interner.borrow(),
            )?;
        }
        classify::Stage::MakeExecutable | classify::Stage::Run => {
            let executable = link::make_exec(linked, &mut interner.borrow_mut())?;
            if target == classify::Stage::MakeExecutable {
                run::write_executable(
                    &executable,
                    effective_output
                        .as_ref()
                        .expect("output should be set at MakeExecutable stage")
                        .clone(),
                )?;
            } else {
                run::run(executable)?;
            }
        }
    }

    Ok(())
}
