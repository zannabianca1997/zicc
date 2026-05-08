use std::{
    ffi::{OsStr, OsString},
    path::{Path, PathBuf},
};

use clap_stdin::FileOrStdout;
use snafu::Snafu;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Stage {
    Assembly,
    Link,
    MakeExecutable,
    Run,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum InputKind {
    Source,
    AsmSource,
    Object,
    Executable,
}

#[derive(Debug, Snafu)]
pub enum ClassifyError {
    #[snafu(display("Unknown file extension '{}'", ext.display()))]
    UnknownExtension { ext: OsString },
    #[snafu(display("Unknown or unsupported output extension '{}'", ext.display()))]
    UnknownOutputExtension { ext: OsString },
    #[snafu(display("Cannot mix `.ints` executables with other input types"))]
    CannotMixExecutable,
    #[snafu(display("Cannot produce output for an already-linked `.ints` executable"))]
    CannotRecompileExecutable,
    #[snafu(display(
        "Cannot emit `.ica` assembly from multiple inputs — linking would be required"
    ))]
    MultipleAssemblyInputs,
    #[snafu(display("Cannot emit `.ica` assembly from a `.icob` object file"))]
    CannotAssembleObject,
    #[snafu(display("Can only run a single `.ints` executable"))]
    MultipleExecutables,
}

fn classify_path(path: &Path) -> Result<InputKind, ClassifyError> {
    let ext = path
        .extension()
        .ok_or_else(|| ClassifyError::UnknownExtension {
            ext: path.file_name().unwrap_or(path.as_os_str()).to_owned(),
        })?;

    match ext.to_str() {
        Some("ic") => Ok(InputKind::Source),
        Some("ica") => Ok(InputKind::AsmSource),
        Some("icob") => Ok(InputKind::Object),
        Some("ints") => Ok(InputKind::Executable),
        _ => Err(ClassifyError::UnknownExtension {
            ext: ext.to_owned(),
        }),
    }
}

fn target_from_ext(ext: &OsStr) -> Result<Stage, ClassifyError> {
    match ext.to_str() {
        Some("ica") => Ok(Stage::Assembly),
        Some("icob") => Ok(Stage::Link),
        Some("ints") => Ok(Stage::MakeExecutable),
        _ => Err(ClassifyError::UnknownOutputExtension {
            ext: ext.to_owned(),
        }),
    }
}

pub fn classify(
    inputs: &[PathBuf],
    output: &Option<FileOrStdout>,
    link_only: bool,
) -> Result<(Vec<InputKind>, Stage), ClassifyError> {
    let kinds: Vec<InputKind> = inputs
        .iter()
        .map(|p| classify_path(p))
        .collect::<Result<Vec<_>, _>>()?;

    let target = if link_only {
        Stage::Link
    } else {
        match output {
            None => Stage::Run,
            Some(o) if o.is_stdout() => Stage::MakeExecutable,
            Some(o) => {
                let path = Path::new(o.filename());
                let ext =
                    path.extension()
                        .ok_or_else(|| ClassifyError::UnknownOutputExtension {
                            ext: path.file_name().unwrap_or(path.as_os_str()).to_owned(),
                        })?;
                target_from_ext(ext)?
            }
        }
    };

    let has_executable = kinds.contains(&InputKind::Executable);
    let has_other = kinds.iter().any(|k| *k != InputKind::Executable);

    if has_executable && has_other {
        return Err(ClassifyError::CannotMixExecutable);
    }

    if has_executable && target != Stage::Run {
        return Err(ClassifyError::CannotRecompileExecutable);
    }

    if has_executable && kinds.len() > 1 {
        return Err(ClassifyError::MultipleExecutables);
    }

    if target == Stage::Assembly {
        if kinds.len() != 1 {
            return Err(ClassifyError::MultipleAssemblyInputs);
        }
        if kinds[0] == InputKind::Object {
            return Err(ClassifyError::CannotAssembleObject);
        }
    }

    Ok((kinds, target))
}
