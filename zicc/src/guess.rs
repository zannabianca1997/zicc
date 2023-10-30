//! Function to guess parameters based to file extensions and content

use std::ffi::OsStr;

use anyhow::bail;
use uncased::AsUncased;

use crate::{path_or_dash::PathOrDash, Filetype, Format, MAGIC};

pub(crate) fn type_and_format(
    input_path: &PathOrDash,
    input_content: &[u8],
) -> anyhow::Result<(Filetype, Format)> {
    let r#type = r#type(input_path, input_content)?;
    let format = format(r#type, input_content)?;
    Ok((r#type, format))
}

pub(crate) fn r#type(input_path: &PathOrDash, input_content: &[u8]) -> anyhow::Result<Filetype> {
    if let PathOrDash::Path(path) = input_path {
        if let Some(ext) = path.extension().and_then(OsStr::to_str) {
            let ext = ext.as_uncased();
            if ext == "icc" {
                return Ok(Filetype::ICC);
            } else if ext == "ica" {
                return Ok(Filetype::ICA);
            } else if ext == "ico" {
                return Ok(Filetype::ICO);
            } else if ext == "ints" {
                return Ok(Filetype::INTS);
            }
        }
    };

    if input_content.starts_with(&MAGIC) {
        match input_content.get(MAGIC.len()..(MAGIC.len() + 3)) {
            Some(b"ICC") => return Ok(Filetype::ICC),
            Some(b"ICA") => return Ok(Filetype::ICA),
            Some(b"ICO") => return Ok(Filetype::ICO),
            Some(b"INT") => return Ok(Filetype::INTS),
            _ => bail!("Invalid file type after magic number"),
        }
    };

    bail!("Cannot guess file type")
}

pub(crate) fn format(r#type: Filetype, input_content: &[u8]) -> anyhow::Result<Format> {
    if input_content.starts_with(&MAGIC) {
        // the other formats are all utf-8, while MAGIC is invalid utf-8
        Ok(Format::Binary)
    } else {
        let maybe_json = input_content
            .iter()
            .copied()
            .take(100)
            .take_while(|b| b.is_ascii())
            .skip_while(|b| b.is_ascii_whitespace())
            .next()
            .map(|f| f == b'{')
            .unwrap_or(false);
        match r#type {
            Filetype::ICC => Ok(Format::Source),
            Filetype::ICA => Ok(if maybe_json {
                Format::Json
            } else {
                Format::Source
            }),
            Filetype::ICO => Ok(if maybe_json {
                Format::Json
            } else {
                Format::Yaml
            }),
            Filetype::INTS => {
                if maybe_json {
                    bail!("No format for INTS files starts with a '{{'")
                } else {
                    Ok(Format::Source)
                }
            }
        }
    }
}
