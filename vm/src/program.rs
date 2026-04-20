use std::{borrow::Cow, io, str::Utf8Error};

use derive_more::IsVariant;
use itertools::Itertools;
use lazy_regex::regex_captures_iter;
use num::BigInt;
use serde::{Deserialize, Serialize};
use snafu::{ResultExt, Snafu};
use zicc_frontmatter::FrontMatter;

use crate::stream::Format as StreamFormat;

pub struct Program {
    pub info: ProgramInfo,
    pub content: Vec<BigInt>,
}

#[derive(Debug, Clone, Serialize, Deserialize, Default, PartialEq, Eq)]
pub struct ProgramInfo {
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub name: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub author: Option<String>,
    #[serde(default, skip_serializing_if = "StreamFormat::is_ints")]
    pub input: StreamFormat,
    #[serde(default, skip_serializing_if = "StreamFormat::is_ints")]
    pub output: StreamFormat,
    #[serde(default, skip_serializing_if = "Format::is_ascii")]
    pub format: Format,
    #[serde(default, skip_serializing_if = "Compression::is_none")]
    pub compression: Compression,
}

/// Storage format of the program
#[derive(Debug, Clone, Copy, Serialize, Deserialize, Default, IsVariant, PartialEq, Eq)]
pub enum Format {
    /// IntCode default comma separated list
    ///
    /// This format is even more lenient, effectively ignoring anything that's
    /// not a number
    #[default]
    Ascii,
}

/// Additional compression to apply
#[derive(Debug, Clone, Copy, Serialize, Deserialize, Default, IsVariant, PartialEq, Eq)]
pub enum Compression {
    /// No compression
    #[default]
    None,
}

impl Program {
    /// Parse a source program
    pub fn parse(source: &[u8]) -> Result<Self, ParseError> {
        let FrontMatter {
            frontmatter:
                info @ ProgramInfo {
                    format,
                    compression,
                    ..
                },
            content,
            ..
        } = FrontMatter::parse_optional(source)
            .context(InvalidInfoSnafu)?
            .unwrap_or_default();

        let content = match compression {
            Compression::None => Cow::Borrowed(content),
        };

        let content = match format {
            Format::Ascii => regex_captures_iter!(r"(\+|\-)?\d+", str::from_utf8(&content)?)
                .map(|c| c.get_match().as_str().parse().unwrap())
                .collect(),
        };

        Ok(Self { info, content })
    }

    /// Dump the program to a writer
    pub fn dump(&self, mut dest: impl io::Write) -> io::Result<()> {
        zicc_frontmatter::write_optional(
            (self.info != Default::default()).then_some(&self.info),
            &mut dest,
        )?;

        let mut dest = match self.info.compression {
            Compression::None => dest,
        };

        match self.info.format {
            Format::Ascii => {
                write!(dest, "{}", self.content.iter().format(", "))
            }
        }
    }
}

#[derive(Debug, Snafu)]
pub enum ParseError {
    #[snafu(display("Invalid program info"))]
    InvalidInfo { source: zicc_frontmatter::Error },

    #[snafu(transparent)]
    NotUtf8 { source: Utf8Error },
}
