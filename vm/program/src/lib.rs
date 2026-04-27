use std::{
    borrow::{Borrow, Cow},
    collections::BTreeMap,
    io,
    str::{FromStr, Utf8Error},
};

use derive_more::IsVariant;
use itertools::Itertools;
use serde::{Deserialize, Serialize};
use snafu::{ResultExt, Snafu};
use toml::Table;
use zicc_frontmatter::FrontMatter;
use zicc_intcode::OpCode;
use zicc_limits::{ParseValueError, Value};
use zicc_vm_stream::std_io::Format as StreamFormat;

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct Program {
    pub info: ProgramInfo,
    pub content: Vec<Value>,
}

#[derive(Debug, Clone, Serialize, Deserialize, Default, PartialEq)]
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

    /// Free form metadata
    #[serde(default, skip_serializing_if = "BTreeMap::is_empty")]
    pub metadata: BTreeMap<String, Table>,
}

/// Storage format of the program
#[derive(Debug, Clone, Copy, Serialize, Deserialize, Default, IsVariant, PartialEq, Eq, Hash)]
pub enum Format {
    /// IntCode default comma separated list
    ///
    /// This format is even more lenient, effectively ignoring anything that's
    /// not a number
    #[default]
    Ascii,
}

/// Additional compression to apply
#[derive(Debug, Clone, Copy, Serialize, Deserialize, Default, IsVariant, PartialEq, Eq, Hash)]
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
            Format::Ascii => str::from_utf8(&content)?
                .split(',')
                .map(|v| Value::from_str(v.trim()))
                .collect::<Result<_, _>>()?,
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

    #[must_use]
    pub fn len(&self) -> usize {
        self.content.len()
    }

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

impl ProgramInfo {
    /// Extract a given metadata table from the metadata section
    pub fn get_metadata<'de, T, K>(&self, key: &K) -> Result<T, toml::de::Error>
    where
        String: Borrow<K>,
        K: Ord + ?Sized,
        T: Deserialize<'de>,
    {
        self.metadata
            .get(key)
            .cloned()
            .unwrap_or_default()
            .try_into()
    }

    /// Set a given metadata table
    pub fn set_metadata<T, K>(&mut self, key: K, value: T) -> Result<(), toml::ser::Error>
    where
        K: ToString,
        T: Serialize,
    {
        self.metadata
            .insert(key.to_string(), Table::try_from(value)?);
        Ok(())
    }
}

impl Default for Program {
    fn default() -> Self {
        // Simple program that halts immediately
        Self {
            info: Default::default(),
            content: vec![OpCode::HLT.to_u8().into()],
        }
    }
}

#[derive(Debug, Snafu)]
pub enum ParseError {
    #[snafu(display("Invalid program info"))]
    InvalidInfo { source: zicc_frontmatter::Error },

    #[snafu(transparent)]
    NotUtf8 { source: Utf8Error },

    #[snafu(transparent)]
    InvalidValue { source: ParseValueError },
}
