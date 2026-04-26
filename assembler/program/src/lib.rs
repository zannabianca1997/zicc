use std::{
    borrow::{Borrow, Cow},
    collections::BTreeMap,
    io,
    str::Utf8Error,
};

use derive_more::IsVariant;
use serde::{Deserialize, Serialize};
use snafu::{OptionExt, ResultExt, Snafu};
use string_interner::DefaultStringInterner;
use toml::Table;
use zicc_assembler_ast::File;
use zicc_assembler_parser::{ParserError, parse};
use zicc_display::DisplayWith;
use zicc_frontmatter::FrontMatter;

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Program {
    pub info: ProgramInfo,
    pub content: File,
}

#[derive(Debug, Clone, Serialize, Deserialize, Default, PartialEq)]
pub struct ProgramInfo {
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub name: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub author: Option<String>,
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
    /// Intcode assembly
    ///
    /// Plain text assembly
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
    pub fn parse<'s>(
        source: &'s [u8],
        interner: &mut DefaultStringInterner,
        mut error_handler: impl FnMut(ParserError<'_>),
    ) -> Result<Self, ParseError> {
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
            Format::Ascii => {
                let content = str::from_utf8(&content)?;
                let (content, errors) = parse(content, interner).into_output_errors();
                for error in errors {
                    error_handler(error)
                }
                content.context(ParsingFailedSnafu)?
            }
        };

        Ok(Self { info, content })
    }

    /// Dump the program to a writer
    pub fn dump(
        &self,
        mut dest: impl io::Write,
        interner: &DefaultStringInterner,
    ) -> io::Result<()> {
        zicc_frontmatter::write_optional(
            (self.info != Default::default()).then_some(&self.info),
            &mut dest,
        )?;

        let mut dest = match self.info.compression {
            Compression::None => dest,
        };

        match self.info.format {
            Format::Ascii => write!(dest, "{}", self.content.display(interner)),
        }
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

#[derive(Debug, Snafu)]
pub enum ParseError {
    #[snafu(display("Invalid program info"))]
    InvalidInfo {
        source: zicc_frontmatter::Error,
    },

    #[snafu(transparent)]
    NotUtf8 {
        source: Utf8Error,
    },

    ParsingFailed,
}
