#![doc = include_str!("../README.md")]

use std::{io, ops::Range};

use lazy_regex::bytes_regex_captures;

pub type Error = toml::de::Error;

/// A parsed source file consisting of an optional TOML frontmatter block and the remaining content.
///
/// Byte offsets are retained so that spans reported by the TOML parser or by content parsers can
/// be mapped back to positions in the original source with `map_frontmatter_span` /
/// `map_content_span`.
pub struct FrontMatter<'s, F> {
    /// Byte offset of the first frontmatter line in the original source.
    frontmatter_start: usize,
    /// Deserialized frontmatter value.
    pub frontmatter: F,
    /// Byte offset of the first content byte in the original source (right after the closing `+++`).
    content_start: usize,
    /// The slice of the original source that follows the frontmatter block.
    pub content: &'s [u8],
}

impl<'s, F> FrontMatter<'s, Option<F>> {
    /// Parse a source that may or may not contain a `+++`-delimited TOML frontmatter block.
    ///
    /// Returns `frontmatter: None` (and `content` equal to the full source) when no frontmatter
    /// fence is found.  Any leading blank lines before the opening `+++` are silently skipped.
    pub fn parse_optional(source: &'s [u8]) -> Result<Self, Error>
    where
        F: serde::Deserialize<'s>,
    {
        let Some((full_front, before_front, frontmatter)) = bytes_regex_captures!(
            r#"^(?<before_front>(?:[^\S\n]*\n)*\+\+\+[^\S\n]*\n)(?<frontmatter>(?:(?:|\+|\+\+|(?:[^+\n]|\+[^+\n]|\+\+[^+\n]).*|\+\+\+[^\S\n]*\S.*)\n)*)\+\+\+[^\S\n]*\n"#,
            source
        ) else {
            // No frontmatter
            return Ok(Self {
                frontmatter_start: 0,
                frontmatter: None,
                content_start: 0,
                content: source,
            });
        };

        let frontmatter_start = before_front.len();
        let content_start = full_front.len();

        let frontmatter = toml::from_slice(frontmatter)?;

        Ok(Self {
            frontmatter_start,
            frontmatter: Some(frontmatter),
            content_start,
            content: &source[content_start..],
        })
    }

    /// Convert `FrontMatter<Option<F>>` into `FrontMatter<F>`, substituting `F::default()` when
    /// no frontmatter was present.
    pub fn unwrap_or_default(self) -> FrontMatter<'s, F>
    where
        F: Default,
    {
        let Self {
            frontmatter_start,
            frontmatter,
            content_start,
            content,
        } = self;
        FrontMatter {
            frontmatter_start,
            frontmatter: frontmatter.unwrap_or_default(),
            content_start,
            content,
        }
    }
}
impl<'a, F> FrontMatter<'a, F> {
    /// Translate a span that is relative to the frontmatter body into a span relative to the
    /// original source.
    pub fn map_frontmatter_span(&self, span: Range<usize>) -> Range<usize> {
        (span.start + self.frontmatter_start)..(span.end + self.frontmatter_start)
    }
    /// Translate a span that is relative to `self.content` into a span relative to the original
    /// source.
    pub fn map_content_span(&self, span: Range<usize>) -> Range<usize> {
        (span.start + self.content_start)..(span.end + self.content_start)
    }
}

/// Serialize `front` as TOML wrapped in `+++` fences.
///
/// Panics if `front` cannot be serialized to TOML (this should never happen for well-formed
/// frontmatter types).
pub fn write<F>(front: &F, mut dest: impl io::Write) -> io::Result<()>
where
    F: serde::Serialize,
{
    let mut front = toml::to_string(front).expect("Frontmatter serialization should be infallible");
    if !front.ends_with('\n') {
        front.push('\n');
    }

    dest.write_all(b"+++\n")?;
    dest.write_all(front.as_bytes())?;
    dest.write_all(b"+++\n")?;

    Ok(())
}

/// Like [`write`], but writes only when `front` is not `None` (no fences emitted).
pub fn write_optional<F>(front: Option<&F>, dest: impl io::Write) -> io::Result<()>
where
    F: serde::Serialize,
{
    if let Some(front) = front {
        write(front, dest)
    } else {
        Ok(())
    }
}
