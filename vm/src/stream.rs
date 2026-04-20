//! I/O streams formats

use std::io;

use derive_more::{Display, FromStr};
use num::BigInt;
use serde::{Deserialize, Serialize};
use snafu::Snafu;

/// Format of an I/O stream
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, Hash, Default, Serialize, Deserialize, FromStr, Display,
)]
pub enum Format {
    /// Comma separated integers
    #[default]
    Ints,

    /// Octet streams
    Bytes,

    /// Unicode codepoints
    Unicode,
}

/// Adapter to write an intcode stream to an [`io::Write`] object
#[derive(Debug, Clone, Copy)]
pub struct Writer<W> {
    /// The format to serialise values in.
    format: Format,
    /// The underlying sink.
    inner: W,
    /// Whether at least one value has already been written; used to insert the `", "` separator between integers.
    written_before: bool,
}
impl<W> Writer<W> {
    /// Create a new writer that serialises values in `format` into `inner`.
    pub fn new(format: Format, inner: W) -> Self {
        Self {
            format,
            inner,
            written_before: false,
        }
    }
}

impl<W> Writer<W>
where
    W: io::Write,
{
    /// Encode `value` and append it to the underlying sink.
    ///
    /// Returns `Error::InvalidByte` when `format` is `Bytes` and the value does
    /// not fit in a `u8`, or when `format` is `Unicode` and the value is not a
    /// valid Unicode scalar.
    pub fn write(&mut self, value: BigInt) -> Result<()> {
        match self.format {
            Format::Ints => {
                if self.written_before {
                    self.inner.write_all(b", ")?;
                }
                write!(self.inner, "{value}")?;
            }
            Format::Bytes => {
                let byte = u8::try_from(value).map_err(|err| Error::InvalidByte {
                    original: err.into_original(),
                })?;
                self.inner.write_all(&[byte])?
            }
            Format::Unicode => {
                let ch = u32::try_from(&value)
                    .ok()
                    .and_then(char::from_u32)
                    .ok_or_else(|| Error::InvalidByte { original: value })?;
                self.inner
                    .write_all(ch.encode_utf8(&mut [0; 4]).as_bytes())?;
            }
        }
        self.written_before = true;
        Ok(())
    }
}

/// Adapter to read an intcode stream from an [`io::Read`] object
#[derive(Debug, Clone, Copy)]
pub struct Reader<R> {
    /// The format to parse values from.
    format: Format,
    /// The underlying source.
    inner: R,
    /// One-byte pushback buffer used by [`Self::read_int`] when it reads one byte past the end of an integer.
    pending: Option<u8>,
}
impl<R> Reader<R> {
    /// Create a new reader that parses values in `format` from `inner`.
    pub fn new(format: Format, inner: R) -> Self {
        Self {
            format,
            inner,
            pending: None,
        }
    }
}

impl<R> Reader<R>
where
    R: io::Read,
{
    /// Read the next value from the stream. Returns `Ok(None)` on clean EOF.
    pub fn read(&mut self) -> Result<Option<BigInt>> {
        match self.format {
            Format::Ints => self.read_int(),
            Format::Bytes => self.read_byte(),
            Format::Unicode => self.read_char(),
        }
    }

    /// Return the pending pushback byte if present, otherwise read one byte from `inner`; `Ok(None)` on clean EOF.
    fn take_byte(&mut self) -> io::Result<Option<u8>> {
        if let Some(b) = self.pending.take() {
            return Ok(Some(b));
        }
        let mut buf = [0u8; 1];
        match self.inner.read(&mut buf)? {
            0 => Ok(None),
            _ => Ok(Some(buf[0])),
        }
    }

    /// Parse the next signed decimal integer, skipping non-numeric noise between values.
    fn read_int(&mut self) -> Result<Option<BigInt>> {
        let mut accum: Vec<u8> = Vec::new();
        let mut has_digit = false;

        loop {
            let Some(b) = self.take_byte()? else { break };
            match b {
                b'0'..=b'9' => {
                    accum.push(b);
                    has_digit = true;
                }
                b'+' | b'-' if !has_digit => {
                    accum.clear();
                    accum.push(b);
                }
                _ => {
                    if has_digit {
                        self.pending = Some(b);
                        break;
                    } else {
                        accum.clear();
                    }
                }
            }
        }

        if has_digit {
            let s = std::str::from_utf8(&accum).expect("ASCII sign and digits are valid utf-8");
            Ok(Some(s.parse().expect("sign + digits parses as BigInt")))
        } else {
            Ok(None)
        }
    }

    /// Read a single raw byte and return its numeric value.
    fn read_byte(&mut self) -> Result<Option<BigInt>> {
        Ok(self.take_byte()?.map(BigInt::from))
    }

    /// Decode one UTF-8 scalar value and return its codepoint.
    fn read_char(&mut self) -> Result<Option<BigInt>> {
        let Some(first) = self.take_byte()? else {
            return Ok(None);
        };
        let len = match first {
            0x00..=0x7f => 1,
            0xc0..=0xdf => 2,
            0xe0..=0xef => 3,
            0xf0..=0xf7 => 4,
            _ => return Err(Error::InvalidUtf8),
        };
        let mut buf = [0u8; 4];
        buf[0] = first;
        self.inner.read_exact(&mut buf[1..len]).map_err(|e| {
            if e.kind() == io::ErrorKind::UnexpectedEof {
                Error::InvalidUtf8
            } else {
                Error::Io { source: e }
            }
        })?;
        let s = std::str::from_utf8(&buf[..len]).map_err(|_| Error::InvalidUtf8)?;
        let ch = s.chars().next().expect("non-empty valid utf-8 has a char");
        Ok(Some(BigInt::from(ch as u32)))
    }
}

/// Iterate over decoded values, ending cleanly on EOF.
impl<R: io::Read> Iterator for Reader<R> {
    type Item = Result<BigInt>;

    fn next(&mut self) -> Option<Self::Item> {
        self.read().transpose()
    }
}

/// Errors that can arise when reading or writing an intcode stream.
#[derive(Debug, Snafu)]
pub enum Error {
    /// An underlying I/O error.
    #[snafu(transparent)]
    Io { source: io::Error },
    /// The value cannot be represented in the current stream format.
    #[snafu(display("{original} is not a valid byte"))]
    InvalidByte { original: BigInt },
    /// The byte sequence is not valid UTF-8.
    #[snafu(display("invalid utf-8 sequence in stream"))]
    InvalidUtf8,
}

/// Convenience alias for results from this module.
pub type Result<T> = std::result::Result<T, Error>;

#[cfg(test)]
mod tests {
    use num::BigInt;

    use super::{Format, Reader, Writer};

    fn collect_ints(input: &[u8]) -> Vec<BigInt> {
        Reader::new(Format::Ints, input)
            .map(|r| r.unwrap())
            .collect()
    }

    fn nums(xs: &[i64]) -> Vec<BigInt> {
        xs.iter().copied().map(BigInt::from).collect()
    }

    fn write_all(format: Format, values: &[BigInt]) -> Result<Vec<u8>, super::Error> {
        let mut buf = Vec::new();
        let mut w = Writer::new(format, &mut buf);
        for v in values {
            w.write(v.clone())?;
        }
        Ok(buf)
    }

    // ── Reader tests ────────────────────────────────────────────────────────

    /// Comma-separated integers round-trip cleanly
    #[test]
    fn reader_should_parse_comma_separated_ints() {
        assert_eq!(collect_ints(b"1, 2, 3"), nums(&[1, 2, 3]));
    }

    /// Non-numeric noise between integers is silently ignored
    #[test]
    fn reader_should_skip_noise_between_ints() {
        assert_eq!(collect_ints(b"foo 12 bar -34 baz"), nums(&[12, -34]));
    }

    /// Adjacent sign-then-digits are parsed as a signed integer
    #[test]
    fn reader_should_handle_adjacent_signed_ints() {
        assert_eq!(collect_ints(b"123-456"), nums(&[123, -456]));
    }

    /// A leading plus sign is accepted
    #[test]
    fn reader_should_accept_plus_prefix() {
        assert_eq!(collect_ints(b"+7"), nums(&[7]));
    }

    /// A bare sign at end of stream is silently dropped
    #[test]
    fn reader_should_drop_trailing_bare_sign() {
        assert_eq!(collect_ints(b"5 +"), nums(&[5]));
    }

    /// Only the last sign before digits counts
    #[test]
    fn reader_should_collapse_repeated_signs() {
        assert_eq!(collect_ints(b"++-9"), nums(&[-9]));
    }

    /// Clean EOF with no data returns None
    #[test]
    fn reader_should_return_none_on_empty_int_stream() {
        let mut r = Reader::new(Format::Ints, b"".as_ref());
        assert!(r.read().unwrap().is_none());
    }

    /// Bytes are read one byte at a time as their integer value
    #[test]
    fn reader_should_read_bytes() {
        let bytes: &[u8] = &[0, 255, 42];
        assert_eq!(
            Reader::new(Format::Bytes, bytes)
                .map(|r| r.unwrap())
                .collect::<Vec<_>>(),
            nums(&[0, 255, 42])
        );
    }

    /// Unicode codepoints are decoded and returned as their scalar value
    #[test]
    fn reader_should_read_unicode_codepoints() {
        let input = "a£€🦀".as_bytes();
        let got: Vec<BigInt> = Reader::new(Format::Unicode, input)
            .map(|r| r.unwrap())
            .collect();
        assert_eq!(got, nums(&[0x61, 0xA3, 0x20AC, 0x1F980]));
    }

    /// An invalid UTF-8 leading byte produces an error
    #[test]
    fn reader_should_error_on_invalid_utf8() {
        let mut r = Reader::new(Format::Unicode, [0xFFu8].as_ref());
        assert!(matches!(r.read(), Err(super::Error::InvalidUtf8)));
    }

    /// A truncated multi-byte sequence produces an error
    #[test]
    fn reader_should_error_on_truncated_utf8() {
        let mut r = Reader::new(Format::Unicode, [0xE2u8, 0x82].as_ref());
        assert!(matches!(r.read(), Err(super::Error::InvalidUtf8)));
    }

    /// Reader works as an iterator over Result<BigInt>
    #[test]
    fn reader_should_iterate() {
        let values: Vec<BigInt> = Reader::new(Format::Ints, b"10 20 30".as_ref())
            .map(|r| r.unwrap())
            .collect();
        assert_eq!(values, nums(&[10, 20, 30]));
    }

    // ── Writer tests ─────────────────────────────────────────────────────────

    /// Multiple integers are written as comma-separated values
    #[test]
    fn writer_should_write_comma_separated_ints() {
        let out = write_all(Format::Ints, &nums(&[1, 2, 3])).unwrap();
        assert_eq!(out, b"1, 2, 3");
    }

    /// A single integer is written without any separator
    #[test]
    fn writer_should_write_single_int_without_separator() {
        let out = write_all(Format::Ints, &nums(&[42])).unwrap();
        assert_eq!(out, b"42");
    }

    /// Writing no values produces an empty buffer
    #[test]
    fn writer_should_write_nothing_for_empty_int_sequence() {
        let out = write_all(Format::Ints, &[]).unwrap();
        assert!(out.is_empty());
    }

    /// Byte values are written as raw bytes
    #[test]
    fn writer_should_write_raw_bytes() {
        let out = write_all(Format::Bytes, &nums(&[0, 255, 42])).unwrap();
        assert_eq!(out, &[0u8, 255, 42]);
    }

    /// A value outside 0..=255 yields InvalidByte in Bytes mode
    #[test]
    fn writer_should_error_on_out_of_range_byte() {
        assert!(matches!(
            write_all(Format::Bytes, &nums(&[-1])),
            Err(super::Error::InvalidByte { .. })
        ));
        assert!(matches!(
            write_all(Format::Bytes, &nums(&[256])),
            Err(super::Error::InvalidByte { .. })
        ));
    }

    /// Unicode codepoints are encoded as UTF-8
    #[test]
    fn writer_should_write_utf8_for_unicode_codepoints() {
        let codepoints = nums(&[0x61, 0xA3, 0x20AC, 0x1F980]);
        let out = write_all(Format::Unicode, &codepoints).unwrap();
        assert_eq!(out, "a£€🦀".as_bytes());
    }

    /// A surrogate or out-of-range codepoint yields InvalidByte in Unicode mode
    #[test]
    fn writer_should_error_on_invalid_unicode_scalar() {
        assert!(matches!(
            write_all(Format::Unicode, &nums(&[0xD800])),
            Err(super::Error::InvalidByte { .. })
        ));
        assert!(matches!(
            write_all(Format::Unicode, &nums(&[0x11_0000])),
            Err(super::Error::InvalidByte { .. })
        ));
    }

    /// Values written as Ints can be read back by Reader
    #[test]
    fn writer_then_reader_should_roundtrip_ints() {
        let original = nums(&[1, -2, 3]);
        let bytes = write_all(Format::Ints, &original).unwrap();
        let recovered: Vec<BigInt> = Reader::new(Format::Ints, bytes.as_slice())
            .map(|r| r.unwrap())
            .collect();
        assert_eq!(recovered, original);
    }
}
