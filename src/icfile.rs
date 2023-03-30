//! Write and parse intcode programs

use arrayvec::ArrayString;
use byteorder::{ReadBytesExt, WriteBytesExt};
use either::Either::{Left, Right};
use std::{
    convert::identity,
    io::{self, Read, Write},
    iter::{once, repeat},
    mem,
    num::ParseIntError,
    str::FromStr,
};
use strum_macros::EnumDiscriminants;
use thiserror::Error;

use crate::{ICProgram, ICValue};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[cfg_attr(test, derive(strum_macros::EnumIter))]
pub enum ByteOrder {
    BigEndian,
    LittleEndian,
    NativeEndian,
    NetworkEndian,
}
impl const Default for ByteOrder {
    fn default() -> Self {
        Self::LittleEndian
    }
}
#[derive(Debug, Error)]
#[error("{0:?} is not a valid byteorder")]
pub struct ParseByteOrderFromStringError(String);
impl FromStr for ByteOrder {
    type Err = ParseByteOrderFromStringError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let s = s.to_lowercase();
        match s.as_ref() {
            "little" | "littleendian" | "little_endian" | "little-endian" | "le" | "l" => {
                Ok(Self::LittleEndian)
            }
            "big" | "bigendian" | "big_endian" | "big-endian" | "be" | "b" => Ok(Self::BigEndian),
            "native" | "nativeendian" | "native_endian" | "native-endian" | "ne" | "n" => {
                Ok(Self::NativeEndian)
            }
            "network" | "networkendian" | "network_endian" | "network-endian" | "net" => {
                Ok(Self::NetworkEndian)
            }
            _ => Err(ParseByteOrderFromStringError(s)),
        }
    }
}

/// Format of a intcode file
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ICFormat {
    Ascii { sep: u8 },
    Binary { endianness: ByteOrder },
}

#[derive(Debug, Error, PartialEq, Eq)]
pub enum NewAsciiFormatError {
    #[error("Separator {0:?} is not an ascii character")]
    NonAsciiSep(char),
    #[error("Separator {0:?} is a ascii digit")]
    DigitSep(char),
}
impl ICFormat {
    pub const fn ascii_default() -> Self {
        match Self::ascii(',') {
            Ok(v) => v,
            Err(_) => unreachable!(),
        }
    }
    pub const fn binary_default() -> Self {
        Self::binary(Default::default())
    }
    pub const fn ascii(sep: char) -> Result<Self, NewAsciiFormatError> {
        if !sep.is_ascii() {
            Err(NewAsciiFormatError::NonAsciiSep(sep))
        } else if sep.is_ascii_digit() {
            Err(NewAsciiFormatError::DigitSep(sep))
        } else {
            Ok(Self::Ascii { sep: sep as u8 })
        }
    }
    pub const fn binary(endianness: ByteOrder) -> Self {
        Self::Binary { endianness }
    }
}
impl const Default for ICFormat {
    fn default() -> Self {
        Self::ascii_default()
    }
}

#[derive(Debug, Error)]
pub enum ICReadError {
    #[error(transparent)]
    IO(#[from] io::Error),
    #[error("Non ascii byte {0:x}")]
    NonAscii(u8),
    #[error("Expected {1}, found {0:?}")]
    Unexpected(char, &'static str),
    #[error("Value too big to fit inside ICValue")]
    ValueTooBig(
        #[from]
        #[source]
        ParseIntError,
    ),
}
impl ICFormat {
    /// Read a input stream into a Program
    ///
    /// The implementation call `read` for every few bytes, if reading from file a `BufReader` is suggested
    pub fn read(&self, src: &mut impl Read) -> Result<ICProgram, ICReadError> {
        match *self {
            ICFormat::Ascii { sep } => {
                #[derive(Debug, EnumDiscriminants)]
                enum AsciiFmtFSM {
                    WaitForValue,
                    AccumulateValue(ArrayString<25>),
                    WaitingForSeparator,
                }
                impl AsciiFmtFSM {
                    fn try_into_accumulate_value(self) -> Result<ArrayString<25>, Self> {
                        if let Self::AccumulateValue(v) = self {
                            Ok(v)
                        } else {
                            Err(self)
                        }
                    }

                    fn as_accumulate_value_mut(&mut self) -> Option<&mut ArrayString<25>> {
                        if let Self::AccumulateValue(v) = self {
                            Some(v)
                        } else {
                            None
                        }
                    }
                }
                use AsciiFmtFSMDiscriminants::*;
                use ICReadError::*;

                src.bytes()
                    .map(Some)
                    .chain(once(None))
                    .scan(
                        AsciiFmtFSM::WaitForValue,
                        |current, byte| -> Option<Option<Result<ICValue, ICReadError>>> {
                            match byte {
                                Some(Ok(v)) if !v.is_ascii() => Some(Some(Err(NonAscii(v)))),
                                Some(Ok(v)) => match (AsciiFmtFSMDiscriminants::from(&*current), v)
                                {
                                    (WaitForValue, v) if v.is_ascii_whitespace() => Some(None),
                                    (WaitForValue, v) if v.is_ascii_digit() || v == b'-' => {
                                        let mut s = ArrayString::new();
                                        s.push(v as char);
                                        *current = AsciiFmtFSM::AccumulateValue(s);
                                        Some(None)
                                    }
                                    (WaitForValue, v) => Some(Some(Err(Unexpected(
                                        v as char,
                                        "whitespace, digit or '-'",
                                    )))),
                                    (AccumulateValue, v) if v.is_ascii_whitespace() || v == sep => {
                                        let v = mem::replace(
                                            current,
                                            if v == sep {
                                                AsciiFmtFSM::WaitForValue
                                            } else {
                                                AsciiFmtFSM::WaitingForSeparator
                                            },
                                        )
                                        .try_into_accumulate_value()
                                        .unwrap()
                                        .parse::<ICValue>()
                                        .map_err(Into::into);
                                        Some(Some(v))
                                    }
                                    (AccumulateValue, v) if v.is_ascii_digit() => {
                                        let s = current.as_accumulate_value_mut().unwrap();
                                        match s.try_push(v as char) {
                                            Ok(()) => Some(None),
                                            Err(_) => Some(Some(Err(s
                                                .parse::<ICValue>()
                                                .err()
                                                .expect("The string should be too long for ICValue")
                                                .into()))),
                                        }
                                    }
                                    (AccumulateValue, v) => Some(Some(Err(Unexpected(
                                        v as char,
                                        "digit, separator or whitespace",
                                    )))),
                                    (WaitingForSeparator, v) if v == sep => {
                                        *current = AsciiFmtFSM::WaitForValue;
                                        Some(None)
                                    }
                                    (WaitingForSeparator, v) if v.is_ascii_whitespace() => {
                                        Some(None)
                                    }
                                    (WaitingForSeparator, v) => Some(Some(Err(Unexpected(
                                        v as char,
                                        "separator or whitespace",
                                    )))),
                                },
                                Some(Err(err)) => Some(Some(Err(err.into()))),
                                None => {
                                    // bytes ended. Check for last value...
                                    match current {
                                        AsciiFmtFSM::WaitingForSeparator
                                        | AsciiFmtFSM::WaitForValue => Some(None), // No last value
                                        AsciiFmtFSM::AccumulateValue(s) => {
                                            let v = s.parse::<ICValue>().map_err(Into::into);
                                            Some(Some(v))
                                        }
                                    }
                                }
                            }
                        },
                    )
                    .filter_map(identity) // Ignore the `None`s between a result and another
                    .collect()
            }
            ICFormat::Binary { endianness } => repeat(())
                .map(move |_| {
                    match endianness {
                        ByteOrder::BigEndian => src.read_i64::<byteorder::BigEndian>(),
                        ByteOrder::LittleEndian => src.read_i64::<byteorder::LittleEndian>(),
                        ByteOrder::NativeEndian => src.read_i64::<byteorder::NativeEndian>(),
                        ByteOrder::NetworkEndian => src.read_i64::<byteorder::NetworkEndian>(),
                    }
                    .map(|v| Some(ICValue(v)))
                    .or_else(|err| match err.kind() {
                        io::ErrorKind::UnexpectedEof => Ok(None),
                        _ => Err(err.into()),
                    })
                    .transpose()
                })
                .scan((), |_, item| item)
                .collect(),
        }
    }

    /// Write a program into a output stream
    ///
    /// The implementation call `write` for every few bytes, if reading from file a `BufWriter` is suggested
    pub fn write(&self, prog: &ICProgram, dest: &mut impl Write) -> io::Result<()> {
        match *self {
            ICFormat::Ascii { sep } => {
                for v in prog.iter().copied().map(Left).intersperse(Right(())) {
                    match v {
                        Left(v) => write!(dest, "{v}")?,
                        Right(()) => write!(dest, "{}", sep as char)?,
                    }
                }
                Ok(())
            }
            ICFormat::Binary { endianness } => {
                for v in prog.iter().copied() {
                    match endianness {
                        ByteOrder::BigEndian => dest.write_i64::<byteorder::BigEndian>(v.into()),
                        ByteOrder::LittleEndian => {
                            dest.write_i64::<byteorder::LittleEndian>(v.into())
                        }
                        ByteOrder::NativeEndian => {
                            dest.write_i64::<byteorder::NativeEndian>(v.into())
                        }
                        ByteOrder::NetworkEndian => {
                            dest.write_i64::<byteorder::NetworkEndian>(v.into())
                        }
                    }?
                }
                Ok(())
            }
        }
    }
}

#[cfg(test)]
mod test {
    use strum::IntoEnumIterator;

    use crate::{ICProgram, ICValue};

    use super::{ByteOrder, ICFormat, NewAsciiFormatError};

    #[test]
    fn ascii_format_rejects_bad_seps() {
        use NewAsciiFormatError::*;
        for sep in (u8::MIN..u8::MAX).map(char::from) {
            let res = ICFormat::ascii(sep).err();
            if !sep.is_ascii() {
                assert_eq!(res, Some(NonAsciiSep(sep)))
            } else if sep.is_ascii_digit() {
                assert_eq!(res, Some(DigitSep(sep)))
            } else {
                assert_eq!(res, None)
            }
        }
    }

    /// iter all possible formats
    fn iter_formats() -> impl Iterator<Item = ICFormat> {
        ByteOrder::iter().map(ICFormat::binary).chain(
            (u8::MIN..u8::MAX)
                .map(|sep| ICFormat::ascii(sep as _))
                .filter_map(Result::ok),
        )
    }

    static CASES: &[(&[i64], &str, char)] = &[
        (&[1, 2, 3], "1,2,3", ','),
        (&[1, 2, 3], "1\t, 2\n, 3", ','),
        (&[-1, 2, 3], "-1,2,3,", ','),
        (&[1, 2, -3], "1;2;-3", ';'),
        (&[], "", ','),
        (&[3], "3,", ','),
        (&[3], "3", ','),
        (&[-53], " -53 ", ','),
        (&[1, 2, 3], "1;2;3", ';'),
        (&[1, 2, 3], "1 2  3", ' '),
    ];

    #[test]
    fn ascii() {
        for (values, src, sep) in CASES.into_iter().copied() {
            let prog = ICProgram::from_iter(values.into_iter().copied().map(ICValue));
            let bytes = src.as_bytes();
            let format = ICFormat::ascii(sep).expect("Cases should contains only valid separators");

            // check parsing
            {
                let readed = format
                    .read(&mut &*bytes)
                    .expect("Example should be readable");
                assert_eq!(prog, readed, "Format did not write ")
            }
        }
    }

    #[test]
    fn round_trip() {
        for format in iter_formats() {
            for (values, _, _) in CASES.into_iter().copied() {
                let prog = ICProgram::from_iter(values.into_iter().copied().map(ICValue));

                // check round tripping
                {
                    let mut writed: Vec<u8> = Vec::new();
                    format
                        .write(&prog, &mut writed)
                        .expect("Vec should always be writable");
                    let readed = format
                        .read(&mut writed.as_slice())
                        .expect("Example should be readable");
                    assert_eq!(prog, readed, "Format did not round-trip")
                }
            }
        }
    }
}
