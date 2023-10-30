use std::{
    ffi::{OsStr, OsString},
    fmt::{Display, Write},
    fs::File,
    io::{self, stdin, stdout, Stdin, Stdout},
    path::PathBuf,
};

#[derive(Debug, Clone)]
pub(crate) enum PathOrDash {
    Path(PathBuf),
    Dash,
}
impl PathOrDash {
    pub(crate) fn with_extension<S: AsRef<OsStr>>(&self, extension: S) -> PathOrDash {
        match self {
            PathOrDash::Path(path) => PathOrDash::Path(path.with_extension(extension)),
            PathOrDash::Dash => todo!(),
        }
    }
}

impl From<OsString> for PathOrDash {
    fn from(value: OsString) -> Self {
        if value.as_os_str_bytes() == b"-" {
            Self::Dash
        } else {
            Self::Path(PathBuf::from(value))
        }
    }
}

impl Display for PathOrDash {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PathOrDash::Path(path) => path.display().fmt(f),
            PathOrDash::Dash => f.write_char('-'),
        }
    }
}

#[derive(Debug)]
pub(crate) enum FileOrStdin {
    File(File),
    Stdin(Stdin),
}

impl FileOrStdin {
    pub(crate) fn open(path: &PathOrDash) -> io::Result<Self> {
        match path {
            PathOrDash::Path(path) => File::open(path).map(Self::File),
            PathOrDash::Dash => Ok(Self::Stdin(stdin())),
        }
    }
}
impl std::io::Read for FileOrStdin {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        match self {
            FileOrStdin::File(f) => f.read(buf),
            FileOrStdin::Stdin(s) => s.read(buf),
        }
    }

    fn read_vectored(&mut self, bufs: &mut [io::IoSliceMut<'_>]) -> io::Result<usize> {
        match self {
            FileOrStdin::File(f) => f.read_vectored(bufs),
            FileOrStdin::Stdin(s) => s.read_vectored(bufs),
        }
    }

    fn read_to_end(&mut self, buf: &mut Vec<u8>) -> io::Result<usize> {
        match self {
            FileOrStdin::File(f) => f.read_to_end(buf),
            FileOrStdin::Stdin(s) => s.read_to_end(buf),
        }
    }

    fn read_to_string(&mut self, buf: &mut String) -> io::Result<usize> {
        match self {
            FileOrStdin::File(f) => f.read_to_string(buf),
            FileOrStdin::Stdin(s) => s.read_to_string(buf),
        }
    }

    fn read_exact(&mut self, buf: &mut [u8]) -> io::Result<()> {
        match self {
            FileOrStdin::File(f) => f.read_exact(buf),
            FileOrStdin::Stdin(s) => s.read_exact(buf),
        }
    }
}

#[derive(Debug)]
pub(crate) enum FileOrStdout {
    File(File),
    Stdout(Stdout),
}

impl FileOrStdout {
    pub(crate) fn create(path: &PathOrDash) -> io::Result<Self> {
        match path {
            PathOrDash::Path(path) => File::create(path).map(Self::File),
            PathOrDash::Dash => Ok(Self::Stdout(stdout())),
        }
    }
}
impl std::io::Write for FileOrStdout {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        match self {
            FileOrStdout::File(f) => f.write(buf),
            FileOrStdout::Stdout(o) => o.write(buf),
        }
    }

    fn flush(&mut self) -> io::Result<()> {
        match self {
            FileOrStdout::File(f) => f.flush(),
            FileOrStdout::Stdout(o) => o.flush(),
        }
    }

    fn write_vectored(&mut self, bufs: &[io::IoSlice<'_>]) -> io::Result<usize> {
        match self {
            FileOrStdout::File(f) => f.write_vectored(bufs),
            FileOrStdout::Stdout(o) => o.write_vectored(bufs),
        }
    }

    fn write_all(&mut self, buf: &[u8]) -> io::Result<()> {
        match self {
            FileOrStdout::File(f) => f.write_all(buf),
            FileOrStdout::Stdout(o) => o.write_all(buf),
        }
    }

    fn write_fmt(&mut self, fmt: std::fmt::Arguments<'_>) -> io::Result<()> {
        match self {
            FileOrStdout::File(f) => f.write_fmt(fmt),
            FileOrStdout::Stdout(o) => o.write_fmt(fmt),
        }
    }
}
