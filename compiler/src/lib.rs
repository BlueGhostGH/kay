use std::{error, fmt, io};

#[derive(Debug)]
pub enum Error {
    MissingSourcePath,
    SourceContainsUnicode,
    Io(io::Error),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::MissingSourcePath => write!(f, "no source file path has been provided"),
            Error::SourceContainsUnicode => write!(f, "source contains non-ascii chars"),
            Error::Io(io_err) => write!(f, "{}", io_err),
        }
    }
}

impl error::Error for Error {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        match self {
            Error::Io(io_err) => Some(io_err),
            _ => None,
        }
    }
}

impl From<io::Error> for Error {
    fn from(io_err: io::Error) -> Self {
        Error::Io(io_err)
    }
}
