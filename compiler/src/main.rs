use std::{env, fs};

use kay_syntax::{parse_module, SrcId};

mod cli {
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
}

fn try_main() -> Result<(), cli::Error> {
    let path = env::args().nth(1).ok_or(cli::Error::MissingSourcePath)?;
    let src = SrcId::from_path(path.clone());
    let code = fs::read_to_string(path)?;
    if !code.is_ascii() {
        return Err(cli::Error::SourceContainsUnicode);
    }

    let (ast, syntax_errors) = parse_module(&code, src);

    dbg!(ast);
    dbg!(syntax_errors);

    Ok(())
}

fn main() {
    if let Err(error) = try_main() {
        panic!("{:?}", error);
    }
}
