use std::{env, fs};

use chumsky::{Parser, Span, Stream};

mod cli {
    use std::{error, fmt, io};

    use chumsky::error::Simple;

    use kaytlin_syntax::{Span, Token};

    #[derive(Debug)]
    pub enum Error {
        MissingSourcePath,
        SourceContainsUnicode,
        Io(io::Error),

        Lexer(Simple<char, Span>),
        Parser(Simple<Token, Span>),
    }

    impl fmt::Display for Error {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match self {
                Error::MissingSourcePath => write!(f, "no source file path has been provided"),
                Error::SourceContainsUnicode => write!(f, "source contains non-ascii chars"),
                Error::Io(io_err) => write!(f, "{}", io_err),

                Error::Lexer(lexer_err) => write!(f, "{}", lexer_err),
                Error::Parser(parser_err) => write!(f, "{}", parser_err),
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

    impl From<Vec<Simple<char, Span>>> for Error {
        fn from(mut lexer_errs: Vec<Simple<char, Span>>) -> Self {
            Error::Lexer(lexer_errs.remove(0))
        }
    }

    impl From<Vec<Simple<Token, Span>>> for Error {
        fn from(mut parser_errs: Vec<Simple<Token, Span>>) -> Self {
            Error::Parser(parser_errs.remove(0))
        }
    }
}

fn try_main() -> Result<(), cli::Error> {
    let path = env::args().nth(1).ok_or(cli::Error::MissingSourcePath)?;
    let code = fs::read_to_string(path)?;
    if !code.is_ascii() {
        return Err(cli::Error::SourceContainsUnicode);
    }

    let len = code.chars().count();
    let span = |i| kaytlin_syntax::Span::new(code.clone(), i..i + 1);

    let tokens = kaytlin_syntax::lexer().parse(Stream::from_iter(
        span(len),
        code.chars().enumerate().map(|(i, c)| (c, span(i))),
    ))?;
    dbg!(&tokens);

    let ast = kaytlin_syntax::parser().parse(Stream::from_iter(span(len), tokens.into_iter()))?;
    dbg!(&ast);

    Ok(())
}

fn main() {
    if let Err(error) = try_main() {
        panic!("{:?}", error);
    }
}
