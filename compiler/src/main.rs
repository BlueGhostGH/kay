use std::{env, fs};

use kay_analysis::lower;
use kay_syntax::{parse_module, src::SrcId};

use kay::Error;

fn try_main() -> Result<(), Error> {
    let path = env::args().nth(1).ok_or(Error::MissingSourcePath)?;
    let src = SrcId::from_path(path.clone());
    let code = fs::read_to_string(path)?;
    if !code.is_ascii() {
        return Err(Error::SourceContainsUnicode);
    }

    let (ast, syntax_errors) = parse_module(&code, src);

    dbg!(syntax_errors);

    if let Some(ast) = ast {
        let hir = lower(ast.into_inner());

        dbg!(hir);
    }

    Ok(())
}

fn main() {
    if let Err(error) = try_main() {
        panic!("{:?}", error);
    }
}
