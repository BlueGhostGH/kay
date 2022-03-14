use std::{
    fmt::{self, Write},
    path::Path,
};

use internment::Intern;

#[derive(Clone)]
pub struct SrcId {
    inner: Intern<Vec<String>>,
}

impl fmt::Debug for SrcId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.inner.is_empty() {
            f.write_char('?')
        } else {
            write!(f, "{}", self.inner.join("/"))
        }
    }
}

impl fmt::Display for SrcId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl SrcId {
    #[cfg(test)]
    pub fn empty() -> Self {
        let inner = Intern::new(Vec::new());

        Self { inner }
    }

    pub fn from_path<P>(path: P) -> Self
    where
        P: AsRef<Path>,
    {
        let inner = Intern::new(
            path.as_ref()
                .iter()
                .map(|c| c.to_string_lossy().into_owned())
                .collect(),
        );

        Self { inner }
    }
}
