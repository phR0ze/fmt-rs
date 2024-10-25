use std::{fmt, io};

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug)]
#[non_exhaustive]
pub struct Error {
    msg: String,
    data: Option<String>,
    source: Option<Box<dyn std::error::Error>>,
}

impl Error {
    pub(crate) fn new<T: AsRef<str>>(msg: T) -> Self {
        Self {
            msg: msg.as_ref().to_string(),
            data: None,
            source: None,
        }
    }

    pub(crate) fn with_data(mut self, data: &str) -> Self {
        self.data = Some(data.to_string());
        self
    }

    /// Add the given error as a source to this error.
    pub(crate) fn wrap(mut self, err: Error) -> Self {
        self.source = Some(Box::new(err));
        self
    }

    /// Add the given error as a source to this error.
    pub(crate) fn wrap_io(mut self, err: io::Error) -> Self {
        self.source = Some(Box::new(err));
        self
    }

    /// Add the given error as a source to this error.
    pub(crate) fn wrap_lex(mut self, err: proc_macro2::LexError) -> Self {
        self.source = Some(Box::new(err));
        self
    }

    /// Add the given error as a source to this error.
    pub(crate) fn wrap_syn(mut self, err: syn::Error) -> Self {
        self.source = Some(Box::new(err));
        self
    }

    /// Add the given error as a source to this error.
    pub(crate) fn wrap_any(mut self, err: Box<dyn std::error::Error>) -> Self {
        self.source = Some(err);
        self
    }
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        self.source.as_ref().map(|s| s.as_ref())
    }
}

impl AsRef<dyn std::error::Error> for Error {
    fn as_ref(&self) -> &(dyn std::error::Error + 'static) {
        self
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.msg)?;

        // Display additional data if available
        if let Some(data) = self.data.as_ref() {
            if !data.is_empty() {
                write!(f, ": {}", data)?;
            };
        };
        Ok(())
    }
}

#[cfg(test)]
pub(crate) fn to_string<T: AsRef<dyn std::error::Error>>(err: T) -> String {
    let mut errs: Vec<String> = Vec::new();
    let mut err = err.as_ref();
    errs.push(err.to_string());

    loop {
        match err.source() {
            Some(e) => {
                errs.push(e.to_string());
                err = e;
            }
            None => break,
        }
    }
    errs.join(" ==> ")
}

mod tests {
    use super::*;

    #[test]
    fn with_data() {
        assert_eq!(
            Error::new("test").with_data("data").to_string(),
            "test: data"
        );
    }

    #[test]
    fn chain() {
        let msg = vec![
            "top level error",
            "source error 0",
            "source error 1",
            "source error 2",
            "source error 3",
        ];

        let err = to_string(
            Error::new(msg[0]).wrap(
                Error::new(msg[1])
                    .wrap(Error::new(msg[2]).wrap(Error::new(msg[3]).wrap(Error::new(msg[4])))),
            ),
        );
        assert_eq!(err.to_string(), msg.join(" ==> "));
    }
}
