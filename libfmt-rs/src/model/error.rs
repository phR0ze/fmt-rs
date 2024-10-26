use std::{fmt, io};

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug)]
pub struct Error {
    msg: String,
    data: Option<String>,

    // Need Send and Sync to satisfy anyhow::Error
    source: Option<Box<dyn std::error::Error + Send + Sync>>,
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
        // Athough Send and Sync are implemented by this error type converting allows for a title
        self.source = Some(Box::new(SourceError::from("io::Error: ", err)));
        self
    }

    /// Add the given error as a source to this error.
    pub(crate) fn wrap_lex(mut self, err: proc_macro2::LexError) -> Self {
        // LexError does not implement Send or Sync thus we need to convert it to
        // a SourceError that does to satisfy the anyhow which most consumers will likely use
        self.source = Some(Box::new(SourceError::from("proc_macro2::LexError: ", err)));
        self
    }

    /// Add the given error as a source to this error.
    pub(crate) fn wrap_syn(mut self, err: syn::Error) -> Self {
        // Athough Send and Sync are implemented by this error type converting allows for a title
        self.source = Some(Box::new(SourceError::from("syn::Error: ", err)));
        self
    }
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        self.source
            .as_ref()
            // Need to cast to a lessor trait to satisfy the return type
            .map(|x| x.as_ref() as &(dyn std::error::Error + 'static))
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

/// ContextError is a simple error type that allows for converting underlying errors
/// into into a more readable error message with a prefix to indicate the underlying
/// component that generated the error.
#[derive(Debug)]
#[non_exhaustive]
pub struct SourceError {
    prefix: String,
    msg: String,
    source: Option<Box<SourceError>>,
}

impl SourceError {
    /// Convert the given error into an `SourceError` or chain of `SourceError`s
    pub(crate) fn from<T: std::error::Error>(prefix: &str, err: T) -> Self {
        let under = Self {
            prefix: prefix.into(),
            msg: err.to_string(),
            source: if let Some(err) = err.source() {
                Some(Self::from(prefix, err).into())
            } else {
                None
            },
        };

        under
    }
}

impl fmt::Display for SourceError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}{}", self.prefix, self.msg)
    }
}

impl std::error::Error for SourceError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match &self.source {
            Some(source) => Some(source),
            None => None,
        }
    }
}

// Provides a way to get the generic Error type
impl AsRef<dyn std::error::Error> for SourceError {
    fn as_ref(&self) -> &(dyn std::error::Error + 'static) {
        self
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
    fn test_source_error() {
        assert_eq!(
            SourceError::from("io::Error: ", io::Error::from(io::ErrorKind::NotFound)).to_string(),
            "io::Error: entity not found"
        );
    }

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
