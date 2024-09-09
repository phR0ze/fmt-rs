use std::io;

mod error;
pub use error::{Error, Result};

/// Format the given
pub fn format<T: io::Write>(src: &mut T) -> Result<()> {
    src.write_all(b"Hello, world!")
        .map_err(|e| Error::new("failed to write to file").wrap_io(e))
    //Err(Error::new("Not implemented"))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_format() {
        let mut data = io::Cursor::new(Vec::new());
        format(&mut data).unwrap();
        assert_eq!(
            String::from_utf8(data.into_inner()).unwrap(),
            "Hello, world!"
        );
    }
}
