use std::borrow::Cow;

use super::{BeginToken, BreakToken};

/// Buffer Entry is a token and its size.
#[derive(Debug, Clone)]
pub(crate) struct BufEntry {
    pub(crate) token: Token,
    pub(crate) size: isize,
}

#[derive(Debug, Clone)]
pub(crate) enum Token {
    ///
    String(Cow<'static, str>),

    ///
    Break(BreakToken),

    /// Begin tokens are pushed onto the ring-buffer when a block is opened. They carry an offset
    Begin(BeginToken),

    /// End tokens are pushed onto the ring-buffer when a block is closed.
    End,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_buf_entry_debug() {
        let entry = BufEntry {
            token: Token::String(Cow::Borrowed("hello")),
            size: 5,
        };
        assert_eq!(
            format!("{:?}", entry),
            "BufEntry { token: String(\"hello\"), size: 5 }",
        );
    }

    #[test]
    fn test_token_debug() {
        let token = Token::String(Cow::Borrowed("hello"));
        assert_eq!(format!("{:?}", token), "String(\"hello\")",);
    }
}