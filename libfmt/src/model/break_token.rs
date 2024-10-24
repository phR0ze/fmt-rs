/// BreakToken
#[derive(Debug, Clone, Copy, Default)]
pub(crate) struct BreakToken {
    /// ?
    pub offset: isize,

    /// Use this many spaces for this break when printing out the final code.
    /// * SIZE_INFINITY is used to indicate that the break should be a newline instead
    pub blank_space: usize,

    /// ?
    pub pre_break: Option<char>,

    /// ?
    pub post_break: Option<char>,

    /// ?
    pub no_break: Option<char>,

    /// ?
    pub if_nonempty: bool,

    /// ?
    pub never_break: bool,
}

impl BreakToken {
    /// Create a new BreakToken with the neverbreak flag set
    pub fn never() -> Self {
        Self {
            never_break: true,
            ..BreakToken::default()
        }
    }

    /// Create a new BreakToken with the blank_space property set to n
    pub fn space(n: usize) -> Self {
        Self {
            blank_space: n,
            ..BreakToken::default()
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_break_token_debug() {
        let token = BreakToken {
            offset: 4,
            blank_space: 2,
            pre_break: Some(' '),
            post_break: Some(' '),
            no_break: Some(' '),
            if_nonempty: true,
            never_break: false,
        };
        assert_eq!(
            format!("{:?}", token),
            "BreakToken { offset: 4, blank_space: 2, pre_break: Some(' '), post_break: Some(' '), no_break: Some(' '), if_nonempty: true, never_break: false }",
        );
    }
}
