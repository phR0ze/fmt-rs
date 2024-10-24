/// BreakToken
#[derive(Debug, Clone, Copy, Default)]
pub(crate) struct BreakToken {
    /// ?
    pub offset: isize,

    /// ?
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
    /// Create a new neverbreak BreakToken
    pub fn neverbreak() -> Self {
        Self {
            never_break: true,
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
