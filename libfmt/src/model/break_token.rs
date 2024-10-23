/// Breaks can be consistent or inconsistent. Consistent breaking means that after the first break,
/// no attempt will be made to flow subsequent breaks together onto lines. Inconsistent is the
/// opposite. Inconsistent breaking example would be, say:
/// ```ignore
/// foo(hello, there, good, friends)
/// ```
///
/// breaking inconsistently to become
///
/// ```ignore
/// foo(hello, there,
///     good, friends);
/// ```
///
/// whereas a consistent breaking would yield:
///
/// ```ignore
/// foo(hello,
///     there,
///     good,
///     friends);
/// ```
///
/// That is, in the consistent-break blocks we value vertical alignment more than the ability to
/// cram stuff onto a line. But in all cases if it can make a block a one-liner, it’ll do so.
#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) enum Break {
    /// Interprets all breaks as having a consistent vertical alignment
    ///
    /// #### Example:
    /// ```ignore
    /// foo(hello,
    ///     there,
    ///     good,
    ///     friends);
    /// ```
    Consistent,

    /// Flow subsequent components on the same line if possible after the break
    ///
    /// #### Example:
    /// ```ignore
    /// foo(hello, there,
    ///     good, friends);
    /// ```
    Inconsistent,
}

/// Contains information about how to break
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

    #[test]
    fn test_break_debug() {
        assert_eq!(format!("{:?}", Break::Consistent), "Consistent",);
        assert_eq!(format!("{:?}", Break::Inconsistent), "Inconsistent",);
    }
}
