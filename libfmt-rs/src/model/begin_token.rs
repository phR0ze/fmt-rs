/// Flow can be vertical or horizontal. Vertical flow means that after the first break all
/// subsequent breaks will be aligned vertically similar to the Rustfmt default. Horizontal flow
/// means that each break will be evaluated to determine if it can be placed on the same line as the
/// previous component and only if it has reached the maximum line length will it break to the next
/// line. Thus verical flow favors using vertical space on your screen while horizontal flow favors
/// using horizontal space until the maximum line length is reached.
///
/// Flow is used in the BeginToken to guide how the begining of lines are handled and subsequent
/// flow for breaks.
///
/// ### Example
/// ```ignore
/// foo(hello, there, good, friends)
/// ```
///
/// Breaks will flow horizontally until the max line length is reached. with a short max line length
/// you'll get the results below.
/// ```ignore
/// foo(hello, there,
///     good, friends);
/// ```
///
/// Breaks will flow vertically with every break on a new line as shown below.
/// ```ignore
/// foo(hello,
///     there,
///     good,
///     friends);
/// ```
#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) enum Flow {
    /// Flow all subsequent breaks on separate lines with a left hand consistent vertical alignment
    ///
    /// #### Example:
    /// ```ignore
    /// foo(hello,
    ///     there,
    ///     good,
    ///     friends);
    /// ```
    Vertical,

    /// Flow all subsequent components on the same line if possible only breaking when max line
    /// length is reached while indenting subsequent lines with the given offset supplied during
    /// the begin token where this is used.
    ///
    /// #### Example:
    /// ```ignore
    /// foo(hello, there,
    ///     good, friends);
    /// ```
    Horizontal,
}

/// Begin tokens can carry an offset, saying "how far to indent when you break inside this block",
/// as well as a flow type that guides how breaks are handled.
#[derive(Debug, Clone, Copy)]
pub(crate) struct BeginToken {
    pub offset: isize,
    pub flow: Flow,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn begin_token_debug_output() {
        let token = BeginToken {
            offset: 4,
            flow: Flow::Vertical,
        };
        assert_eq!(
            format!("{:?}", token),
            "BeginToken { offset: 4, flow: Vertical }",
        );
    }

    #[test]
    fn flow_debug_output() {
        assert_eq!(format!("{:?}", Flow::Vertical), "Vertical",);
        assert_eq!(format!("{:?}", Flow::Horizontal), "Horizontal",);
    }
}
