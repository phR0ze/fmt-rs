use std::borrow::Cow;

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
#[derive(Clone, Copy, PartialEq)]
pub(crate) enum Break {
    Consistent,
    Inconsistent,
}

#[derive(Debug, Clone, Copy, Default)]
pub(crate) struct BreakToken {
    pub offset: isize,
    pub blank_space: usize,
    pub pre_break: Option<char>,
    pub post_break: Option<char>,
    pub no_break: Option<char>,
    pub if_nonempty: bool,
    pub never_break: bool,
}

/// Begin tokens can carry an offset, saying "how far to indent when you break inside this block",
/// as well as a flag indicating whether the block is "consistent" or "inconsistent".
#[derive(Clone, Copy)]
pub(crate) struct BeginToken {
    pub offset: isize,
    pub breaks: Break,
}

#[derive(Clone)]
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

#[derive(Clone)]
pub(crate) struct BufEntry {
    pub(crate) token: Token,
    pub(crate) size: isize,
}
