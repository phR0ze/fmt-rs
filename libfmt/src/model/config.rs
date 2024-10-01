pub struct Config {
    pub(crate) indent: isize,                          // 4?
    pub(crate) margin: isize,                          // 89?
    pub(crate) min_space: isize,                       // 60?
    pub(crate) invocation_brace_style_same_line: bool, // true?

    /// Enable or disable comments
    pub(crate) comments: bool,
}

impl Config {
    pub fn new() -> Self {
        Self::default()
    }

    /// Disable comments
    pub fn with_no_comments(&self) -> Self {
        Self {
            comments: false,
            ..Self::default()
        }
    }
}

/// Default implementation
impl Default for Config {
    fn default() -> Self {
        Self {
            comments: true,
            indent: 4,
            margin: 89,
            min_space: 60,
            invocation_brace_style_same_line: true,
        }
    }
}
