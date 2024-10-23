#[derive(Debug, Clone)]
pub struct Config {
    pub(crate) indent: isize,    // 4?
    pub(crate) margin: isize,    // 89?
    pub(crate) min_space: isize, // 60?

    /// Enable or disable features
    comments: bool,
    smart_wrapping: bool,
    skip_trailing_comma: bool,
}

impl Config {
    pub fn new() -> Self {
        Self::default()
    }

    /// Disable all configuration features
    pub fn none() -> Self {
        Self {
            comments: false,
            smart_wrapping: false,
            skip_trailing_comma: false,
            ..Self::default()
        }
    }

    /// Disable comments
    pub fn with_no_comments(mut self) -> Self {
        self.comments = false;
        self
    }

    /// Disable smart wrapping
    pub fn with_no_smart_wrapping(mut self) -> Self {
        self.smart_wrapping = false;
        self
    }

    /// Disable skipping trailing comma
    pub fn with_no_skip_trailing_comma(mut self) -> Self {
        self.skip_trailing_comma = false;
        self
    }

    /// Return true if comments are enabled
    pub fn comments(&self) -> bool {
        self.comments
    }

    /// Return true if smart_wrapping is enabled
    pub fn smart_wrapping(&self) -> bool {
        self.smart_wrapping
    }

    /// Return true if skip_trailing_comma is enabled
    pub fn skip_trailing_comma(&self) -> bool {
        self.skip_trailing_comma
    }
}

/// Default implementation
impl Default for Config {
    fn default() -> Self {
        Self {
            indent: 4,
            margin: 89,
            min_space: 60,
            comments: true,
            smart_wrapping: true,
            skip_trailing_comma: true,
        }
    }
}
