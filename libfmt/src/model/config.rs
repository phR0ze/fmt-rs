#[derive(Debug, Clone)]
pub struct Config {
    pub(crate) indent: isize,         // Line indent in spaces
    pub(crate) max_line_width: isize, // Maximum line width before wrapping occurs
    pub(crate) min_line_width: isize, // 60?

    /// Enable or disable features
    comments: bool,
    smart_wrapping: bool,
    drop_trailing_comma: bool,
}

/// Default implementation
impl Default for Config {
    fn default() -> Self {
        Self {
            indent: 4,
            max_line_width: 100,
            min_line_width: 60,
            comments: true,
            smart_wrapping: true,
            drop_trailing_comma: true,
        }
    }
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
            drop_trailing_comma: false,
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

    /// Disable dropping trailing comma
    pub fn with_no_drop_trailing_comma(mut self) -> Self {
        self.drop_trailing_comma = false;
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

    /// Return true if drop_trailing_comma is enabled
    pub fn drop_trailing_comma(&self) -> bool {
        self.drop_trailing_comma
    }
}
