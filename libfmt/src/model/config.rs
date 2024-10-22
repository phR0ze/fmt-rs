#[derive(Debug, Clone)]
pub struct Config {
    pub(crate) indent: isize,    // 4?
    pub(crate) margin: isize,    // 89?
    pub(crate) min_space: isize, // 60?

    /// Enable or disable features
    comments: bool,
    smart_wrapping: bool,
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

    /// Return true if comments are enabled
    pub fn comments(&self) -> bool {
        self.comments
    }

    /// Return true if commments are disabled
    pub fn no_comments(&self) -> bool {
        self.comments == false
    }

    /// Disable smart wrapping
    pub fn with_no_smart_wrapping(&self) -> Self {
        Self {
            smart_wrapping: false,
            ..Self::default()
        }
    }

    /// Return true if smart_wrapping is enabled
    pub fn smart_wrapping(&self) -> bool {
        self.smart_wrapping
    }

    /// Return true if smart_wrapping is disabled
    pub fn no_smart_wrapping(&self) -> bool {
        self.smart_wrapping == false
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
        }
    }
}
