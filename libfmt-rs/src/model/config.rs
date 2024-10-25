#[derive(Debug, Clone)]
pub struct Config {
    /// Line indent in spaces
    pub(crate) indent: isize,

    /// Maximum line width before wrapping occurs
    pub(crate) max_line_width: isize,

    /// Minimum line width
    pub(crate) min_line_width: isize,

    /// Number of empty lines allowed
    /// * Use None to indicate no limits should exist
    /// * Only applies when developer_comments are enabled
    pub(crate) num_empty_lines_allowed: Option<usize>,

    /// Enable or disable features
    f0000_drop_trailing_comma: bool,
    f0001_developer_comments: bool,
    f0002_smart_wrapping: bool,
}

/// Default implementation
impl Default for Config {
    fn default() -> Self {
        Self {
            indent: 4,
            max_line_width: 100,
            min_line_width: 60,
            num_empty_lines_allowed: None,

            // Feature controls
            f0001_developer_comments: true,
            f0002_smart_wrapping: true,
            f0000_drop_trailing_comma: true,
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
            f0000_drop_trailing_comma: false,
            f0001_developer_comments: false,
            f0002_smart_wrapping: false,
            ..Self::default()
        }
    }

    // Supporting configuration
    // --------------------------------------------------------------------------------------------
    // Grouping non-feature methods here.

    /// Set the number of empty lines to allow in sequence. Anything over this limit will be removed.
    /// This value is only taken into account if feature ***F0001: Developer comments*** is enabled.
    ///
    /// * ***None*** indicates no limit is specified
    /// * ***num_empty_lines_allowed*** is the number to use
    pub fn with_num_empty_lines_allowed(mut self, num_empty_lines_allowed: Option<usize>) -> Self {
        self.num_empty_lines_allowed = num_empty_lines_allowed;
        self
    }

    // Feature configuration
    // --------------------------------------------------------------------------------------------

    /// Disable feature ***F0000: Drop trailing comma***
    pub fn with_no_drop_trailing_comma(mut self) -> Self {
        self.f0000_drop_trailing_comma = false;
        self
    }

    /// Disable feature ***F0001: Developer comments***
    pub fn with_no_developer_comments(mut self) -> Self {
        self.f0001_developer_comments = false;
        self
    }

    /// Disable feature ***F0002: Smart wrapping***
    pub fn with_no_smart_wrapping(mut self) -> Self {
        self.f0002_smart_wrapping = false;
        self
    }

    /// State of feature ***F0000: Drop trailing comma***
    pub fn drop_trailing_comma(&self) -> bool {
        self.f0000_drop_trailing_comma
    }

    /// State of feature ***F0001: Developer comments***
    pub fn developer_comments(&self) -> bool {
        self.f0001_developer_comments
    }

    /// State of feature ***F0002: Smart wrapping***
    pub fn smart_wrapping(&self) -> bool {
        self.f0002_smart_wrapping
    }
}
