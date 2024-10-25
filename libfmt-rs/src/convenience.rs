use crate::engine::{Engine, SIZE_INFINITY};
use crate::model::{BeginToken, BreakToken, Flow};

impl Engine {
    /// Begin tracking block using a horizontal flow for any breaks
    ///
    /// * ***indent***: The number of spaces to indent the block beyond the typical 4 space indent
    pub fn scan_begin_horizontal(&mut self, indent: isize) {
        self.scan_begin(BeginToken {
            offset: indent,
            flow: Flow::Horizontal,
        });
    }

    /// Begin tracking block using a vertical flow for any breaks
    ///
    /// * ***indent***: The number of spaces to indent the block beyond the typical 4 space indent
    pub fn scan_begin_vertical(&mut self, indent: isize) {
        self.scan_begin(BeginToken {
            offset: indent,
            flow: Flow::Vertical,
        });
    }

    /// Add a single space to the buffer
    pub fn scan_space(&mut self) {
        self.scan_string(" ");
    }

    /// Add a break to the buffer that will use a zero spaces if the break is used
    /// during the final printing of the code
    pub fn scan_break_zero(&mut self) {
        self.scan_break(BreakToken::space(0));
    }

    /// Add a break to the buffer that will use a single space if the break is used
    /// during the final printing of the code
    pub fn scan_break_space(&mut self) {
        self.scan_break_spaces(1);
    }

    /// Add a break to the buffer that will use the given number of spaces if the break is used
    /// during the final printing of the code
    pub fn scan_break_spaces(&mut self, n: usize) {
        self.scan_break(BreakToken::space(n));
    }

    /// Add a BreakToken with the never_break flag set
    pub fn scan_break_never(&mut self) {
        self.scan_break(BreakToken::never());
    }

    /// Add a break to the buffer that will use a newline in the final printing of the code
    pub fn scan_break_newline(&mut self) {
        self.scan_break_spaces(SIZE_INFINITY as usize);
    }

    /// Add a break to the buffer that will use a single space if the break is used if the buffer is
    /// nonempty
    pub fn scan_break_space_if_nonempty(&mut self) {
        self.scan_break(BreakToken {
            blank_space: 1,
            if_nonempty: true,
            ..BreakToken::default()
        });
    }

    /// Add a break to the buffer that will use a newline if the buffer is not empty
    pub fn scan_break_newline_if_nonempty(&mut self) {
        self.scan_break(BreakToken {
            blank_space: SIZE_INFINITY as usize,
            if_nonempty: true,
            ..BreakToken::default()
        });
    }

    pub fn trailing_comma(&mut self, is_last: bool) {
        if is_last {
            self.scan_break(BreakToken {
                pre_break: Some(','),
                ..BreakToken::default()
            });
        } else {
            self.scan_string(",");
            self.scan_break_space();
        }
    }

    pub fn trailing_comma_or_space(&mut self, is_last: bool) {
        if is_last {
            self.scan_break(BreakToken {
                blank_space: 1,
                pre_break: Some(','),
                ..BreakToken::default()
            });
        } else {
            self.scan_string(",");
            self.scan_break_space();
        }
    }
}
