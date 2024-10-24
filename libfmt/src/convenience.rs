use crate::engine::{Engine, SIZE_INFINITY};
use crate::model::{BeginToken, BreakToken, Flow};

impl Engine {
    /// Begin tracking block using a horizontal flow for any breaks
    pub fn scan_begin_horizontal(&mut self, indent: isize) {
        self.scan_begin(BeginToken {
            offset: indent,
            flow: Flow::Horizontal,
        });
    }

    /// Begin tracking block using a vertical flow for any breaks
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
    pub fn scan_zero_break(&mut self) {
        self.scan_break(BreakToken::space_break(0));
    }

    /// Add a break to the buffer that will use a single space if the break is used
    /// during the final printing of the code
    pub fn scan_space_break(&mut self) {
        self.scan_spaces_break(1);
    }

    /// Add a break to the buffer that will use the given number of spaces if the break is used
    /// during the final printing of the code
    pub fn scan_spaces_break(&mut self, n: usize) {
        self.scan_break(BreakToken::space_break(n));
    }

    /// Add a BreakToken with the neverbreak flag set
    pub fn scan_never_break(&mut self) {
        self.scan_break(BreakToken::never_break());
    }

    /// Add a break to the buffer that will use a newline if the break is used during the final
    /// printing of the code
    pub fn scan_newline_break(&mut self) {
        self.scan_spaces_break(SIZE_INFINITY as usize);
    }

    /// Add a break to the buffer that will use a single space if the break is used if the buffer is
    /// nonempty
    pub fn scan_space_break_if_nonempty(&mut self) {
        self.scan_break(BreakToken {
            blank_space: 1,
            if_nonempty: true,
            ..BreakToken::default()
        });
    }

    /// Add a break to the buffer that will use a newline if the buffer is not empty and if the
    /// break is used during the final printing of the code
    pub fn scan_newline_break_if_nonempty(&mut self) {
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
            self.scan_space_break();
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
            self.scan_space_break();
        }
    }
}
