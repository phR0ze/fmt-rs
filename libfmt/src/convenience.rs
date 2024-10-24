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

    fn scan_spaces(&mut self, n: usize) {
        self.scan_break(BreakToken {
            blank_space: n,
            ..BreakToken::default()
        });
    }

    pub fn zerobreak(&mut self) {
        self.scan_spaces(0);
    }

    pub fn scan_space(&mut self) {
        self.scan_spaces(1);
    }

    /// Add a single space to the buffer
    pub fn nbsp(&mut self) {
        self.scan_string(" ");
    }

    /// Add a single newline to the buffer
    pub fn scan_hardbreak(&mut self) {
        self.scan_spaces(SIZE_INFINITY as usize);
    }

    pub fn space_if_nonempty(&mut self) {
        self.scan_break(BreakToken {
            blank_space: 1,
            if_nonempty: true,
            ..BreakToken::default()
        });
    }

    pub fn hardbreak_if_nonempty(&mut self) {
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
            self.scan_space();
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
            self.scan_space();
        }
    }

    /// Create a neverbreak break token
    pub fn neverbreak(&mut self) {
        self.scan_break(BreakToken::neverbreak());
    }
}
