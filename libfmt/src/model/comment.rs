use proc_macro2::LineColumn;
use std::ops::Range;

pub(crate) struct Comment {
    pub(crate) src: String,         // Comment string
    pub(crate) range: Range<usize>, // Location of next token in the source code
    pub(crate) start: LineColumn,   // Start of the next token in the source code
    pub(crate) end: LineColumn,     // End of the next token in the source code
}
