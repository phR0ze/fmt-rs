// Adapted from https://github.com/rust-lang/rust/blob/1.57.0/compiler/rustc_ast_pretty/src/pp.rs.
// See "Algorithm notes" in the crate-level rustdoc.
// https://doc.rust-lang.org/stable/nightly-rustc/rustc_ast_pretty/pp/index.html
use crate::model::*;
use std::borrow::Cow;
use std::collections::VecDeque;
use std::iter;
use std::{cmp, fmt};
use tracing::trace;

pub(crate) const SIZE_INFINITY: isize = 0xffff;

/// Printer handles a stream of input tokens.
///
/// The printer buffers up to 3N tokens inside itself, where N is linewidth. Yes, linewidth is chars
/// and tokens are multi-char, but in the worst case every token worth buffering is 1 char long, so
/// it’s ok.
///
/// Tokens are String, Break, and Begin/End to delimit block.
///
/// Tokens are buffered through the RingBuffer. The ‘left’ and ‘right’ indices denote the active
/// portion of the ring buffer as well as describing hypothetical points-in-the-infinite-stream at
/// most 3N tokens apart (i.e., “not wrapped to ring-buffer boundaries”). ‘left’ and ‘right’ terms
/// are used to denote the wrapped-to-ring-buffer and point-in-infinite-stream.
///
/// There is a parallel ring buffer, size, that holds the calculated size of each token. Why
/// calculated? Because for Begin/End pairs, the “size” includes everything between the pair. That
/// is, the “size” of Begin is actually the sum of the sizes of everything between Begin and the
/// paired End that follows. Since that is arbitrarily far in the future, size is being rewritten
/// regularly while the printer runs; in fact most of the machinery is here to work out size entries
/// on the fly (and give up when they’re so obviously over-long that “infinity” is a good enough
/// approximation for purposes of line breaking).
///
/// The “input side” of the printer is managed as an abstract process called SCAN, which uses
/// scan_stack, to manage calculating size. SCAN is, in other words, the process of calculating
/// ‘size’ entries.
///
/// The “output side” of the printer is managed by an abstract process called PRINT, which uses
/// print_stack, margin and space to figure out what to do with each token/size pair it consumes as
/// it goes. It’s trying to consume the entire buffered window, but can’t output anything until the
/// size is >= 0 (sizes are set to negative while they’re pending calculation).
///
/// So SCAN takes input and buffers tokens and pending calculations, while PRINT gobbles up
/// completed calculations and tokens from the buffer. The theory is that the two can never get more
/// than 3N tokens apart, because once there’s “obviously” too much data to fit on a line, in a size
/// calculation, SCAN will write “infinity” to the size and let PRINT consume it.
pub struct Engine {
    /// Formatting configuration
    pub(crate) config: Config,

    /// Original input source
    pub(crate) src: String,

    /// Final formatted output
    pub(crate) out: String,

    /// Used to track how much of the max line width is still available. As characters are printed,
    /// this value is decremented.
    pub(crate) space: isize,

    /// Ring buffer of tokens and calculated size of strings i.e. number of characters
    pub(crate) scan_buf: RingBuffer<BufEntry>,

    /// Tracks the total size (i.e. number of characters) of tokens that have been printed from the
    /// ring buffer.
    /// Always starts at 1
    pub(crate) left_total: isize,

    /// Tracks the total size (i.e. number of characters) of tokens that have been enqueued in the
    /// ring buffer.
    /// Always starts at 1
    pub(crate) right_total: isize,

    /// Tracks the current block being scanned. Holds ring-buffer index values of the Begin that
    /// started the current block, possibly with the most recent Break after that Begin (if there is
    /// any) on top of it. Values are pushed and popped on the back of the queue using it like
    /// stack, and elsewhere old values are popped from the front of the queue as they become
    /// irrelevant due to the primary ring-buffer advancing.
    pub(crate) scan_stack: VecDeque<usize>,

    /// Stack of blocks-in-progress being flushed by print methods
    pub(crate) print_stack: Vec<PrintFrame>,

    /// Print related field for level of indentation of current line
    pub(crate) indent: usize,

    /// Print related field for indentation to avoid writing trailing whitespace
    pub(crate) pending_indentation: usize,
}

impl Engine {
    pub fn new(source: &str, config: Config) -> Self {
        let margin = config.margin;
        Self {
            config,
            src: source.into(),
            out: String::new(),
            space: margin,
            scan_buf: RingBuffer::new(),
            left_total: 0,
            right_total: 0,
            scan_stack: VecDeque::new(),
            print_stack: Vec::new(),
            indent: 0,
            pending_indentation: 0,
        }
    }

    /// Complete the formatting process and return the final output string.
    pub fn print(mut self) -> String {
        trace!("Print");

        if !self.scan_stack.is_empty() {
            self.check_stack(0);
            self.print_any();
        }
        self.out
    }

    /// Marks the beginning of a block of code by pushing a BeginToken onto the scan buffer and
    /// tracks it's index on the scan stack.
    /// * Used for: Start, Open Paren, Open Brace, Open Bracket, async, etc...
    pub fn scan_begin(&mut self, token: BeginToken) {
        trace!("Scan begin: {:?}", token);

        if self.scan_stack.is_empty() {
            self.left_total = 1;
            self.right_total = 1;
            self.scan_buf.clear();
        }
        let right = self.scan_buf.push(BufEntry {
            token: Scan::Begin(token),
            size: -self.right_total,
        });
        self.scan_stack.push_back(right);

        trace!("{}", self);
    }

    /// Marks the end of a block of code by pushing an End token onto the scan buffer and tracks it's
    /// index on the scan stack.
    /// * Used for: End, Close Paren, Close Brace, Close Bracket, etc...
    pub fn scan_end(&mut self) {
        trace!("Scan end");

        if self.scan_stack.is_empty() {
            self.print_end();
        } else {
            if !self.scan_buf.is_empty() {
                if let Scan::Break(break_token) = self.scan_buf.last().token {
                    if self.scan_buf.len() >= 2 {
                        if let Scan::Begin(_) = self.scan_buf.second_last().token {
                            self.scan_buf.pop_last();
                            self.scan_buf.pop_last();
                            self.scan_stack.pop_back();
                            self.scan_stack.pop_back();
                            self.right_total -= break_token.blank_space as isize;
                            return;
                        }
                    }
                    if break_token.if_nonempty {
                        self.scan_buf.pop_last();
                        self.scan_stack.pop_back();
                        self.right_total -= break_token.blank_space as isize;
                    }
                }
            }
            let right = self.scan_buf.push(BufEntry {
                token: Scan::End,
                size: -1,
            });
            self.scan_stack.push_back(right);
        }

        trace!("{}", self);
    }

    /// Marks a break in the code by pushing a BreakToken onto the scan buffer and tracks it's index
    /// on the scan stack.
    /// * Used for: Comma, Semicolon, etc...
    pub fn scan_break(&mut self, token: BreakToken) {
        trace!("Scan break: {:?}", token);

        if self.scan_stack.is_empty() {
            self.left_total = 1;
            self.right_total = 1;
            self.scan_buf.clear();
        } else {
            self.check_stack(0);
        }
        let right = self.scan_buf.push(BufEntry {
            token: Scan::Break(token),
            size: -self.right_total,
        });
        self.scan_stack.push_back(right);
        self.right_total += token.blank_space as isize;

        trace!("{}", self);
    }

    /// Push a string onto the scan buffer or simply print it to out if there is no begin in progress.
    pub fn scan_string<S: Into<Cow<'static, str>>>(&mut self, string: S) {
        let string = string.into();
        trace!("Scan string: {:?}", string);

        if self.scan_stack.is_empty() {
            self.print_string(string);
        } else {
            // Store the string and its length in the scan buffer
            let len = string.len() as isize;
            self.scan_buf.push(BufEntry {
                token: Scan::String(string),
                size: len,
            });

            // Update the total string length to include the new string
            self.right_total += len;

            self.print_if_past_max_line_width();
        }

        trace!("{}", self);
    }

    /// Update the previous break to include the detected offset change.
    pub fn offset(&mut self, offset: isize) {
        trace!("Offset: {:?}", offset);

        match &mut self.scan_buf.last_mut().token {
            // Update the previous break to include detected offset change
            Scan::Break(token) => token.offset += offset,

            // Nothing to do here
            Scan::Begin(_) => {}

            // The algorithm should never reach this point as there will always be a break or begin
            // token present if `offset` is being called.
            Scan::String(_) | Scan::End => unreachable!(),
        }

        trace!("{}", self);
    }

    /// ?
    pub fn end_with_max_width(&mut self, max: isize) {
        trace!("End with max width: {:?}", max);

        let mut depth = 1;
        for &index in self.scan_stack.iter().rev() {
            let entry = &self.scan_buf[index];
            match entry.token {
                Scan::Begin(_) => {
                    depth -= 1;
                    if depth == 0 {
                        if entry.size < 0 {
                            let actual_width = entry.size + self.right_total;
                            if actual_width > max {
                                self.scan_buf.push(BufEntry {
                                    token: Scan::String(Cow::Borrowed("")),
                                    size: SIZE_INFINITY,
                                });
                                self.right_total += SIZE_INFINITY;
                            }
                        }
                        break;
                    }
                }
                Scan::End => depth += 1,
                Scan::Break(_) => {}
                Scan::String(_) => unreachable!(),
            }
        }
        self.scan_end();
    }

    /// Print the current block of code to the output string if the max line width threshold has
    /// been exceeded.
    fn print_if_past_max_line_width(&mut self) {
        trace!("Print if past max line width");

        // While the current stream is longer than the allowed max width
        while self.right_total - self.left_total > self.space {
            // Pop the first element from the scan stack if it is also the first
            // element in the scan buffer, then update the scan buffer element's size to infinity.
            if *self.scan_stack.front().unwrap() == self.scan_buf.index_of_first() {
                self.scan_stack.pop_front().unwrap();
                self.scan_buf.first_mut().size = SIZE_INFINITY;
            }

            // Now print out all tokens until a control token of negative size is encountered
            self.print_any();

            if self.scan_buf.is_empty() {
                break;
            }
        }
    }

    /// ?
    pub(crate) fn check_stack(&mut self, mut depth: usize) {
        trace!("Check stack");

        while let Some(&index) = self.scan_stack.back() {
            let entry = &mut self.scan_buf[index];
            match entry.token {
                Scan::Begin(_) => {
                    if depth == 0 {
                        break;
                    }
                    self.scan_stack.pop_back().unwrap();
                    entry.size += self.right_total;
                    depth -= 1;
                }
                Scan::End => {
                    self.scan_stack.pop_back().unwrap();
                    entry.size = 1;
                    depth += 1;
                }
                Scan::Break(_) => {
                    self.scan_stack.pop_back().unwrap();
                    entry.size += self.right_total;
                    if depth == 0 {
                        break;
                    }
                }
                Scan::String(_) => unreachable!(),
            }
        }

        trace!("{}", self);
    }

    /// Prints any token of zero or positive size. During print the control tokens that usually have
    /// a negative size are updated to have a positive size such that print_next will consume them.
    /// Also updates the left_total field to track the printed characters.
    pub(crate) fn print_any(&mut self) {
        trace!("Print any");

        while self.scan_buf.first().size >= 0 {
            let left = self.scan_buf.pop_first();

            match left.token {
                Scan::String(string) => {
                    if self.skip_trailing_comma(&string) {
                        continue;
                    }
                    self.left_total += left.size;
                    self.print_string(string);
                }
                Scan::Break(token) => {
                    self.left_total += token.blank_space as isize;
                    self.print_break(token, left.size);
                }
                Scan::Begin(token) => self.print_begin(token, left.size),
                Scan::End => self.print_end(),
            }

            if self.scan_buf.is_empty() {
                break;
            }
        }

        trace!("{}", self);
    }

    /// If the given value is a comma and the scan buffer still has more tokens and the next token
    /// is an End token, then skip the trailing comma.
    fn skip_trailing_comma(&self, value: &str) -> bool {
        if value == "," && !self.scan_buf.is_empty() {
            if let Scan::End = &self.scan_buf.first().token {
                return true;
            }
        }
        return false;
    }

    fn get_top(&self) -> PrintFrame {
        const OUTER: PrintFrame = PrintFrame::Broken(0, Break::Inconsistent);
        self.print_stack.last().map_or(OUTER, PrintFrame::clone)
    }

    fn print_begin(&mut self, token: BeginToken, size: isize) {
        trace!("Print begin");

        if cfg!(prettyplease_debug) {
            self.out.push(match token.breaks {
                Break::Consistent => '«',
                Break::Inconsistent => '‹',
            });
            if cfg!(prettyplease_debug_indent) {
                self.out
                    .extend(token.offset.to_string().chars().map(|ch| match ch {
                        '0'..='9' => ['₀', '₁', '₂', '₃', '₄', '₅', '₆', '₇', '₈', '₉']
                            [(ch as u8 - b'0') as usize],
                        '-' => '₋',
                        _ => unreachable!(),
                    }));
            }
        }
        if size > self.space {
            self.print_stack
                .push(PrintFrame::Broken(self.indent, token.breaks));
            self.indent = usize::try_from(self.indent as isize + token.offset).unwrap();
        } else {
            self.print_stack.push(PrintFrame::Fits(token.breaks));
        }
    }

    fn print_end(&mut self) {
        trace!("Print end");

        let breaks = match self.print_stack.pop().unwrap() {
            PrintFrame::Broken(indent, breaks) => {
                self.indent = indent;
                breaks
            }
            PrintFrame::Fits(breaks) => breaks,
        };
        if cfg!(prettyplease_debug) {
            self.out.push(match breaks {
                Break::Consistent => '»',
                Break::Inconsistent => '›',
            });
        }

        trace!("Print end (out): {}", self.out);
    }

    fn print_break(&mut self, token: BreakToken, size: isize) {
        trace!("Print break: {:?}", token);

        let fits = token.never_break
            || match self.get_top() {
                PrintFrame::Fits(..) => true,
                PrintFrame::Broken(.., Break::Consistent) => false,
                PrintFrame::Broken(.., Break::Inconsistent) => size <= self.space,
            };
        if fits {
            self.pending_indentation += token.blank_space;
            self.space -= token.blank_space as isize;
            if let Some(no_break) = token.no_break {
                self.out.push(no_break);
                self.space -= no_break.len_utf8() as isize;
            }
            if cfg!(prettyplease_debug) {
                self.out.push('·');
            }
        } else {
            if let Some(pre_break) = token.pre_break {
                self.print_indent();
                self.out.push(pre_break);
            }
            if cfg!(prettyplease_debug) {
                self.out.push('·');
            }
            self.out.push('\n');
            let indent = self.indent as isize + token.offset;
            self.pending_indentation = usize::try_from(indent).unwrap();
            self.space = cmp::max(self.config.margin - indent, self.config.min_space);
            if let Some(post_break) = token.post_break {
                self.print_indent();
                self.out.push(post_break);
                self.space -= post_break.len_utf8() as isize;
            }
        }

        trace!("Print break (out): {}", self.out);
    }

    /// Print the given value including any indent to the output String
    fn print_string(&mut self, value: Cow<'static, str>) {
        trace!("Print string: {}", value);

        self.print_indent();

        self.out.push_str(&value);
        self.space -= value.len() as isize;

        if !self.out.is_empty() {
            trace!("Print stack: {:?}", self.print_stack);
            trace!("Print string (out): {}", self.out);
        }
    }

    /// Print indentation spaces to the output String
    fn print_indent(&mut self) {
        trace!("Print indent");

        self.out.reserve(self.pending_indentation);
        self.out
            .extend(iter::repeat(' ').take(self.pending_indentation));
        self.pending_indentation = 0;

        trace!("Print indent (out): {}", self.out);
    }
}

impl fmt::Display for Engine {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Engine {{")?;
        writeln!(f, "  out: {}", self.out)?;
        writeln!(f, "  space: {}", self.space)?;

        // Convert self.buf to debug string, split on newline, indent each line, and rejoin
        let buf = format!("{}", self.scan_buf);
        writeln!(
            f,
            "{}",
            buf.lines()
                .map(|line| format!("  {}", line))
                .collect::<Vec<String>>()
                .join("\n")
        )?;

        writeln!(f, "  left_total: {}", self.left_total)?;
        writeln!(f, "  right_total: {}", self.right_total)?;
        writeln!(f, "  scan_stack: {:?}", self.scan_stack)?;
        writeln!(f, "  print_stack: {:?}", self.print_stack)?;
        writeln!(f, "  indent: {}", self.indent)?;
        writeln!(f, "  pending_indentation: {}", self.pending_indentation)?;
        writeln!(f, "}}")?;

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_engine() {
        //
    }
}
