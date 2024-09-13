use super::token::*;

use crate::ring::RingBuffer;
use crate::{MARGIN, MIN_SPACE};
use std::borrow::Cow;
use std::cmp;
use std::collections::VecDeque;
use std::iter;

#[derive(Copy, Clone)]
enum PrintFrame {
    Fits(Break),
    Broken(usize, Break),
}

pub(crate) const SIZE_INFINITY: isize = 0xffff;

/// Printer handles a stream of input tokens.
///
/// The printer buffers up to 3N tokens inside itself, where N is linewidth. Yes, linewidth is chars
/// and tokens are multi-char, but in the worst case every token worth buffering is 1 char long, so
/// it’s ok.
///
/// Tokens are String, Break, and Begin/End to delimit blocks.
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
    /// Final formatted output
    out: String,

    /// Number of spaces left on line
    space: isize,

    /// Ring-buffer of tokens and calculated sizes
    buf: RingBuffer<BufEntry>,

    /// Total size of tokens already printed
    left_total: isize,

    /// Total size of tokens enqueued, including printed and not yet printed
    right_total: isize,

    /// Holds the ring-buffer index of the Begin that started the current block,
    /// possibly with the most recent Break after that Begin (if there is any) on
    /// top of it. Values are pushed and popped on the back of the queue using it
    /// like stack, and elsewhere old values are popped from the front of the
    /// queue as they become irrelevant due to the primary ring-buffer advancing.
    scan_stack: VecDeque<usize>,

    /// Stack of blocks-in-progress being flushed by print
    print_stack: Vec<PrintFrame>,

    /// Level of indentation of current line
    indent: usize,

    /// Buffered indentation to avoid writing trailing whitespace
    pending_indentation: usize,
}

impl Engine {
    pub fn new() -> Self {
        Self {
            out: String::new(),
            space: MARGIN,
            buf: RingBuffer::new(),
            left_total: 0,
            right_total: 0,
            scan_stack: VecDeque::new(),
            print_stack: Vec::new(),
            indent: 0,
            pending_indentation: 0,
        }
    }

    pub fn eof(mut self) -> String {
        if !self.scan_stack.is_empty() {
            self.check_stack(0);
            self.advance_left();
        }
        self.out
    }

    pub fn scan_begin(&mut self, token: BeginToken) {
        if self.scan_stack.is_empty() {
            self.left_total = 1;
            self.right_total = 1;
            self.buf.clear();
        }
        let right = self.buf.push(BufEntry {
            token: Token::Begin(token),
            size: -self.right_total,
        });
        self.scan_stack.push_back(right);
    }

    pub fn scan_end(&mut self) {
        if self.scan_stack.is_empty() {
            self.print_end();
        } else {
            if !self.buf.is_empty() {
                if let Token::Break(break_token) = self.buf.last().token {
                    if self.buf.len() >= 2 {
                        if let Token::Begin(_) = self.buf.second_last().token {
                            self.buf.pop_last();
                            self.buf.pop_last();
                            self.scan_stack.pop_back();
                            self.scan_stack.pop_back();
                            self.right_total -= break_token.blank_space as isize;
                            return;
                        }
                    }
                    if break_token.if_nonempty {
                        self.buf.pop_last();
                        self.scan_stack.pop_back();
                        self.right_total -= break_token.blank_space as isize;
                    }
                }
            }
            let right = self.buf.push(BufEntry {
                token: Token::End,
                size: -1,
            });
            self.scan_stack.push_back(right);
        }
    }

    pub fn scan_break(&mut self, token: BreakToken) {
        if self.scan_stack.is_empty() {
            self.left_total = 1;
            self.right_total = 1;
            self.buf.clear();
        } else {
            self.check_stack(0);
        }
        let right = self.buf.push(BufEntry {
            token: Token::Break(token),
            size: -self.right_total,
        });
        self.scan_stack.push_back(right);
        self.right_total += token.blank_space as isize;
    }

    pub fn scan_string(&mut self, string: Cow<'static, str>) {
        if self.scan_stack.is_empty() {
            self.print_string(string);
        } else {
            let len = string.len() as isize;
            self.buf.push(BufEntry {
                token: Token::String(string),
                size: len,
            });
            self.right_total += len;
            self.check_stream();
        }
    }

    pub fn offset(&mut self, offset: isize) {
        match &mut self.buf.last_mut().token {
            Token::Break(token) => token.offset += offset,
            Token::Begin(_) => {}
            Token::String(_) | Token::End => unreachable!(),
        }
    }

    pub fn end_with_max_width(&mut self, max: isize) {
        let mut depth = 1;
        for &index in self.scan_stack.iter().rev() {
            let entry = &self.buf[index];
            match entry.token {
                Token::Begin(_) => {
                    depth -= 1;
                    if depth == 0 {
                        if entry.size < 0 {
                            let actual_width = entry.size + self.right_total;
                            if actual_width > max {
                                self.buf.push(BufEntry {
                                    token: Token::String(Cow::Borrowed("")),
                                    size: SIZE_INFINITY,
                                });
                                self.right_total += SIZE_INFINITY;
                            }
                        }
                        break;
                    }
                }
                Token::End => depth += 1,
                Token::Break(_) => {}
                Token::String(_) => unreachable!(),
            }
        }
        self.scan_end();
    }

    fn check_stream(&mut self) {
        while self.right_total - self.left_total > self.space {
            if *self.scan_stack.front().unwrap() == self.buf.index_of_first() {
                self.scan_stack.pop_front().unwrap();
                self.buf.first_mut().size = SIZE_INFINITY;
            }

            self.advance_left();

            if self.buf.is_empty() {
                break;
            }
        }
    }

    fn advance_left(&mut self) {
        while self.buf.first().size >= 0 {
            let left = self.buf.pop_first();

            match left.token {
                Token::String(string) => {
                    self.left_total += left.size;
                    self.print_string(string);
                }
                Token::Break(token) => {
                    self.left_total += token.blank_space as isize;
                    self.print_break(token, left.size);
                }
                Token::Begin(token) => self.print_begin(token, left.size),
                Token::End => self.print_end(),
            }

            if self.buf.is_empty() {
                break;
            }
        }
    }

    fn check_stack(&mut self, mut depth: usize) {
        while let Some(&index) = self.scan_stack.back() {
            let entry = &mut self.buf[index];
            match entry.token {
                Token::Begin(_) => {
                    if depth == 0 {
                        break;
                    }
                    self.scan_stack.pop_back().unwrap();
                    entry.size += self.right_total;
                    depth -= 1;
                }
                Token::End => {
                    self.scan_stack.pop_back().unwrap();
                    entry.size = 1;
                    depth += 1;
                }
                Token::Break(_) => {
                    self.scan_stack.pop_back().unwrap();
                    entry.size += self.right_total;
                    if depth == 0 {
                        break;
                    }
                }
                Token::String(_) => unreachable!(),
            }
        }
    }

    fn get_top(&self) -> PrintFrame {
        const OUTER: PrintFrame = PrintFrame::Broken(0, Break::Inconsistent);
        self.print_stack.last().map_or(OUTER, PrintFrame::clone)
    }

    fn print_begin(&mut self, token: BeginToken, size: isize) {
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
    }

    fn print_break(&mut self, token: BreakToken, size: isize) {
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
            self.space = cmp::max(MARGIN - indent, MIN_SPACE);
            if let Some(post_break) = token.post_break {
                self.print_indent();
                self.out.push(post_break);
                self.space -= post_break.len_utf8() as isize;
            }
        }
    }

    /// Print the given value including any indent to the output String
    fn print_string(&mut self, value: Cow<'static, str>) {
        self.print_indent();
        self.out.push_str(&value);
        self.space -= value.len() as isize;
    }

    /// Print indentation spaces to the output String
    fn print_indent(&mut self) {
        self.out.reserve(self.pending_indentation);
        self.out
            .extend(iter::repeat(' ').take(self.pending_indentation));
        self.pending_indentation = 0;
    }
}
