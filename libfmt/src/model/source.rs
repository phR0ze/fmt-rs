use crate::pos;
use proc_macro2::LineColumn;

/// Source provides a convenient way to store the source code of a program as a 2D vector of characters.
/// This allows for easy manipulation of the source code for whitespace lookups.
///
/// * `Vec<Vec<char>>` - A 2D vector of characters representing the source code
/// * `LineColumn` - Tracking for our current position in the source code
pub(crate) struct Source {
    src: Vec<Vec<char>>,
    pos: LineColumn,
    end: LineColumn,
}

impl Source {
    /// Convert the given source string into a 2D vector of characters
    pub(crate) fn new(source: &str) -> Self {
        let src: Vec<Vec<char>> = source
            .lines()
            .map(|line| {
                let mut column = line.chars().collect::<Vec<char>>();
                column.push('\n');
                column
            })
            .collect();

        // Calculate the end position
        let end = if let Some(i) = src.len().checked_sub(1) {
            pos(i, src[i].len() - 1)
        } else {
            pos(0, 0)
        };

        Self {
            src,
            pos: pos(0, 0),
            end,
        }
    }

    /// Advance the position by one character
    pub(crate) fn adv(&mut self) {
        let p = self.pos;

        // Ensure we are not at the end of the source
        if p.line < self.src.len() - 1 {
            // If we are not at the end of the line, advance the column
            if p.column < self.src[p.line].len() - 1 {
                self.pos = pos(p.line, p.column + 1);

            // If we are at the end of the line, advance the line
            } else {
                self.pos = pos(p.line + 1, 0);
            }
        }
    }

    /// Get the character at the current position
    pub(crate) fn curr(&self) -> Option<&char> {
        self.get(self.pos)
    }

    /// Get the character at the given position
    pub(crate) fn get(&self, position: LineColumn) -> Option<&char> {
        self.src
            .get(position.line as usize)?
            .get(position.column as usize)
    }

    /// Get string from the current position to the given position
    pub(crate) fn str(&self, end: LineColumn) -> Option<String> {
        if self.pos <= self.end {
            let mut str = String::new();
            let mut p = self.pos;

            while p < end {
                if let Some(c) = self.get(p) {
                    str.push(*c);
                }
                p = self.adv_pos(p);
            }
            Some(str)
        } else {
            None
        }
    }

    /// Advance the given position by one as pertains to the source.
    /// Once the end of the source is reached, the column will continue to advance off the end.
    fn adv_pos(&self, position: LineColumn) -> LineColumn {
        let p = position;

        if p < self.end {
            // If we are not at the end of the line, advance the column
            if p.column < self.src[p.line].len() - 1 {
                pos(p.line, p.column + 1)

            // If we are at the end of the line, advance the line
            } else {
                pos(p.line + 1, 0)
            }
        } else {
            // Simply advance off the end
            pos(p.line, p.column + 1)
        }
    }

    /// Get the current position
    pub(crate) fn get_pos(&self) -> LineColumn {
        self.pos
    }

    /// Get the character at the previous position
    pub(crate) fn prev(&self) -> Option<&char> {
        let p = self.pos;
        if p.column > 0 {
            self.get(pos(p.line, p.column - 1))
        } else if p.line > 0 {
            if let Some(c) = self.src[p.line - 1].len().checked_sub(1) {
                self.get(pos(p.line - 1, c))
            } else {
                self.get(pos(p.line - 1, 0))
            }
        } else {
            None
        }
    }

    /// Advance the position by one character
    pub(crate) fn set_pos(&mut self, pos: LineColumn) {
        self.pos = pos;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_str() {
        let mut source = Source::new("Hello\nFoo\n");
        assert_eq!(source.str(pos(0, 5)), Some("Hello".to_string()));
        assert_eq!(source.str(pos(1, 5)), Some("Hello\nFoo\n".to_string()));

        // None to return
        source.set_pos(pos(1, 4));
        assert_eq!(source.str(pos(1, 5)), None);
    }

    #[test]
    fn test_adv_pos() {
        let source = Source::new("Hello\nFoo\n");
        assert_eq!(source.adv_pos(pos(0, 4)), pos(0, 5));
        assert_eq!(source.adv_pos(pos(0, 5)), pos(1, 0));
        assert_eq!(source.adv_pos(pos(1, 3)), pos(1, 4));
    }

    #[test]
    fn test_adv() {
        let mut source = Source::new("Hello, World!\nThis is a test.\n");
        source.set_pos(pos(0, 12));

        source.adv();
        assert_eq!(source.get_pos(), pos(0, 13));
        assert_eq!(source.curr(), Some(&'\n'));

        source.adv();
        assert_eq!(source.get_pos(), pos(1, 0));
        assert_eq!(source.curr(), Some(&'T'));

        source.set_pos(pos(1, 15));
        source.adv();
        assert_eq!(source.get_pos(), pos(1, 15));
        assert_eq!(source.curr(), Some(&'\n'));
    }

    #[test]
    fn test_pos() {
        let mut source = Source::new("Hello, World!\nThis is a test.\n");
        source.set_pos(pos(1, 1));
        assert_eq!(source.get_pos(), pos(1, 1));
        assert_eq!(source.curr(), Some(&'h'));
    }

    #[test]
    fn test_prev() {
        let mut source = Source::new("Hello, World!\nThis is a test.\n");

        // No previous char
        source.set_pos(pos(0, 0));
        assert_eq!(source.prev(), None);

        // Previous char is 'H'
        source.set_pos(pos(0, 1));
        assert_eq!(source.prev(), Some(&'H'));

        // Check previous char on previous line
        source.set_pos(pos(1, 0));
        assert_eq!(source.prev(), Some(&'\n'));
    }

    #[test]
    fn test_get() {
        let source = Source::new("Hello, World!\nThis is a test.\n");
        assert_eq!(source.get(LineColumn { line: 0, column: 0 }), Some(&'H'));
        assert_eq!(source.get(LineColumn { line: 0, column: 7 }), Some(&'W'));
        assert_eq!(source.get(LineColumn { line: 1, column: 0 }), Some(&'T'));
        assert_eq!(source.get(LineColumn { line: 1, column: 5 }), Some(&'i'));
        assert_eq!(
            source.get(LineColumn {
                line: 1,
                column: 14
            }),
            Some(&'.')
        );
        assert_eq!(
            source.get(LineColumn {
                line: 1,
                column: 15
            }),
            Some(&'\n')
        );
    }
}
