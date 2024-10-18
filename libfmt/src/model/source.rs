use super::Position;

/// Source provides a convenient way to store the source code of a program as a 2D vector of characters.
/// This allows for easy manipulation of the source code for whitespace lookups.
///
/// * `Vec<Vec<char>>` - A 2D vector of characters representing the source code
/// * `Position` - Tracking for our current position in the source code
#[derive(Debug)]
pub(crate) struct Source {
    src: Vec<Vec<char>>,
    pos: Position,

    /// Intentially one past the end
    end: Position,
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
            Position::new(i, src[i].len()) // intentially one past the end
        } else {
            Position::default()
        };

        Self {
            src,
            pos: Position::default(),
            end,
        }
    }

    /// Get the current position
    ///
    /// * return - The current position
    pub(crate) fn pos(&self) -> Position {
        self.pos
    }

    /// Get the character at the current position
    pub(crate) fn curr(&self) -> Option<&char> {
        self.get(self.pos)
    }

    /// Get the character at the given position or None if the position is out of bounds
    pub(crate) fn get<T: Into<Position>>(&self, position: T) -> Option<&char> {
        let p = position.into();
        self.src.get(p.line as usize)?.get(p.column as usize)
    }

    /// Check if the character at the given position is the given character
    pub(crate) fn char_at_is<C: Into<char>, P: Into<Position>>(&self, pos: P, c: C) -> bool {
        self.get(pos).filter(|x| **x == c.into()).is_some()
    }

    /// Advance the current position by one character relative to the source. This means that source
    /// lines and columns are taken into account and position will advance accordingly. Once the end
    /// of the source is reached it will refuse to advance further.
    pub(crate) fn adv_one(&mut self) -> Position {
        self.pos = self.inc(self.pos);
        self.pos
    }

    /// Check if the given range contains a newline character
    ///
    /// * ***start*** - The start position, inclusive
    /// * ***end*** - The end position inclusive
    pub(crate) fn contains_newline(&self, start: Option<Position>, end: Option<Position>) -> bool {
        if start.is_some() && end.is_some() {
            let (start, end) = (start.unwrap(), end.unwrap());
            let mut pos = start;
            while pos < self.end && pos <= end {
                if self.get(pos).filter(|x| *x == &'\n').is_some() {
                    return true;
                }
                pos = self.inc(pos);
            }
        }
        false
    }

    /// Get the given range of characters as a string
    /// * Non inclusive of the end position
    /// * If end is past source end then all characters up to the end of the source will be returned
    pub(crate) fn range(&self, start: Option<Position>, end: Option<Position>) -> Option<String> {
        let mut pos = start.unwrap_or_default();
        let end = end.unwrap_or(Position::max());

        if self.src.len() > 0 && pos < self.end && pos < end {
            let mut str = String::new();

            // Not inclusive of the end position
            while pos < self.end && pos < end {
                if let Some(c) = self.get(pos) {
                    str.push(*c);
                }
                pos = self.inc(pos);
            }
            Some(str)
        } else {
            None
        }
    }

    /// Get string from the current position to the given position otherwise return None.
    /// * Non inclusive of the end position
    /// * If end is past source end then all characters up to the end of the source will be returned
    pub(crate) fn str<T: Into<Position>>(&self, end: T) -> Option<String> {
        self.range(Some(self.pos), Some(end.into()))
    }

    /// Get the end position of the source. End actually refers to one character past the end of
    /// of the last line in the source.
    pub(crate) fn end(&self) -> Position {
        self.end
    }

    /// Set the current position if it is valid and return true. Invalid positions will not be set
    /// and will return false.
    pub(crate) fn set_pos<T: Into<Position>>(&mut self, position: T) -> bool {
        let mut result = false;

        if self.src.len() > 0 {
            let p = position.into();

            // Check that the given position exists
            if self.get(p).is_some() {
                self.pos = p;
                result = true;
            }
        }

        result
    }

    /// Decrement the given position by one relative to the source. This means that
    /// source lines and columns are taken into account and position will decrement accordingly.
    /// Once the start of the source is reached it will refuse to decrement further.
    ///
    /// Automatically decrements to end of source if was beyond the end. This is true for self.end
    /// which is one past the source end. Thus to iterator backward you can start with self.end
    ///
    /// * ***pos*** - The position to decrement
    pub(crate) fn dec(&self, pos: Position) -> Position {
        let mut p = pos;

        if p > Position::default() {
            if p > self.end {
                p = self.end;
            }

            // Column can be decremented
            if p.column > 0 {
                if p.column > self.src[p.line].len() - 1 {
                    return Position::new(p.line, self.src[p.line].len() - 1);
                }
                return Position::new(p.line, p.column - 1);

            // Line can be decremented
            } else if p.line > 0 {
                // indexing is safe here as we checked position is within source
                return Position::new(p.line - 1, self.src[p.line - 1].len() - 1);
            }
        }
        Position::default()
    }

    /// Increment the given position by one relative to the source. This means that
    /// source lines and columns are taken into account and position will advance accordingly.
    /// Once the end of the source is reached it will refuse to advance further.
    ///
    /// * ***pos*** - The position to increment
    fn inc(&self, pos: Position) -> Position {
        if pos < self.end - 1 {
            // If we are not at the end of the line, advance the column
            // Indexing is safe as we know pos < end and column length is always at least 1
            if pos.column < self.src[pos.line].len() - 1 {
                return Position::new(pos.line, pos.column + 1);

            // If we are at the end of the line, advance the line
            } else {
                return Position::new(pos.line + 1, 0);
            }
        }

        self.end
    }
}

/// Implement from string for source
impl From<&str> for Source {
    fn from(source: &str) -> Self {
        Source::new(source)
    }
}

/// Extension trait for chars to add some convenience methods
pub(crate) trait CharExt {
    /// Check if the character is a whitespace character
    fn is_whitespace(&self) -> bool;

    /// Check if the character is not a whitespace character
    fn is_not_whitespace(&self) -> bool;
}

impl CharExt for Option<&char> {
    fn is_whitespace(&self) -> bool {
        self.filter(|x| x.is_whitespace()).is_some()
    }
    fn is_not_whitespace(&self) -> bool {
        self.filter(|x| !x.is_whitespace()).is_some()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn pos(line: usize, column: usize) -> Position {
        Position::new(line, column)
    }
    fn pos2(line: usize, column: usize) -> Option<Position> {
        pos(line, column).into()
    }

    #[test]
    fn test_contains_newline() {
        let source = Source::new("Hello\nFoo\n");
        assert_eq!(source.contains_newline(pos2(0, 0), pos2(0, 1)), false);
        assert_eq!(source.contains_newline(pos2(0, 0), pos2(0, 2)), false);
        assert_eq!(source.contains_newline(pos2(0, 0), pos2(0, 5)), true);
        assert_eq!(source.contains_newline(pos2(0, 5), pos2(0, 6)), true);
        assert_eq!(source.contains_newline(pos2(0, 5), pos2(0, 7)), true);
        assert_eq!(source.contains_newline(pos2(0, 6), pos2(0, 7)), false);
        assert_eq!(source.contains_newline(pos2(0, 6), pos2(0, 9)), false);
        assert_eq!(source.contains_newline(pos2(1, 0), pos2(1, 3)), true);

        assert_eq!(
            source.contains_newline(Some(Position::default()), Some(Position::max())),
            true
        );
    }

    #[test]
    fn test_char_is() {
        let source = Source::new("Hello\nFoo\n");
        assert_eq!(source.char_at_is((0, 0), 'H'), true);
        assert_eq!(source.char_at_is((0, 4), 'o'), true);
        assert_eq!(source.char_at_is((1, 0), 'F'), true);
        assert_eq!(source.char_at_is((1, 3), '\n'), true);
    }

    #[test]
    fn test_str() {
        let mut source = Source::new("Hello\nFoo\n");
        assert_eq!(source.str((0, 5)), Some("Hello".to_string()));

        // Everything if end is past source end
        assert_eq!(source.str((1, 5)), Some("Hello\nFoo\n".to_string()));

        // None if before current position
        source.set_pos((1, 0));
        assert_eq!(source.str((0, 0)), None);

        // Grab a selection
        source.set_pos((0, 2));
        assert_eq!(source.str((1, 2)), Some("llo\nFo".to_string()));
    }

    #[test]
    fn test_inc() {
        let mut source = Source::new("Hello!\nWorld\n");
        assert_eq!(source.curr(), Some(&'H'));

        assert_eq!(source.adv_one(), pos(0, 1));
        assert_eq!(source.curr(), Some(&'e'));

        assert_eq!(source.adv_one(), pos(0, 2));
        assert_eq!(source.curr(), Some(&'l'));

        assert_eq!(source.adv_one(), pos(0, 3));
        assert_eq!(source.curr(), Some(&'l'));

        assert_eq!(source.adv_one(), pos(0, 4));
        assert_eq!(source.curr(), Some(&'o'));

        assert_eq!(source.adv_one(), pos(0, 5));
        assert_eq!(source.curr(), Some(&'!'));

        assert_eq!(source.adv_one(), pos(0, 6));
        assert_eq!(source.curr(), Some(&'\n'));

        assert_eq!(source.adv_one(), pos(1, 0));
        assert_eq!(source.curr(), Some(&'W'));

        assert_eq!(source.adv_one(), pos(1, 1));
        assert_eq!(source.curr(), Some(&'o'));

        assert_eq!(source.adv_one(), pos(1, 2));
        assert_eq!(source.curr(), Some(&'r'));

        assert_eq!(source.adv_one(), pos(1, 3));
        assert_eq!(source.curr(), Some(&'l'));

        assert_eq!(source.adv_one(), pos(1, 4));
        assert_eq!(source.curr(), Some(&'d'));

        assert_eq!(source.adv_one(), pos(1, 5));
        assert_eq!(source.curr(), Some(&'\n'));

        assert_eq!(source.adv_one(), pos(1, 6));
        assert_eq!(source.curr(), None);
    }

    #[test]
    fn test_dec() {
        let source = Source::new("Hello!\nWorld\n");

        // Invalid position should default to end
        assert_eq!(source.dec(pos(2, 5)), pos(1, 5));

        // Invalid column len will get set to len - 1
        assert_eq!(source.dec(pos(0, 15)), pos(0, 6));

        // start from the end and work to the begining
        assert_eq!(source.dec(source.end()), pos(1, 5));
        assert_eq!(source.dec(pos(1, 6)), pos(1, 5));
        assert_eq!(source.dec(pos(1, 0)), pos(0, 6));
        assert_eq!(source.dec(pos(0, 6)), pos(0, 5));
        assert_eq!(source.dec(pos(0, 1)), pos(0, 0));

        // If zero then no more decrementing
        assert_eq!(source.dec(pos(0, 0)), pos(0, 0));
    }

    #[test]
    fn test_set_pos() {
        let mut source = Source::new("Hello\nFoo\n");

        // Exists start
        assert_eq!(source.pos(), pos(0, 0));

        assert_eq!(source.set_pos((1, 3)), true);
        assert_eq!(source.pos(), pos(1, 3));

        // Allow for end to be set
        assert_eq!(source.set_pos((1, 4)), false);
        assert_eq!(source.pos(), pos(1, 3));

        assert_eq!(source.set_pos((3, 4)), false);
        assert_eq!(source.pos(), pos(1, 3));
    }

    #[test]
    fn test_pos() {
        let mut source = Source::new("Hello, World!\nThis is a test.\n");
        source.set_pos((1, 1));
        assert_eq!(source.pos(), pos(1, 1));
        assert_eq!(source.curr(), Some(&'h'));
    }

    #[test]
    fn test_get() {
        let source = Source::new("Hello, World!\nThis is a test.\n");
        assert_eq!(source.get((0, 0)), Some(&'H'));
        assert_eq!(source.get((0, 7)), Some(&'W'));
        assert_eq!(source.get((1, 0)), Some(&'T'));
        assert_eq!(source.get((1, 5)), Some(&'i'));
        assert_eq!(source.get((1, 14)), Some(&'.'));
        assert_eq!(source.get((1, 15)), Some(&'\n'));
    }

    #[test]
    fn test_empty_source() {
        let mut source = Source::new("");
        assert_eq!(source.get((1, 5)), None);
        assert_eq!(source.str((1, 5)), None);
        source.set_pos(pos(1, 1));
        assert_eq!(source.pos(), pos(0, 0));
        source.adv_one();
        assert_eq!(source.pos(), pos(0, 0));
    }
}
