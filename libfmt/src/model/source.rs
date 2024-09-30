use super::Position;

/// Source provides a convenient way to store the source code of a program as a 2D vector of characters.
/// This allows for easy manipulation of the source code for whitespace lookups.
///
/// * `Vec<Vec<char>>` - A 2D vector of characters representing the source code
/// * `Position` - Tracking for our current position in the source code
pub(crate) struct Source {
    src: Vec<Vec<char>>,
    pos: Position,
    end: Position, // One past the source end
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

    /// Advance the current position by one character relative to the source. This means that source
    /// lines and columns are taken into account and position will advance accordingly. Once the end
    /// of the source is reached it will refuse to advance further.
    pub(crate) fn adv_one(&mut self) {
        self.pos = self.calculate_adv(self.pos);
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
    pub(crate) fn curr_is<C: Into<char>, P: Into<Position>>(&self, pos: P, c: C) -> bool {
        self.get(pos).filter(|x| **x == c.into()).is_some()
    }

    /// Get string from the current position to the given position otherwise return None.
    /// * Non inclusive of the end position
    /// * If end is past source end then all characters up to the end of the source will be returned
    pub(crate) fn str<T: Into<Position>>(&self, end: T) -> Option<String> {
        let end = end.into();

        if self.src.len() > 0 && self.pos < self.end && self.pos < end {
            let mut str = String::new();
            let mut p = self.pos;

            // Not inclusive of the end position
            while p < self.end && p < end {
                if let Some(c) = self.get(p) {
                    str.push(*c);
                }
                p = self.calculate_adv(p);
            }
            Some(str)
        } else {
            None
        }
    }

    /// Get the end position of the source. End actually refers to one character past the end of
    /// of the last line in the source.
    pub(crate) fn end(&self) -> Position {
        self.end
    }

    /// Get the current position
    ///
    /// * return - The current position
    pub(crate) fn get_pos(&self) -> Position {
        self.pos
    }

    /// Get the character at the previous position
    pub(crate) fn prev(&self) -> Option<&char> {
        let p = self.pos;
        if p.column > 0 {
            self.get((p.line, p.column - 1))
        } else if p.line > 0 {
            if let Some(c) = self.src[p.line - 1].len().checked_sub(1) {
                self.get((p.line - 1, c))
            } else {
                self.get((p.line - 1, 0))
            }
        } else {
            None
        }
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

    /// Calculate advancing the given position by one relative to the source. This means that
    /// source lines and columns are taken into account and position will advance accordingly.
    /// Once the end of the source is reached it will refuse to advance further.
    ///
    /// * return - newly calculated position after advancement by one
    fn calculate_adv<T: Into<Position>>(&self, position: T) -> Position {
        let p = position.into();

        if p < self.end - 1 {
            // If we are not at the end of the line, advance the column
            if p.column < self.src[p.line].len() - 1 {
                Position::new(p.line, p.column + 1)

            // If we are at the end of the line, advance the line
            } else {
                Position::new(p.line + 1, 0)
            }
        } else {
            self.end
        }
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
    use super::super::pos;
    use super::*;

    #[test]
    fn test_char_is() {
        let source = Source::new("Hello\nFoo\n");
        assert_eq!(source.curr_is((0, 0), 'H'), true);
        assert_eq!(source.curr_is((0, 4), 'o'), true);
        assert_eq!(source.curr_is((1, 0), 'F'), true);
        assert_eq!(source.curr_is((1, 3), '\n'), true);
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
    fn test_adv() {
        let mut source = Source::new("Hello, World!\nThis is a test.\n");
        source.set_pos((0, 12));

        source.adv_one();
        assert_eq!(source.get_pos(), pos(0, 13));
        assert_eq!(source.curr(), Some(&'\n'));

        source.adv_one();
        assert_eq!(source.get_pos(), pos(1, 0));
        assert_eq!(source.curr(), Some(&'T'));

        source.set_pos((1, 15));
        assert_eq!(source.curr(), Some(&'\n'));

        // Advance off end
        source.adv_one();
        assert_eq!(source.get_pos(), pos(1, 16));
        assert_eq!(source.curr(), None);

        // No further advancing allowed once at end
        source.adv_one();
        assert_eq!(source.get_pos(), pos(1, 16));
        assert_eq!(source.curr(), None);
    }

    #[test]
    fn test_calculate_adv() {
        let source = Source::new("Hello\nFoo\n");
        assert_eq!(source.calculate_adv(pos(0, 0)), pos(0, 1));
        assert_eq!(source.calculate_adv(pos(0, 1)), pos(0, 2));
        assert_eq!(source.calculate_adv(pos(0, 2)), pos(0, 3));
        assert_eq!(source.calculate_adv(pos(0, 3)), pos(0, 4));
        assert_eq!(source.calculate_adv(pos(0, 4)), pos(0, 5));
        assert_eq!(source.calculate_adv(pos(0, 5)), pos(1, 0));
        assert_eq!(source.calculate_adv(pos(1, 0)), pos(1, 1));
        assert_eq!(source.calculate_adv(pos(1, 1)), pos(1, 2));
        assert_eq!(source.calculate_adv(pos(1, 2)), pos(1, 3));
        assert_eq!(source.calculate_adv(pos(1, 3)), pos(1, 4));
        assert_eq!(source.calculate_adv(pos(1, 4)), pos(1, 4));
    }

    #[test]
    fn test_set_pos() {
        let mut source = Source::new("Hello\nFoo\n");

        // Exists start
        assert_eq!(source.get_pos(), pos(0, 0));

        assert_eq!(source.set_pos((1, 3)), true);
        assert_eq!(source.get_pos(), pos(1, 3));

        // Allow for end to be set
        assert_eq!(source.set_pos((1, 4)), false);
        assert_eq!(source.get_pos(), pos(1, 3));

        assert_eq!(source.set_pos((3, 4)), false);
        assert_eq!(source.get_pos(), pos(1, 3));
    }

    #[test]
    fn test_pos() {
        let mut source = Source::new("Hello, World!\nThis is a test.\n");
        source.set_pos((1, 1));
        assert_eq!(source.get_pos(), pos(1, 1));
        assert_eq!(source.curr(), Some(&'h'));
    }

    #[test]
    fn test_prev() {
        let mut source = Source::new("Hello, World!\nThis is a test.\n");

        // No previous char
        source.set_pos((0, 0));
        assert_eq!(source.prev(), None);

        // Previous char is 'H'
        source.set_pos((0, 1));
        assert_eq!(source.prev(), Some(&'H'));

        // Check previous char on previous line
        source.set_pos((1, 0));
        assert_eq!(source.prev(), Some(&'\n'));
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
        assert_eq!(source.prev(), None);
        source.set_pos((1, 1));
        assert_eq!(source.get_pos(), pos(0, 0));
        source.adv_one();
        assert_eq!(source.get_pos(), pos(0, 0));
    }
}
