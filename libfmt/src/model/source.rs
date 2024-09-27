use super::Position;

/// Source provides a convenient way to store the source code of a program as a 2D vector of characters.
/// This allows for easy manipulation of the source code for whitespace lookups.
///
/// * `Vec<Vec<char>>` - A 2D vector of characters representing the source code
/// * `Position` - Tracking for our current position in the source code
pub(crate) struct Source {
    src: Vec<Vec<char>>,
    pos: Position,
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
            Position::new(i, src[i].len() - 1)
        } else {
            Position::default()
        };

        Self {
            src,
            pos: Position::default(),
            end,
        }
    }

    /// Advance the position by one character
    pub(crate) fn adv(&mut self) {
        if self.src.len() == 0 {
            return;
        }

        let p = self.pos;

        // Ensure we are not at the end of the source
        if p.line < self.src.len() - 1 {
            // If we are not at the end of the line, advance the column
            if p.column < self.src[p.line].len() - 1 {
                self.pos = Position::new(p.line, p.column + 1);

            // If we are at the end of the line, advance the line
            } else {
                self.pos = Position::new(p.line + 1, 0);
            }
        }
    }

    /// Get the character at the current position
    pub(crate) fn curr(&self) -> Option<&char> {
        self.get(self.pos)
    }

    /// Get the character at the given position
    pub(crate) fn get<T: Into<Position>>(&self, position: T) -> Option<&char> {
        let p = position.into();
        self.src.get(p.line as usize)?.get(p.column as usize)
    }

    /// Get string from the current position to the given position
    pub(crate) fn str<T: Into<Position>>(&self, end: T) -> Option<String> {
        let end = end.into();

        if self.src.len() > 0 && self.pos <= self.end {
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

    /// Advance the given position by one relative to the source.
    /// Once the end of the source is reached, the column will continue to advance off the end.
    fn adv_pos<T: Into<Position>>(&self, position: T) -> Position {
        let p = position.into();

        if p < self.end {
            // If we are not at the end of the line, advance the column
            if p.column < self.src[p.line].len() - 1 {
                Position::new(p.line, p.column + 1)

            // If we are at the end of the line, advance the line
            } else {
                Position::new(p.line + 1, 0)
            }
        } else {
            // Simply advance off the end
            Position::new(p.line, p.column + 1)
        }
    }

    /// Get the current position
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

    /// Advance the position by one character
    pub(crate) fn set_pos<T: Into<Position>>(&mut self, position: T) {
        if self.src.len() > 0 {
            let p = position.into();
            self.pos = p;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::super::pos;
    use super::*;

    #[test]
    fn test_empty_source() {
        let mut source = Source::new("");
        assert_eq!(source.get((1, 5)), None);
        assert_eq!(source.str((1, 5)), None);
        assert_eq!(source.prev(), None);
        source.set_pos((1, 1));
        assert_eq!(source.get_pos(), pos(0, 0));
        source.adv();
        assert_eq!(source.get_pos(), pos(0, 0));
    }

    #[test]
    fn test_str() {
        let mut source = Source::new("Hello\nFoo\n");
        assert_eq!(source.str((0, 5)), Some("Hello".to_string()));
        assert_eq!(source.str((1, 5)), Some("Hello\nFoo\n".to_string()));

        // None to return
        source.set_pos((1, 4));
        assert_eq!(source.str((1, 5)), None);
    }

    #[test]
    fn test_adv_pos() {
        let source = Source::new("Hello\nFoo\n");
        assert_eq!(source.adv_pos((0, 4)), pos(0, 5));
        assert_eq!(source.adv_pos((0, 5)), pos(1, 0));
        assert_eq!(source.adv_pos((1, 3)), pos(1, 4));
    }

    #[test]
    fn test_adv() {
        let mut source = Source::new("Hello, World!\nThis is a test.\n");
        source.set_pos((0, 12));

        source.adv();
        assert_eq!(source.get_pos(), pos(0, 13));
        assert_eq!(source.curr(), Some(&'\n'));

        source.adv();
        assert_eq!(source.get_pos(), pos(1, 0));
        assert_eq!(source.curr(), Some(&'T'));

        source.set_pos((1, 15));
        source.adv();
        assert_eq!(source.get_pos(), pos(1, 15));
        assert_eq!(source.curr(), Some(&'\n'));
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
}
