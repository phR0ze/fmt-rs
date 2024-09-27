use proc_macro2::LineColumn;

/// Convenience function for creating a new Position
pub(crate) fn pos(line: usize, column: usize) -> Position {
    Position::new(line, column)
}

/// Position in the source code.
/// Position is zero indexed meaning 0,0 is the first character in the source code.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub(crate) struct Position {
    pub(crate) line: usize,
    pub(crate) column: usize,
}

impl Position {
    /// Create a new position
    pub(crate) fn new(line: usize, column: usize) -> Self {
        Self { line, column }
    }
}

/// Default Position
impl Default for Position {
    fn default() -> Self {
        Self::new(0, 0)
    }
}

/// From LineColumn to Position conversion
impl From<LineColumn> for Position {
    fn from(lc: LineColumn) -> Self {
        Self {
            // LineColumn line is 1-indexed while Position is 0-indexed
            line: lc.line.checked_sub(1).unwrap_or(0),
            column: lc.column, // LineColumn column is 0-indexed
        }
    }
}

/// From (usize, usize) tuple to Position conversion
impl From<(usize, usize)> for Position {
    fn from((line, column): (usize, usize)) -> Self {
        Self { line, column }
    }
}

/// Display Position as line:column
impl std::fmt::Display for Position {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_order() {
        // Greater than
        assert!(pos(0, 1) > pos(0, 0));
        assert!(pos(1, 0) > pos(0, 5));
        assert!(pos(0, 7) > pos(0, 5));

        // Less than
        assert!(pos(0, 7) < pos(1, 5));
        assert!(pos(1, 7) < pos(1, 8));

        // Equal
        assert!(pos(1, 7) == pos(1, 7));
    }

    #[test]
    fn test_to_string() {
        assert_eq!(pos(1, 1).to_string(), "1:1".to_string());
    }

    #[test]
    fn test_from_linecolumn() {
        // LineColumn is 1-indexed so we need to subtract 1 to convert it to Position
        let pos1: Position = LineColumn { line: 1, column: 0 }.into();
        assert_eq!(pos1, pos(0, 0));
    }
}
