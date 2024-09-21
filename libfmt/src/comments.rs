#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Comment {
    Empty(String),   // Empty line ending in a newline
    Line(String),    // Single line comment noted by the `//` prefix
    Block(String),   // Block comment noted by the `/*` prefix and `*/` suffix
    Inner(String),   // Inner block comment noted by the `///` prefix
    Outer(String),   // Outer block comment noted by the `//!` prefix
    Unknown(String), // Non-conformant text
}

impl Comment {
    /// Get the text of the comment
    pub(crate) fn text(&self) -> String {
        match self {
            Self::Empty(text) => text.clone(),
            Self::Line(text) => text.clone(),
            Self::Block(text) => text.clone(),
            Self::Inner(text) => text.clone(),
            Self::Outer(text) => text.clone(),
            Self::Unknown(text) => text.clone(),
        }
    }
}

/// Parse comments from the given string
pub(crate) fn from_str<T: Into<String>>(text: T) -> Option<Vec<Comment>> {
    let text: String = text.into();
    let mut comments = vec![]; // final results
    let mut line = String::new(); // temp buffer

    // Track throughout
    let mut prev_empty_line = false;

    // Reset on each newline
    let mut comment_line = false;
    let mut comment_block = false;
    let mut comment_inner = false;
    let mut comment_outer = false;
    let mut empty = true;
    let mut prev_char = '\0';

    let mut iter = text.chars().peekable();
    while let Some(mut char) = iter.next() {
        line.push(char);

        if char == '\n' {
            // Drop carriage returns in favor of newlines
            if prev_char == '\r' {
                line.pop();
            }

            if empty {
                // Only allow a single empty line consecutively
                if prev_empty_line {
                    line.clear();
                    prev_char = '\0';
                    continue;
                } else {
                    prev_empty_line = true;
                }
            } else {
                prev_empty_line = false;
            }

            comments.push(if empty {
                Comment::Empty("\n".to_string())
            } else if comment_line {
                Comment::Line(line)
            } else if comment_block {
                Comment::Block(line)
            } else if comment_inner {
                Comment::Inner(line)
            } else if comment_outer {
                Comment::Outer(line)
            } else {
                Comment::Unknown(line)
            });

            // reset
            empty = true;
            comment_line = false;
            comment_block = false;
            comment_inner = false;
            comment_outer = false;
            line = String::new();
            prev_char = '\0';
            continue;
        } else if char != ' ' {
            empty = false;
            prev_empty_line = false;

            // Check for comments types
            let mut comment = false;
            if let Some(next_char) = iter.peek() {
                if prev_char == '/' && char == '/' && *next_char == '/' {
                    comment = true;
                    char = iter.next().unwrap();
                    line.push(char);
                    comment_inner = true;
                } else if prev_char == '/' && char == '/' && *next_char == '!' {
                    comment = true;
                    char = iter.next().unwrap();
                    line.push(char);
                    comment_outer = true;
                }
            }

            if !comment {
                if prev_char == '/' && char == '/' {
                    comment_line = true;
                } else if prev_char == '/' && char == '*' {
                    comment_block = true;
                }
            }
        }

        // Track the previous character
        prev_char = char;
    }

    // Add any remaining comments that didn't have a newline
    if !line.is_empty() {
        if comment_line {
            comments.push(Comment::Line(line));
        } else if comment_block {
            comments.push(Comment::Block(line));
        } else if comment_inner {
            comments.push(Comment::Inner(line));
        } else if comment_outer {
            comments.push(Comment::Outer(line));

        // Whitespace needs to have more than just spaces to be considered
        // interesting enought to store
        } else if !empty {
            comments.push(Comment::Unknown(line));
        }
    }

    match comments.is_empty() {
        true => None,
        _ => Some(comments),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;

    #[test]
    fn test_comment_block() {
        let source = indoc! {r#"
            /**
             * Block
             */
        "#};
        assert_eq!(
            from_str(source).unwrap(),
            vec![Comment::Block("/**\n * Block\n */\n".into()),]
        );
    }

    #[test]
    fn test_comment_unknown() {
        let source = indoc! {r#"
            foo
        "#};
        assert_eq!(
            from_str(source).unwrap(),
            vec![Comment::Unknown("foo\n".into()),]
        );

        // unknown comment without newline
        assert_eq!(
            from_str("foo").unwrap(),
            vec![Comment::Unknown("foo".into()),]
        );
    }

    #[test]
    fn test_comment_line_and_inner_and_outer() {
        let source = indoc! {r#"

            // Line

            /// Inner

            //! Outer
        "#};
        assert_eq!(
            from_str(source).unwrap(),
            vec![
                Comment::Empty("\n".into()),
                Comment::Line("// Line\n".into()),
                Comment::Empty("\n".into()),
                Comment::Inner("/// Inner\n".into()),
                Comment::Empty("\n".into()),
                Comment::Outer("//! Outer\n".into()),
            ]
        );
    }

    #[test]
    fn test_comment_line_and_inner() {
        let source = indoc! {r#"

            // Line

            /// Inner
        "#};
        assert_eq!(
            from_str(source).unwrap(),
            vec![
                Comment::Empty("\n".into()),
                Comment::Line("// Line\n".into()),
                Comment::Empty("\n".into()),
                Comment::Inner("/// Inner\n".into()),
            ]
        );
    }

    #[test]
    fn test_comment_line() {
        let source = indoc! {r#"

            // Comment
        "#};
        assert_eq!(
            from_str(source).unwrap(),
            vec![
                Comment::Empty("\n".into()),
                Comment::Line("// Comment\n".into()),
            ]
        );
    }

    #[test]
    fn test_only_allow_single_empty_line_consecutively() {
        let source = indoc! {r#"


        "#};
        assert_eq!(source.trim_matches(' '), "\n\n");
        assert_eq!(
            from_str(source).unwrap(),
            vec![Comment::Empty("\n".into()),]
        );
    }
}
