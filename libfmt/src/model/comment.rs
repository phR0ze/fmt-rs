use crate::Error;
use std::str::FromStr;

/// Encapsulate the different types of comments that can be found in the source
#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Comment {
    Empty,                // Empty line ending in a newline
    Line(String),         // Single line comment noted by the `//` prefix
    LineTrailing(String), // Single line comment noted by the `//` prefix that is trailing code
    Block(String),        // Block comment noted by the `/*` prefix and `*/` suffix
    BlockInline(String), // Block comment noted by the `/*` prefix and `*/` suffix without a newline
}

impl Comment {
    /// Return the comment type as an attribute name
    pub(crate) fn attr_name(&self) -> String {
        match self {
            Self::Empty => "comment_empty".to_string(),
            Self::Line(_) => "comment_line".to_string(),
            Self::LineTrailing(_) => "comment_line_trailing".to_string(),
            Self::Block(_) => "comment_block".to_string(),
            Self::BlockInline(_) => "comment_block_inline".to_string(),
        }
    }

    /// Get the raw text of the comment
    pub(crate) fn text(&self) -> String {
        match self {
            Self::Empty => "".to_string(),
            Self::Line(text) => text.clone(),
            Self::LineTrailing(text) => text.clone(),
            Self::Block(text) => text.clone(),
            Self::BlockInline(text) => text.clone(),
        }
    }

    /// Check if the comment is trailing code
    pub(crate) fn is_trailing(&self) -> bool {
        match self {
            Self::LineTrailing(_) => true,
            _ => false,
        }
    }
}

impl FromStr for Comment {
    type Err = Error;

    fn from_str(s: &str) -> core::result::Result<Self, Self::Err> {
        if let Some((first, second)) = s.split_once(':') {
            match first.to_lowercase().as_str() {
                "comment_empty" => Ok(Comment::Empty),
                "comment_line" => Ok(Comment::Line(second.to_string())),
                "comment_line_trailing" => Ok(Comment::LineTrailing(second.to_string())),
                "comment_block" => Ok(Comment::Block(second.to_string())),
                "comment_block_inline" => Ok(Comment::BlockInline(second.to_string())),
                _ => Err(Error::new("failed to parse comment")),
            }
        } else {
            Err(Error::new("failed to parse comment"))
        }
    }
}

impl std::fmt::Display for Comment {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Empty => write!(f, "Comment::Empty"),
            Self::Line(text) => write!(f, "Comment::Line({})", text.escape_default()),
            Self::LineTrailing(text) => {
                write!(f, "Comment::LineTrailing({})", text.escape_default())
            }
            Self::Block(text) => write!(f, "Comment::Block({})", text.escape_default()),
            Self::BlockInline(text) => write!(f, "Comment::BlockInline({})", text.escape_default()),
        }
    }
}
