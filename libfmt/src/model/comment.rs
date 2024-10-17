use crate::Error;
use std::str::FromStr;

/// Encapsulate the different types of comments that can be found in the source
#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Comment {
    Break,
    Empty,
    Line(String),
    LineTrailing(String),
    Block(String),
    BlockInline(String),
}

impl Comment {
    /// Create a new line trailing comment instance
    pub(crate) fn line_trailing(msg: &str) -> Self {
        Self::LineTrailing(msg.to_string())
    }

    /// Create a new block inline comment instance
    pub(crate) fn block_inline(msg: &str) -> Self {
        Self::BlockInline(msg.to_string())
    }

    /// Create a new block comment instance
    pub(crate) fn block(msg: &str) -> Self {
        Self::Block(msg.to_string())
    }

    /// Create a new block comment instance
    pub(crate) fn line(msg: &str) -> Self {
        Self::Line(msg.to_string())
    }

    /// Create a new empty comment instance
    pub(crate) fn empty() -> Self {
        Self::Empty
    }

    /// Create a new code break
    pub(crate) fn code_break() -> Self {
        Self::Break
    }

    /// Return the comment type as an attribute name
    pub(crate) fn attr_name(&self) -> String {
        match self {
            Self::Break => "comment_break".to_string(),
            Self::Empty => "comment_empty".to_string(),
            Self::Line(_) => "comment_line".to_string(),
            Self::LineTrailing(_) => "comment_line_trailing".to_string(),
            Self::Block(_) => "comment_block".to_string(),
            Self::BlockInline(_) => "comment_block_inline".to_string(),
        }
    }

    /// Get the raw text of the comment
    pub(crate) fn text(&self) -> &str {
        match self {
            Self::Break => "",
            Self::Empty => "",
            Self::Line(msg) => &msg,
            Self::LineTrailing(msg) => &msg,
            Self::Block(msg) => &msg,
            Self::BlockInline(msg) => &msg,
        }
    }

    /// Check if the comment is a code break
    pub(crate) fn is_break(&self) -> bool {
        match self {
            Self::Break => true,
            _ => false,
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
                "comment_break" => Ok(Comment::Break),
                "comment_empty" => Ok(Comment::Empty),
                "comment_line" => Ok(Comment::line(second)),
                "comment_line_trailing" => Ok(Comment::line_trailing(second)),
                "comment_block" => Ok(Comment::block(second)),
                "comment_block_inline" => Ok(Comment::block_inline(second)),
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
            Self::Break => write!(f, "Comment::Break"),
            Self::Empty => write!(f, "Comment::Empty"),
            Self::Line(msg) => write!(f, "Comment::Line({})", msg.escape_default()),
            Self::LineTrailing(msg) => {
                write!(f, "Comment::LineTrailing({})", msg.escape_default())
            }
            Self::Block(msg) => write!(f, "Comment::Block({})", msg.escape_default()),
            Self::BlockInline(msg) => {
                write!(f, "Comment::BlockInline({})", msg.escape_default())
            }
        }
    }
}
