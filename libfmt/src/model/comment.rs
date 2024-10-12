use crate::Error;
use std::str::FromStr;

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum CommentKind {
    Empty,
    Line,
    LineTrailing,
    Block,
    BlockInline,
}

/// Encapsulate the different types of comments that can be found in the source
#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Comment {
    msg: String,       // The comment message
    kind: CommentKind, // The comment kind
}

impl Comment {
    /// Create a new comment instance
    pub(crate) fn new(msg: &str, kind: CommentKind) -> Self {
        Self {
            msg: msg.to_string(),
            kind,
        }
    }

    /// Create a new line trailing comment instance
    pub(crate) fn line_trailing(msg: &str) -> Self {
        Self::new(msg, CommentKind::LineTrailing)
    }

    /// Create a new block inline comment instance
    pub(crate) fn block_inline(msg: &str) -> Self {
        Self::new(msg, CommentKind::BlockInline)
    }

    /// Create a new block comment instance
    pub(crate) fn block(msg: &str) -> Self {
        Self::new(msg, CommentKind::Block)
    }

    /// Create a new block comment instance
    pub(crate) fn line(msg: &str) -> Self {
        Self::new(msg, CommentKind::Line)
    }

    /// Create a new empty comment instance
    pub(crate) fn empty() -> Self {
        Self {
            msg: "".to_string(),
            kind: CommentKind::Empty,
        }
    }

    /// Return the comment type as an attribute name
    pub(crate) fn attr_name(&self) -> String {
        match self.kind {
            CommentKind::Empty => "comment_empty".to_string(),
            CommentKind::Line => "comment_line".to_string(),
            CommentKind::LineTrailing => "comment_line_trailing".to_string(),
            CommentKind::Block => "comment_block".to_string(),
            CommentKind::BlockInline => "comment_block_inline".to_string(),
        }
    }

    /// Get the raw text of the comment
    pub(crate) fn text(&self) -> String {
        self.msg.clone()
    }

    /// Check if the comment is trailing code
    pub(crate) fn is_trailing(&self) -> bool {
        match self.kind {
            CommentKind::LineTrailing => true,
            _ => false,
        }
    }
}

impl FromStr for Comment {
    type Err = Error;

    fn from_str(s: &str) -> core::result::Result<Self, Self::Err> {
        if let Some((first, second)) = s.split_once(':') {
            match first.to_lowercase().as_str() {
                "comment_empty" => Ok(Comment::new("", CommentKind::Empty)),
                "comment_line" => Ok(Comment::new(second, CommentKind::Line)),
                "comment_line_trailing" => Ok(Comment::new(second, CommentKind::LineTrailing)),
                "comment_block" => Ok(Comment::new(second, CommentKind::Block)),
                "comment_block_inline" => Ok(Comment::new(second, CommentKind::BlockInline)),
                _ => Err(Error::new("failed to parse comment")),
            }
        } else {
            Err(Error::new("failed to parse comment"))
        }
    }
}

impl std::fmt::Display for Comment {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self.kind {
            CommentKind::Empty => write!(f, "Comment::Empty"),
            CommentKind::Line => write!(f, "Comment::Line({})", self.msg.escape_default()),
            CommentKind::LineTrailing => {
                write!(f, "Comment::LineTrailing({})", self.msg.escape_default())
            }
            CommentKind::Block => write!(f, "Comment::Block({})", self.msg.escape_default()),
            CommentKind::BlockInline => {
                write!(f, "Comment::BlockInline({})", self.msg.escape_default())
            }
        }
    }
}
