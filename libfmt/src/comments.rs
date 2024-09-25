use std::collections::HashMap;

use proc_macro2::{LineColumn, Span, TokenStream, TokenTree};
use tracing::trace;

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Comment {
    Empty(String),       // Empty line ending in a newline
    Line(String),        // Single line comment noted by the `//` prefix
    BlockLine(String),   // Block comment noted by the `/*` prefix and `*/` suffix
    BlockStart(String),  // Block comment noted by the `/*` start
    BlockMiddle(String), // Whatever is between the block start and end
    BlockEnd(String),    // Block comment noted by the `*/` end
    Inner(String),       // Inner block comment noted by the `///` prefix
    Outer(String),       // Outer block comment noted by the `//!` prefix
    Unknown(String),     // Non-conformant text
}

impl Comment {
    /// Check if the comment is a block start
    pub(crate) fn is_block_start(&self) -> bool {
        match self {
            Self::BlockStart(_) => true,
            _ => false,
        }
    }

    /// Check if the comment is an empty line
    pub(crate) fn is_empty_line(&self) -> bool {
        match self {
            Self::Empty(_) => true,
            _ => false,
        }
    }

    /// Get the text of the comment
    pub(crate) fn text(&self) -> String {
        match self {
            Self::Empty(text) => text.clone(),
            Self::Line(text) => text.clone(),
            Self::BlockLine(text) => text.clone(),
            Self::BlockStart(text) => text.clone(),
            Self::BlockMiddle(text) => text.clone(),
            Self::BlockEnd(text) => text.clone(),
            Self::Inner(text) => text.clone(),
            Self::Outer(text) => text.clone(),
            Self::Unknown(text) => text.clone(),
        }
    }
}

// IMplement display trait for Comment
impl std::fmt::Display for Comment {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Empty(text) => write!(f, "Comment::Empty({})", text.escape_default()),
            Self::Line(text) => write!(f, "Comment::Line({})", text.escape_default()),
            Self::BlockLine(text) => write!(f, "Comment::BlockLine({})", text.escape_default()),
            Self::BlockStart(text) => write!(f, "Comment::BlockStart({})", text.escape_default()),
            Self::BlockMiddle(text) => write!(f, "Comment::BlockMiddle({})", text.escape_default()),
            Self::BlockEnd(text) => write!(f, "Comment::BlockEnd({})", text.escape_default()),
            Self::Inner(text) => write!(f, "Comment::Inner({})", text.escape_default()),
            Self::Outer(text) => write!(f, "Comment::Outer({})", text.escape_default()),
            Self::Unknown(text) => write!(f, "Comment::Unknown({})", text.escape_default()),
        }
    }
}

/// Collect comments from the original source using the token stream to provide location
/// information relative the associated tokens. Comments are being defined as any lines
/// of text that were dropped during the conversion into tokens. This can be newlines,
/// regular comments, and inner and outer doc comments.
/// * ***stream***: Token stream to process
/// * ***offset***: Offset into the original source string for tracking comment location
pub(crate) fn pre_process(
    source: &str,
    offset: &mut usize,
    stream: TokenStream,
    collection: &mut HashMap<LineColumn, Vec<Comment>>,
) {
    for token in stream {
        match &token {
            TokenTree::Ident(ident) => {
                println!("{}", debug_span_to_str("Ident:", ident.span()));
                parse(source, offset, collection, ident.span())
            }
            TokenTree::Literal(literal) => {
                println!("{}", debug_span_to_str("Literal:", literal.span()));
                parse(source, offset, collection, literal.span())
            }
            TokenTree::Group(group) => {
                // Start group
                let open = group.span_open();
                println!("{}", debug_span_to_str("Group:", open));
                parse(source, offset, collection, open);

                // Recurse
                pre_process(source, offset, group.stream(), collection);

                // End group after recursion
                let close = group.span_close();
                println!("{}", debug_span_to_str("Group:", close));
                parse(source, offset, collection, close)
            }
            TokenTree::Punct(punct) => {
                println!("{}", debug_span_to_str("Punct:", punct.span()));
                parse(source, offset, collection, punct.span())
            }
        }
    }
}

fn debug_span_to_str(name: &str, span: Span) -> String {
    let range = span.byte_range();
    let start = span.start();
    let end = span.end();
    let mut out = String::new();

    // Create the string for the span
    out.push_str(&format!(
        "{: <8} {: <7} {: <7} {: <7} ({})",
        name,
        format!("{}..{}", start.line, end.line),
        format!("{}..{}", start.column, end.column),
        format!("{:?}", range),
        span.source_text().unwrap_or("<None>".into()),
    ));
    out
}

// Determine if the given span has an associated comment and if so
// store it. In either case advance the offset to account for the span.
fn parse(
    source: &str,
    offset: &mut usize,
    comments: &mut HashMap<LineColumn, Vec<Comment>>,
    span: Span,
) {
    let range = span.byte_range();

    // Comments only potentially exist if offset is not equal to the start of the range
    if range.start > *offset {
        let mut skip = false;

        // A single newline or space is an expected whitespace value for typical code and should
        // not be considered a comment to store.
        if range.start - *offset == 1
            && (source.chars().nth(*offset).map(|x| x == '\n' || x == ' ') == Some(true))
        {
            skip = true;
        }

        // Check for comments
        if !skip {
            if let Some(cmts) = from_str(&source[*offset..range.start]) {
                for comment in &cmts {
                    trace!("{}", comment);
                }
                comments.insert(span.start(), cmts);
            }
        }
    };

    // Update the offset to the end of the token
    *offset += range.end - *offset;
}

/// Parse comments from the given string
fn from_str(text: &str) -> Option<Vec<Comment>> {
    let mut comments: Vec<Comment> = vec![]; // final results
    let mut line = String::new(); // temp buffer

    // Track throughout
    let mut prev_empty_line = false;
    let mut comment_block_starts = 0;
    let mut comment_block_ends = 0;

    // Reset on each newline
    let mut comment_block_start = false;
    let mut comment_block_end = false;
    let mut comment_line = false;
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

            // Only allow a single empty line consecutively regardless of context
            if empty {
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

            comments.push(
                // Everything is included in a block comment until the end
                if comment_block_starts > comment_block_ends {
                    if comment_block_start {
                        Comment::BlockStart(line)
                    } else {
                        Comment::BlockMiddle(line)
                    }
                } else {
                    if empty {
                        Comment::Empty("\n".to_string())
                    } else if comment_line {
                        Comment::Line(line)
                    } else if comment_block_start && comment_block_end {
                        Comment::BlockLine(line)
                    } else if comment_block_end {
                        Comment::BlockEnd(line)
                    } else if comment_inner {
                        Comment::Inner(line)
                    } else if comment_outer {
                        Comment::Outer(line)
                    } else {
                        Comment::Unknown(line)
                    }
                },
            );

            // reset
            empty = true;
            comment_line = false;
            comment_block_start = false;
            comment_block_end = false;
            comment_inner = false;
            comment_outer = false;
            line = String::new();
            prev_char = '\0';
            continue;
        } else if char != ' ' {
            empty = false;
            prev_empty_line = false;

            // Check for public comment types
            let mut pub_comment = false;
            if let Some(next_char) = iter.peek() {
                if prev_char == '/' && char == '/' && *next_char == '/' {
                    pub_comment = true;
                    char = iter.next().unwrap();
                    line.push(char);
                    comment_inner = true;
                } else if prev_char == '/' && char == '/' && *next_char == '!' {
                    pub_comment = true;
                    char = iter.next().unwrap();
                    line.push(char);
                    comment_outer = true;
                }
            }

            // Check for other comment types
            if !pub_comment {
                if prev_char == '/' && char == '/' {
                    comment_line = true;
                } else if prev_char == '/' && char == '*' {
                    comment_block_starts += 1;
                    comment_block_start = true;
                } else if prev_char == '*' && char == '/' {
                    comment_block_ends += 1;
                    comment_block_end = true;
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
        } else if comment_block_start && comment_block_end {
            comments.push(Comment::BlockLine(line));
        } else if comment_block_end {
            comments.push(Comment::BlockStart(line));
        } else if comment_block_end {
            comments.push(Comment::BlockEnd(line));
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
    use proc_macro2::TokenStream;
    use std::str::FromStr;
    use tracing_test::traced_test;

    fn pre_process_comments(src: &str, collection: &mut HashMap<LineColumn, Vec<Comment>>) {
        let tokens = TokenStream::from_str(src).unwrap();
        pre_process(src, &mut 0, tokens, collection);
    }

    #[traced_test]
    #[test]
    fn test_demo() {
        let source = indoc! {r#"
            use indoc::indoc;
            use libfmt::Result;
            use tracing::Level;
            use tracing_subscriber::FmtSubscriber;

            fn main() -> Result<()> {
                let subscriber = FmtSubscriber::builder()
                    .with_max_level(Level::TRACE)
                    .finish();
                tracing::subscriber::set_global_default(subscriber).unwrap();

                // Pass in an example
                let path = "examples/dump.rs";
                let formatted = libfmt::format_file(path)?;
                print!("{}", formatted);

                Ok(())
            }
        "#};
        let mut comments: HashMap<LineColumn, Vec<Comment>> = HashMap::new();
        pre_process_comments(source, &mut comments);

        // No comments should be found
        assert_eq!(0, comments.len());
    }

    // #[traced_test]
    #[test]
    fn test_use_shoud_not_produce_comment() {
        let source = indoc! {r#"
            use foo1;
            use foo2;

            use foo3;
        "#};
        let mut comments: HashMap<LineColumn, Vec<Comment>> = HashMap::new();
        pre_process_comments(source, &mut comments);

        // No comments should be found
        assert_eq!(comments.len(), 1);
        assert!(comments
            .values()
            .nth(0)
            .unwrap()
            .first()
            .unwrap()
            .is_empty_line());
    }

    #[test]
    fn test_comment_block_lines() {
        let source = indoc! {r#"
            // Line 1
            /* Block line 1 */
            /* Block line 2 */ other
            other
        "#};
        assert_eq!(
            from_str(source).unwrap(),
            vec![
                Comment::Line("// Line 1\n".into()),
                Comment::BlockLine("/* Block line 1 */\n".into()),
                Comment::BlockLine("/* Block line 2 */ other\n".into()),
                Comment::Unknown("other\n".into()),
            ]
        );
    }

    #[test]
    fn test_comment_blocks_include_anything_until_end() {
        let source = indoc! {r#"
            /****

             // Line 1
             * Block
             // Line 2

             ***/
        "#};
        assert_eq!(
            from_str(source).unwrap(),
            vec![
                Comment::BlockStart("/****\n".into()),
                Comment::BlockMiddle("\n".into()),
                Comment::BlockMiddle(" // Line 1\n".into()),
                Comment::BlockMiddle(" * Block\n".into()),
                Comment::BlockMiddle(" // Line 2\n".into()),
                Comment::BlockMiddle("\n".into()),
                Comment::BlockEnd(" ***/\n".into()),
            ]
        );
    }

    #[test]
    fn test_non_comment_lines_should_be_captured_as_well() {
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
    fn test_mixing_comment_line_and_inner_and_outer_success() {
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
    fn test_mixing_comment_line_and_inner_success() {
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
    fn test_simple_comment_line_success() {
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
