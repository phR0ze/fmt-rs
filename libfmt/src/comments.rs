use crate::model::{CharExt, Position, Source};
use proc_macro2::{Group, LineColumn, Span, TokenStream, TokenTree};
use std::collections::HashMap;
use tracing::{span, trace};

/// Encapsulate the different types of comments that can be found in the source
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

    /// Get the raw text of the comment
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

/// Collect comments from the original source using the token stream to provide position information
/// relative the associated tokens. Comments are being defined as any lines of text that were
/// dropped during the conversion into tokens that is not going to be added later by the pretty
/// printer. This consists of newlines, regular comments, and
/// inner and outer doc comments.
///
/// * ***source***: Original source
/// * ***tokens***: Tokenized version of the source to process
/// * ***collection***: Resulting collection of comments
pub(crate) fn pre_process(
    source: &str,
    tokens: TokenStream,
    comments: &mut HashMap<Position, Vec<Comment>>,
) -> TokenStream {
    parse_tokens(&mut Source::new(source), tokens, comments)
}

/// Recursively parses tokens to find comments and store them in the collection
///
/// * ***source***: Source character matrix
/// * ***tokens***: Tokenized version of the source to process
/// * ***collection***: Resulting collection of comments
fn parse_tokens(
    source: &mut Source,
    tokens: TokenStream,
    comments: &mut HashMap<Position, Vec<Comment>>,
) -> TokenStream {
    // By filtering the tokens we can drop the Punct doc comments to avoid double counting
    // during the pretty printing of the comments.
    let mut filtered: Vec<TokenTree> = vec![];

    let mut tokens = tokens.into_iter().peekable();
    while let Some(token) = tokens.next() {
        match &token {
            TokenTree::Ident(ident) => {
                trace!("{}", span_to_str("Ident:", ident.span()));
                parse_comments(source, comments, &token);
                filtered.push(token);
            }
            TokenTree::Literal(literal) => {
                trace!("{}", span_to_str("Literal:", literal.span()));
                parse_comments(source, comments, &token);
                filtered.push(token);
            }
            TokenTree::Group(group) => {
                // Start group
                let open = group.span_open();
                trace!("{}", span_to_str("Group:", open));
                parse_comments(source, comments, &token);

                // Recurse
                let tokens = parse_tokens(source, group.stream(), comments);

                // End group after recursion
                let close = group.span_close();
                trace!("{}", span_to_str("Group:", close));
                parse_comments(source, comments, &token);

                let mut _group = Group::new(group.delimiter(), tokens);
                _group.set_span(group.span());
                filtered.push(TokenTree::Group(_group));
            }
            TokenTree::Punct(punct) => {
                trace!("{}", span_to_str("Punct:", punct.span()));

                // proc_macro2 recognizes doc comments and stores them as attributes. Punct doc
                // attributes span the comment starting with '/'; so we can safely skip them as
                // we will be manually parsing out the comments later.
                if punct.as_char() == '#' && source.curr_is(punct.span().start(), '/') {
                    while tokens
                        .peek()
                        .filter(|x| x.span().start() < punct.span().end())
                        .is_some()
                    {
                        tokens.next();
                    }
                }

                parse_comments(source, comments, &token);
                filtered.push(token);
            }
        }
    }

    TokenStream::from_iter(filtered)
}

/// Convert the span into a string for debugging purposes
fn span_to_str(name: &str, span: Span) -> String {
    let start: Position = span.start().into();
    let end: Position = span.end().into();
    let mut out = String::new();

    // Create the string for the span
    out.push_str(&format!(
        "{: <8} {: <10} ({})",
        name,
        format!("{}..{}", start, end),
        span.source_text().unwrap_or("<None>".into()),
    ));
    out
}

/// Determine if the given token span has an associated comment and if so store it. In either case
/// advance the offset to account for the span.
///
/// * ***source***: Character matrix of source string
/// * ***comments***: Collection of comments
/// * ***span***: Span to process
fn parse_comments(
    source: &mut Source,
    comments: &mut HashMap<Position, Vec<Comment>>,
    token: &TokenTree,
) {
    let start: Position = token.span_open().start().into();
    let end: Position = token.span_close().end().into();

    // Comments only potentially exist if there are un-accounted for characters in the source at the
    // the given span is skipping over.
    if start > source.get_pos() {
        // All whitespace between code and code delimiter should be ignored
        if source.prev().is_not_whitespace() {
            while source.curr().is_whitespace() {
                source.adv_one();
            }
        }

        // Extract comments from the string
        let mut extract_comments = |str: String| {
            if !str.is_empty() {
                if let Some(_comments) = from_str(&str) {
                    for comment in &_comments {
                        trace!("{}", comment);
                    }
                    comments.insert(Position::from(start), _comments);
                }
            }
        };
        if let Some(str) = source.str(start) {
            extract_comments(str);
        }
        if let Some(str) = token.doc() {
            extract_comments(str);
        };
    };

    // Update the offset to the end of the token
    if source.get_pos() <= end {
        source.set_pos(end);
        source.adv_one();
    }
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

/// TokenTree extension methods
trait TokenTreeExt {
    fn doc(&self) -> Option<String>;
    fn span_open(&self) -> Span;
    fn span_close(&self) -> Span;
}

impl TokenTreeExt for &TokenTree {
    fn doc(&self) -> Option<String> {
        match self {
            TokenTree::Punct(punct) => {
                let str = punct.span().source_text().unwrap_or("<None>".into());
                if punct.as_char() == '#' && str.chars().next() == Some('/') {
                    Some(str)
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    fn span_open(&self) -> Span {
        match self {
            TokenTree::Group(group) => group.span_open(),
            _ => self.span(),
        }
    }

    fn span_close(&self) -> Span {
        match self {
            TokenTree::Group(group) => group.span_close(),
            _ => self.span(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::pos;
    use indoc::indoc;
    use proc_macro2::TokenStream;
    use std::str::FromStr;
    use tracing_test::traced_test;

    fn pre_process_comments(src: &str, collection: &mut HashMap<Position, Vec<Comment>>) {
        let tokens = TokenStream::from_str(src).unwrap();
        pre_process(src, tokens, collection);
    }

    // #[traced_test]
    // #[test]
    // fn test_doc_comments() {
    //     let source = indoc! {r#"

    //          /// A foo
    //         struct Foo {

    //             /// Field
    //             field: i32,
    //         }
    //     "#};
    //     let mut comments: HashMap<Position, Vec<Comment>> = HashMap::new();
    //     pre_process_comments(source, &mut comments);

    //     assert_eq!(comments.len(), 2);

    //     let comments1 = &comments[&pos(2, 0)];
    //     // assert_eq!(comments1.len(), 1);
    //     // assert_eq!(comments1[0].is_empty_line(), true);

    //     // let comments2 = &comments[&LineColumn { line: 5, column: 0 }];
    //     // assert_eq!(comments2.len(), 1);
    //     // assert_eq!(comments2[0].is_empty_line(), true);
    // }

    // #[test]
    // fn test_multi_comment_types() {
    //     let source = indoc! {r#"
    //         use indoc::indoc;

    //         fn main() -> Result<()> {
    //             let subscriber = FmtSubscriber::builder()
    //                 .with_max_level(Level::TRACE)
    //                 .finish();
    //             tracing::subscriber::set_global_default(subscriber).unwrap();

    //             // Pass in an example
    //             let path = "examples/dump.rs";

    //             Ok(())
    //         }
    //     "#};
    //     let mut comments: HashMap<Position, Vec<Comment>> = HashMap::new();
    //     pre_process_comments(source, &mut comments);

    //     assert_eq!(comments.len(), 4);

    //     // Validate the comments
    //     // let comments1 = &comments[&pos(6, 0)];
    //     // assert_eq!(comments1.len(), 1);
    //     // assert_eq!(comments1[0].is_empty_line(), true);

    //     // let comments2 = &comments[&pos(13, 4)];
    //     // assert_eq!(comments2.len(), 2);
    //     // assert_eq!(comments2[0].is_empty_line(), true);
    //     // assert_eq!(
    //     //     comments2[1].text(),
    //     //     "    // Pass in an example\n".to_string()
    //     // );

    //     // let comments3 = &comments[&pos(17, 4)];
    //     // assert_eq!(comments3.len(), 1);
    //     // assert_eq!(comments3[0].is_empty_line(), true);
    // }

    #[traced_test]
    #[test]
    fn test_empty_lines() {
        let source = indoc! {r#"

            use
            foo1;
            use foo2;

            use foo3;
        "#};
        let mut comments: HashMap<Position, Vec<Comment>> = HashMap::new();
        pre_process_comments(source, &mut comments);

        assert_eq!(comments.len(), 2);

        let comments1 = &comments[&pos(1, 0)];
        assert_eq!(comments1.len(), 1);
        assert_eq!(comments1[0].is_empty_line(), true);

        let comments2 = &comments[&pos(5, 0)];
        assert_eq!(comments2.len(), 1);
        assert_eq!(comments2[0].is_empty_line(), true);
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

    #[test]
    fn test_tokenstream_counts_starting_from_1() {
        // indoc doesn't count the first line unless there is text there
        let source1 = indoc! {r#"
            1
            2
        "#};
        let source2 = indoc! {r#"1
            2
        "#};
        assert_eq!(source1, source2);

        // Tokenization starts counting at 1,0 not 0,0
        let tokens = TokenStream::from_str(source1).unwrap();

        // Check that there are two tokens
        assert_eq!(tokens.clone().into_iter().count(), 2);

        // Check that the tokens are both Ident
        let mut iter = tokens.into_iter();
        let pos1: Position = iter.next().unwrap().span().start().into();
        let pos2: Position = iter.next().unwrap().span().start().into();
        assert_eq!(pos1, pos(0, 0));
        assert_eq!(pos2, pos(1, 0));
    }
}
