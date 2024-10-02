use crate::{
    model::{CharExt, Config, Position, Source},
    Error, Result,
};
use proc_macro2::{Delimiter, Group, Ident, Literal, Punct, Spacing, Span, TokenStream, TokenTree};
use std::str::FromStr;
use tracing::trace;

/// Encapsulate the different types of comments that can be found in the source
#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Comment {
    Empty,         // Empty line ending in a newline
    Line(String),  // Single line comment noted by the `//` prefix
    Block(String), // Block comment noted by the `/*` prefix and `*/` suffix
    Unknown,       // Comment that should be ignored
}

impl Comment {
    /// Check if the comment is a block comment
    pub(crate) fn is_block(&self) -> bool {
        match self {
            Self::Block(_) => true,
            _ => false,
        }
    }

    /// Check if the comment is a line comment
    pub(crate) fn is_line(&self) -> bool {
        match self {
            Self::Line(_) => true,
            _ => false,
        }
    }

    /// Check if the comment is an empty line
    pub(crate) fn is_empty(&self) -> bool {
        match self {
            Self::Empty => true,
            _ => false,
        }
    }

    /// Return the comment type as an attribute name
    pub(crate) fn attr_name(&self) -> String {
        match self {
            Self::Empty => "comment_empty".to_string(),
            Self::Line(_) => "comment_line".to_string(),
            Self::Block(_) => "comment_block".to_string(),
            Self::Unknown => "comment_unknown".to_string(),
        }
    }

    /// Get the raw text of the comment
    pub(crate) fn text(&self) -> String {
        match self {
            Self::Empty => "".to_string(),
            Self::Line(text) => text.clone(),
            Self::Block(text) => text.clone(),
            Self::Unknown => "".to_string(),
        }
    }
}

impl std::fmt::Display for Comment {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Empty => write!(f, "Comment::Empty"),
            Self::Line(text) => write!(f, "Comment::Line({})", text.escape_default()),
            Self::Block(text) => write!(f, "Comment::BlockLine({})", text.escape_default()),
            Self::Unknown => write!(f, "Comment::Unknown"),
        }
    }
}

/// Inject missing comments into the token stream.
///
/// ### Background
/// proc_macro2 recognizes doc comments and stores them as attributes but does not recognize regular
/// comments and skips them only providing a partial solution. However we can parse the resulting
/// tokens and inject the missing comments as a made up comment attribute that will then be
/// recognized by the pretty printer.
///
/// * ***config***: Configuration settings
/// * ***source***: Original source
/// * ***return***: Tokenized version of the source with injected comment tokens
pub(crate) fn inject(config: &Config, source: &str) -> Result<TokenStream> {
    // Parse source into token stream
    let tokens = TokenStream::from_str(source)
        .map_err(|e| Error::new("failed to parse source into token stream").wrap_lex(e))?;

    // Only inject comments if they are enabled
    Ok(match config.no_comments() {
        true => tokens,
        _ => inject_tokens(&mut Source::new(source), tokens),
    })
}

/// Recursively inject tokens as needed to include missing comments
///
/// * ***source***: Source character matrix
/// * ***return***: Tokenized version of the source with injected comment tokens
fn inject_tokens(source: &mut Source, tokens: TokenStream) -> TokenStream {
    let mut result: Vec<TokenTree> = vec![];

    let mut tokens = tokens.into_iter().peekable();
    while let Some(token) = tokens.next() {
        match &token {
            TokenTree::Ident(ident) => {
                let span = ident.span();
                inject_comments(source, &mut result, &token, &span);
                trace!("{}", token_to_str(&token, &span));
                result.push(token);
            }
            TokenTree::Literal(literal) => {
                let span = literal.span();
                inject_comments(source, &mut result, &token, &span);
                trace!("{}", token_to_str(&token, &span));
                result.push(token);
            }
            TokenTree::Group(group) => {
                // Start group
                let open = group.span_open();
                inject_comments(source, &mut result, &token, &open);
                trace!("{}", token_to_str(&token, &open));

                // Recurse
                let tokens = inject_tokens(source, group.stream());

                // End group after recursion
                let close = group.span_close();
                inject_comments(source, &mut result, &token, &close);
                trace!("{}", token_to_str(&token, &close));

                let mut _group = Group::new(group.delimiter(), tokens);
                _group.set_span(group.span());
                result.push(TokenTree::Group(_group));
            }
            TokenTree::Punct(punct) => {
                let span = punct.span();
                inject_comments(source, &mut result, &token, &span);
                trace!("{}", token_to_str(&token, &span));

                // proc_macro2 recognizes doc comments and stores them as attributes so we can
                // safely ignore them as they will be handled by the pretty printer already.
                if punct.as_char() == '#' && source.curr_is(punct.span().start(), '/') {
                    result.push(token.clone());
                    while tokens
                        .peek()
                        .filter(|x| x.span().start() < punct.span().end())
                        .is_some()
                    {
                        // just pass these through to the token stream and don't need to process
                        // for possible comments as they are already being correctly handled.
                        if let Some(token) = tokens.next() {
                            trace!("{}", token_to_str(&token, &token.span()));
                            result.push(token);
                        }
                    }
                } else {
                    result.push(token.clone());
                }
            }
        }
    }

    TokenStream::from_iter(result)
}

/// Convert the span into a string for debugging purposes
fn token_to_str(token: &TokenTree, span: &Span) -> String {
    let start: Position = span.start().into();
    let end: Position = span.end().into();
    let mut out = String::new();

    // Create the string for the span
    out.push_str(&format!(
        "{: <8} {: <10} ({})",
        token.name(),
        format!("{}..{}", start, end),
        span.source_text().unwrap_or("<None>".into()),
    ));
    out
}

/// Determine if the given token span has an associated comment and if so store it. In either case
/// advance the offset to account for the span.
///
/// * ***source***: Character matrix of source string
/// * ***tokens***: Token stream to inject comments into
/// * ***token***: Current token that is being processed
/// * ***span***: Span to process
fn inject_comments(source: &mut Source, tokens: &mut Vec<TokenTree>, _: &TokenTree, span: &Span) {
    let start: Position = span.start().into();
    let end: Position = span.end().into();

    // Comments only potentially exist if there are un-accounted for characters in the source at the
    // the given span is skipping over.
    if start > source.get_pos() {
        // All whitespace between code and code delimiter should be ignored
        if source.prev().is_not_whitespace() {
            while source.curr().is_whitespace() {
                source.adv_one();
            }
        }

        // Extract comments from the unaccounted for characters before this span
        if let Some(str) = source.str(start) {
            if !str.is_empty() {
                if let Some(_comments) = from_str(&str) {
                    for comment in &_comments {
                        let punct = '#';
                        let msg = comment.text();
                        let attr_name = comment.attr_name();
                        trace!("Comment: {}[{} = \"{}\"]", punct, attr_name, msg);

                        // Ignoring spans for now
                        tokens.push(Punct::new(punct, Spacing::Alone).into());
                        tokens.push(
                            Group::new(
                                Delimiter::Bracket,
                                TokenStream::from_iter::<Vec<TokenTree>>(vec![
                                    Ident::new(&attr_name, Span::call_site()).into(),
                                    Punct::new('=', Spacing::Alone).into(),
                                    Literal::string(&msg).into(),
                                ]),
                            )
                            .into(),
                        );
                    }
                }
            }
        }
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
    let mut comment_block = false;

    // Reset on each newline
    let mut comment_line = false;
    let mut empty = true;
    let mut prev_char = '\0';

    // Reset the tracking variables
    let reset =
        |line: &mut String, empty: &mut bool, comment_line: &mut bool, prev_char: &mut char| {
            line.clear();
            *empty = true;
            *comment_line = false;
            *prev_char = '\0';
        };

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

            // Block comment has not be closed yet, so everything is part of it
            if comment_block {
                continue;
            }

            // Store the comment
            let comment = if empty {
                line.pop(); // drop newline
                Comment::Empty
            } else if comment_line {
                line.pop(); // drop newline
                Comment::Line(line.clone())
            } else {
                Comment::Unknown
            };

            // Filter out unknown comments
            if comment != Comment::Unknown {
                comments.push(comment);
            }

            reset(&mut line, &mut empty, &mut comment_line, &mut prev_char);
            continue;
        } else if char != ' ' {
            empty = false;
            prev_empty_line = false;

            // Block comments encompass everything until the end
            if comment_block {
                if prev_char == '*' && char == '/' {
                    comment_block = false; // Reset block checking
                    line.truncate(line.len() - 2); // Drop control characters

                    // Consume any trailing newline before its added to the comment
                    if let Some(c) = iter.next_if_eq(&'\n') {
                        char = c; // Keeps the prev_char accurate
                    }

                    comments.push(Comment::Block(line.clone()));
                    reset(&mut line, &mut empty, &mut comment_line, &mut prev_char);
                }
            } else {
                // Check for doc comments first
                let mut doc_comment = false;
                if let Some(next_char) = iter.peek() {
                    if prev_char == '/' && char == '/' && (*next_char == '/' || *next_char == '!') {
                        line.clear(); // start storing the comment
                        doc_comment = true;
                        char = iter.next().unwrap();
                    }
                }

                // Fall back on regular comments
                if !doc_comment {
                    if prev_char == '/' && (char == '/' || char == '*') {
                        line.clear(); // start storing the comment
                        if char == '/' {
                            comment_line = true;
                        } else {
                            comment_block = true;
                        }
                    }
                }
            }
        }

        // Track the previous character
        prev_char = char;
    }

    match comments.is_empty() {
        true => None,
        _ => Some(comments),
    }
}

/// Extension trait for printing token types to string
trait TokenExt {
    fn name(&self) -> String;
}

impl TokenExt for TokenTree {
    fn name(&self) -> String {
        match self {
            TokenTree::Ident(_) => "Ident".to_string(),
            TokenTree::Punct(_) => "Punct".to_string(),
            TokenTree::Literal(_) => "Literal".to_string(),
            TokenTree::Group(_) => "Group".to_string(),
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
    // use tracing_test::traced_test;

    #[test]
    fn test_multi_comment_types() {
        let source = indoc! {r#"
            use indoc::indoc;

            fn main() -> Result<()> {
                let subscriber = FmtSubscriber::builder()
                    .with_max_level(Level::TRACE)
                    .finish();
                tracing::subscriber::set_global_default(subscriber).unwrap();

                // Pass in an example
                let path = "examples/dump.rs";

                Ok(())
            }
        "#};
        assert_eq!(
            from_str(source).unwrap(),
            vec![
                Comment::Empty,
                Comment::Empty,
                Comment::Line(" Pass in an example".to_string()),
                Comment::Empty,
            ]
        );
    }

    #[traced_test]
    #[test]
    fn test_skip_doc_comments() {
        let source = indoc! {r#"

             /// A foo struct
            struct Foo {

                //     Indented comment
                a: i32,

                // Field b
                b: i32,
            }
        "#};
        assert_eq!(
            from_str(source).unwrap(),
            vec![
                Comment::Empty,
                Comment::Empty,
                Comment::Line("     Indented comment".into()),
                Comment::Empty,
                Comment::Line(" Field b".into()),
            ]
        );

        // Check the tokens that were generated
        let tokens = inject(&Config::default(), source).unwrap().into_iter();

        // (2) punct at the top level
        assert_eq!(
            tokens
                .clone()
                .filter(|x| {
                    if let TokenTree::Punct(punct) = x {
                        return punct.as_char() == '#';
                    }
                    false
                })
                .count(),
            2
        );

        // (2) punct inside the group
        let token = tokens.skip(6).next().unwrap();
        assert!(matches!(token, TokenTree::Group(_)));
        if let TokenTree::Group(group) = token {
            let mut stream = group.stream().into_iter();
            assert_eq!(
                stream
                    .by_ref()
                    .filter(|x| {
                        if let TokenTree::Punct(punct) = x {
                            return punct.as_char() == '#';
                        }
                        false
                    })
                    .count(),
                4
            );
        }
    }

    #[test]
    fn test_inject_tokens() {
        let source = indoc! {r#"
            // A foo struct
            struct Foo;
        "#};
        let mut tokens = inject(&Config::default(), source).unwrap().into_iter();

        // 1. Punct(#)
        let token = tokens.next().unwrap();
        assert!(matches!(token, TokenTree::Punct(_)));
        if let TokenTree::Punct(punct) = token {
            assert_eq!(punct.as_char(), '#');
        }

        // 2. Group: bracket, token stream
        let token = tokens.next().unwrap();
        assert!(matches!(token, TokenTree::Group(_)));
        let group = if let TokenTree::Group(group) = token {
            assert_eq!(group.delimiter(), Delimiter::Bracket);
            group
        } else {
            panic!("Expected a group token");
        };

        // Examine the stream
        let mut stream = group.stream().into_iter();
        let token = stream.next().unwrap();

        // 3. Ident(comment_line)
        assert!(matches!(token, TokenTree::Ident(_)));
        if let TokenTree::Ident(ident) = token {
            assert_eq!(
                format!("{:?}", ident),
                "Ident { sym: comment_line }".to_string()
            );
        }

        // 4. Punct(=)
        let token = stream.next().unwrap();
        assert!(matches!(token, TokenTree::Punct(_)));
        if let TokenTree::Punct(punct) = token {
            assert_eq!(punct.as_char(), '=');
        }

        // 5. Literal(=)
        let token = stream.next().unwrap();
        assert!(matches!(token, TokenTree::Literal(_)));
        if let TokenTree::Literal(literal) = token {
            assert_eq!(
                format!("{:?}", literal),
                "Literal { lit: \" A foo struct\" }".to_string()
            );
        }
    }

    #[test]
    fn test_comment_line_and_block_lines() {
        let source = indoc! {r#"
            // Line 1
            /* Block line 1 */
            /* Block line 2 */ other
            other
        "#};
        assert_eq!(
            from_str(source).unwrap(),
            vec![
                Comment::Line(" Line 1".into()),
                Comment::Block(" Block line 1 ".into()),
                Comment::Block(" Block line 2 ".into()),
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
            vec![Comment::Block(
                "***\n\n // Line 1\n * Block\n // Line 2\n\n **".into()
            ),]
        );
    }

    #[test]
    fn test_non_comment_lines_should_not_be_captured() {
        let source = indoc! {r#"
            foo
        "#};
        assert!(from_str(source).is_none());
    }

    #[test]
    fn test_mixing_comment_line_and_inner_and_outer_success() {
        let source = indoc! {r#"

            // Line

            /// Outer

            //! Inner
        "#};
        assert_eq!(
            from_str(source).unwrap(),
            vec![
                Comment::Empty,
                Comment::Line(" Line".into()),
                Comment::Empty,
                Comment::Empty,
            ]
        );
    }

    #[test]
    fn test_mixing_comment_line_and_outer_success() {
        let source = indoc! {r#"

            // Line

            /// Outer
        "#};
        assert_eq!(
            from_str(source).unwrap(),
            vec![
                Comment::Empty,
                Comment::Line(" Line".into()),
                Comment::Empty,
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
            vec![Comment::Empty, Comment::Line(" Comment".into()),]
        );
    }

    #[test]
    fn test_only_allow_single_empty_line_consecutively() {
        let source = indoc! {r#"


            println!("{}", "1");
        "#};

        // Check the string beeing fed in
        assert_eq!(source, "\n\nprintln!(\"{}\", \"1\");\n");

        // Check the resulting comments parsed out
        assert_eq!(from_str(source).unwrap(), vec![Comment::Empty]);

        // Check the tokens that were generated
        let tokens = inject(&Config::default(), source).unwrap().into_iter();

        assert_eq!(
            tokens
                .filter(|x| {
                    if let TokenTree::Punct(punct) = x {
                        return punct.as_char() == '#';
                    }
                    false
                })
                .count(),
            1
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
