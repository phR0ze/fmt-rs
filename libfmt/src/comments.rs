use crate::{
    model::{Comment, Config, Position, Source},
    Error, Result,
};
use proc_macro2::{Delimiter, Group, Ident, Literal, Punct, Spacing, Span, TokenStream, TokenTree};
use std::str::FromStr;
use tracing::trace;

#[derive(Clone, Debug, PartialEq)]
enum Context {
    Struct,
    Enum,
    None,
}

impl Context {
    /// Convert the string into an item type
    fn from_str(s: &str) -> Self {
        match s.to_lowercase().as_str() {
            "struct" => Context::Struct,
            "enum" => Context::Enum,
            _ => Context::None,
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
        _ => TokenStream::from_iter(inject_tokens(&mut Source::new(source), tokens, None)),
    })
}

/// Recursively inject tokens as needed to include missing comments
///
/// * ***source***: Source character matrix
/// * ***tokens***: Token stream to inject comments into
/// * ***prev***: Previous token to take into account for comment injection
/// * ***return***: Tokenized version of the source with injected comment tokens
fn inject_tokens(
    source: &mut Source,
    tokens: TokenStream,
    prev: Option<TokenTree>,
) -> Vec<TokenTree> {
    let mut result: Vec<TokenTree> = vec![];
    let mut ctx = Context::None;

    // If we have no tokens at all we might still have comments
    if tokens.is_empty() && prev.is_none() {
        inject_comments(source, &mut result, &ctx, None, None, None);
        return result;
    }

    let mut prev = prev;
    let mut tokens = tokens.into_iter().peekable();
    while let Some(token) = tokens.next() {
        let next = tokens.peek();

        match &token {
            TokenTree::Ident(ident) => {
                let span = ident.span();
                inject_comments(source, &mut result, &ctx, Some(&span), next, prev.as_ref());
                trace!("{}", token_to_str(&token, &span));
                result.push(token);
            }
            TokenTree::Literal(literal) => {
                let span = literal.span();
                inject_comments(source, &mut result, &ctx, Some(&span), next, prev.as_ref());
                trace!("{}", token_to_str(&token, &span));
                result.push(token);
            }
            TokenTree::Group(group) => {
                // Need to account for the group's opening control character
                let span = group.span_open();
                inject_comments(source, &mut result, &ctx, Some(&span), next, prev.as_ref());

                // Recurse on group and update previous token to the last one in the group
                trace!("{}", token_to_str(&token, &group.span_open()));
                let mut _tokens = inject_tokens(source, group.stream(), prev.clone());
                prev = _tokens.last().as_deref().cloned();

                // Inject any comments at the end of the group that were not captured
                let span = group.span_close();
                inject_comments(source, &mut _tokens, &ctx, Some(&span), next, prev.as_ref());
                trace!("{}", token_to_str(&token, &group.span_close()));

                // Construct new group to capture group token stream changes
                let mut _group = Group::new(group.delimiter(), TokenStream::from_iter(_tokens));
                _group.set_span(group.span());
                result.push(TokenTree::Group(_group));
            }
            TokenTree::Punct(punct) => {
                let span = punct.span();
                inject_comments(source, &mut result, &ctx, Some(&span), next, prev.as_ref());
                trace!("{}", token_to_str(&token, &span));

                // proc_macro2 recognizes doc comments and stores them as attributes so we can
                // simply pass them through without processing.
                if punct.as_char() == '#' && source.char_at_is(punct.span().start(), '/') {
                    result.push(token.clone());
                    while tokens
                        .peek()
                        .filter(|x| x.span().start() < punct.span().end())
                        .is_some()
                    {
                        if let Some(token) = tokens.next() {
                            trace!("{}", token_to_str(&token, &token.span()));
                            result.push(token);
                        }
                    }
                } else {
                    result.push(token);
                }
            }
        }
        prev = result.last().as_deref().cloned();
    }

    result
}

/// Determine if the given token span has an associated comment and if so store it. In either case
/// advance the offset to account for the span.
///
/// * ***source***: Character matrix of source string
/// * ***tokens***: Token stream to inject comments into
/// * ***context***: Token context were in
/// * ***curr_span***: Current span to process or None if there are no tokens
/// * ***next_token***: Next token to take into account for comment injection
/// * ***prev_token***: Previous token to take into account for comment injection
fn inject_comments(
    source: &mut Source,
    tokens: &mut Vec<TokenTree>,
    context: &Context,
    curr_span: Option<&Span>,
    next_token: Option<&TokenTree>,
    prev_token: Option<&TokenTree>,
) {
    let start = curr_span
        .and_then(|x| Some(Position::from(x.start())))
        .unwrap_or(Position::max());
    let end = curr_span
        .and_then(|x| Some(Position::from(x.end())))
        .unwrap_or(Position::max());

    // Comments only potentially exist if there are un-accounted for characters in the source that the
    // given span is skipping over.
    if start > source.get_pos() {
        if let Some(str) = source.str(start) {
            if !str.is_empty() {
                // Will be none if there are no comments
                if let Some(comments) = from_str(&str, prev_token.is_some()) {
                    for comment in &comments {
                        inject_comment(tokens, comment);
                    }

                    // Get the first comment to help in injecting dummy tokens
                    let comment = comments.first().unwrap();

                    // If there is no source token after this we need to inject a placeholder token
                    // or else the syn package will barf. Closing group tokens are not considered code
                    // and would still require a dummy token.
                    if next_token.is_none() {
                        if let Some(prev) = prev_token {
                            // Comments trailing fields or code blocks
                            // e.g. `a: i32, // Field a` or `println!("\n"); // Block comment`
                            if let TokenTree::Punct(punct) = prev {
                                if comment.is_trailing() {
                                    // Dummy field would be valid after a comma as function arguments
                                    // woudn't qualify for trailing comments
                                    if punct.as_char() == ',' {
                                        inject_dummy_field(tokens);

                                    // Dummy struct is valid after code block
                                    } else if punct.as_char() == ';' {
                                        inject_dummy_struct(tokens);
                                    }
                                }
                            }

                        // No previous token means its an empty file and struct will work
                        } else {
                            inject_dummy_struct(tokens);
                        }
                    }
                }
            }
        }
    };

    // Update the offset to the end of the token
    if source.get_pos() < end {
        source.set_pos(end);
    }
}

/// Convert the span into a string for debugging purposes.  The span is passed in becasue we don't
/// know in the group case if we are dealing with the open or close span from this context.
fn token_to_str(token: &TokenTree, span: &Span) -> String {
    let start: Position = span.start().into();
    let end: Position = span.end().into();
    let mut out = String::new();

    // Create the string for the span
    out.push_str(&format!(
        "{: <8} {: <10} {}",
        token.name(),
        format!("{}..{}", start, end),
        span.source_text().unwrap_or("<None>".into()),
    ));
    out
}

/// Translate the comment into tokens. proc_macro2 already set the precedence
/// of storing doc comments as attributes. We are just extending this pattern
/// to include all comments as attributes.
///
/// * ***tokens***: Token stream to inject the comment into
/// * ***comment***: Comment to inject
fn inject_comment(tokens: &mut Vec<TokenTree>, comment: &Comment) {
    let punct = '#';
    let msg = comment.text();
    let attr_name = comment.attr_name();
    trace!("Comment: {}[{} = \"{}\"]", punct, attr_name, msg);

    // Spans are an optional feature in proc_macro2 that luckily syn doesn't take
    // into account. This means we can safely ignore the spans
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

/// Inject a dummy varient tokens to ensure that the token stream is valid for syn
///
/// * ***tokens***: Token stream to inject the dummy into
fn inject_dummy_variant(tokens: &mut Vec<TokenTree>) {
    let token = TokenTree::from(Ident::new(crate::DUMMY, Span::call_site()));
    trace!("{}", token_to_str(&token, &token.span()));
    tokens.push(token);

    let token = TokenTree::from(Punct::new(',', Spacing::Alone));
    trace!("{}", token_to_str(&token, &token.span()));
    tokens.push(token);
}

/// Inject a dummy field tokens to ensure that the token stream is valid for syn
///
/// * ***tokens***: Token stream to inject the dummy into
fn inject_dummy_field(tokens: &mut Vec<TokenTree>) {
    let token = TokenTree::from(Ident::new(crate::DUMMY, Span::call_site()));
    trace!("{}", token_to_str(&token, &token.span()));
    tokens.push(token);

    let token = TokenTree::from(Punct::new(':', Spacing::Alone));
    trace!("{}", token_to_str(&token, &token.span()));
    tokens.push(token);

    let token = TokenTree::from(Ident::new("i32", Span::call_site()));
    trace!("{}", token_to_str(&token, &token.span()));
    tokens.push(token);

    let token = TokenTree::from(Punct::new(',', Spacing::Alone));
    trace!("{}", token_to_str(&token, &token.span()));
    tokens.push(token);
}

/// Inject a dummy struct tokens to ensure that the token stream is valid for syn
///
/// * ***tokens***: Token stream to inject the dummy into
fn inject_dummy_struct(tokens: &mut Vec<TokenTree>) {
    let token = TokenTree::from(Ident::new("struct", Span::call_site()));
    trace!("{}", token_to_str(&token, &token.span()));
    tokens.push(token);

    let token = TokenTree::from(Ident::new(crate::DUMMY, Span::call_site()));
    trace!("{}", token_to_str(&token, &token.span()));
    tokens.push(token);

    let token = TokenTree::from(Punct::new(';', Spacing::Alone));
    trace!("{}", token_to_str(&token, &token.span()));
    tokens.push(token);
}

/// Parse comments from the given string
///
/// * ***text***: Unaccounted for string between source tokens
/// * ***prev_src***: Indicates source has already been processed
fn from_str(str: &str, prev_src: bool) -> Option<Vec<Comment>> {
    let mut comments: Vec<Comment> = vec![]; // final results
    let mut line = String::new(); // temp buffer

    // Track throughout
    let mut single_expected_newline = false;
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

    let mut iter = str.chars().peekable();
    while let Some(mut char) = iter.next() {
        line.push(char);

        if char == '\n' {
            // Drop carriage returns in favor of newlines
            if prev_char == '\r' {
                line.pop();
            }

            // It is expected to get a single newline after source which we need to drop
            if !single_expected_newline && empty && prev_src {
                single_expected_newline = true;
                reset(&mut line, &mut empty, &mut comment_line, &mut prev_char);
                continue;
            }

            // Only allow a single empty line consecutively regardless of context
            if empty {
                if prev_empty_line {
                    reset(&mut line, &mut empty, &mut comment_line, &mut prev_char);
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
            if empty || comment_line {
                line.pop(); // drop newline

                let comment = if empty {
                    Comment::Empty
                } else {
                    // A trailing comment won't ever have a preceding comment as the preceding string
                    // is source code if we actually have some preceding source code.
                    if comments.is_empty() && prev_src {
                        Comment::LineTrailing(line.clone())
                    } else {
                        Comment::Line(line.clone())
                    }
                };
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
                    line.truncate(line.len() - 2); // drop control characters

                    // Consume anything up to and including the next newline
                    let mut newline = false;
                    while let Some(c) = iter.next() {
                        if c == '\n' {
                            newline = true;
                            break;
                        }
                    }

                    // Store the block comment
                    if newline {
                        comments.push(Comment::Block(line.clone()));
                    } else {
                        comments.push(Comment::BlockInline(line.clone()));
                    }
                    reset(&mut line, &mut empty, &mut comment_line, &mut prev_char);
                }
            } else {
                // Check for doc comments first
                let mut doc_comment = false;
                if let Some(next_char) = iter.peek() {
                    if prev_char == '/' && char == '/' && (*next_char == '/' || *next_char == '!') {
                        line.clear(); // drop garbage and start storing comment
                        doc_comment = true;
                        char = iter.next().unwrap();
                    }
                }

                // Fall back on regular comments
                if !doc_comment {
                    if prev_char == '/' && (char == '/' || char == '*') {
                        line.clear(); // drop garbage and start storing comment
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
    fn is_group_close(&self, start: &Position) -> bool;
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

    /// Check if the token is a closing group token
    ///
    /// * ***start***: Start position of the groups's close span
    fn is_group_close(&self, start: &Position) -> bool {
        if let TokenTree::Group(group) = self {
            return Position::from(group.span_close().start()) == *start;
        }
        false
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::pos;
    use core::panic;
    use indoc::indoc;
    use proc_macro2::TokenStream;
    use std::str::FromStr;
    use tracing_test::traced_test;

    fn tokens_to_comment(iter: &mut dyn Iterator<Item = TokenTree>) -> Comment {
        let mut tokens = iter.next().unwrap().as_group().stream().into_iter();

        // Parse out the comment type
        let ident = tokens.next().unwrap().as_ident();
        let name = ident.to_string();

        // Skip the internal attribute punct token
        tokens.next();

        // Parse out the comment value and un-quote it
        let literal = tokens.next().unwrap().as_literal();
        let str = literal.to_string();

        let value = str[1..str.len() - 1].to_string();
        Comment::from_str(&format!("{}:{}", name, value)).unwrap()
    }

    trait TokenExt {
        fn as_group(self) -> Group;
        fn as_ident(self) -> Ident;
        fn as_literal(self) -> Literal;
        fn is_comment(&self) -> bool;
    }
    impl TokenExt for TokenTree {
        fn as_group(self) -> Group {
            if let TokenTree::Group(group) = self {
                group
            } else {
                panic!("Expected a group token, found: {:?}", self);
            }
        }
        fn as_ident(self) -> Ident {
            if let TokenTree::Ident(ident) = self {
                ident
            } else {
                panic!("Expected a ident token, found: {:?}", self);
            }
        }
        fn as_literal(self) -> Literal {
            if let TokenTree::Literal(literal) = self {
                literal
            } else {
                panic!("Expected a literal token, found: {:?}", self);
            }
        }

        fn is_comment(&self) -> bool {
            if let TokenTree::Punct(punct) = self {
                if punct.as_char() == '#'
                    && punct
                        .span()
                        .source_text()
                        .filter(|x| x.starts_with('/'))
                        .is_none()
                {
                    return true;
                }
            }
            false
        }
    }

    trait TokensExt {
        fn get<P: Into<Position>>(&self, pos: P) -> TokenTree;
        fn comments_after<P: Into<Position>>(&self, pos: P) -> Vec<Comment>;
        fn comments_before<P: Into<Position>>(&self, pos: P) -> Vec<Comment>;
        fn comment_count(&self) -> usize;
        fn recursive_count(&self) -> usize;
    }
    impl<I: Iterator<Item = TokenTree> + Clone> TokensExt for I {
        fn get<P: Into<Position>>(&self, pos: P) -> TokenTree {
            let pos = pos.into();
            let mut iter = self.clone();
            iter.find(|x| pos == Position::from(x.span().start()))
                .unwrap()
        }

        // Get the comments after the given position
        fn comments_after<P: Into<Position>>(&self, pos: P) -> Vec<Comment> {
            let mut result = vec![];
            let pos = pos.into();
            let mut iter = self.clone().peekable();

            while let Some(curr) = iter.next() {
                // Check if curr is comment, should only trigger after the peek case has already
                if result.len() > 0 {
                    if curr.is_comment() {
                        result.push(tokens_to_comment(&mut iter));
                    } else {
                        return result;
                    }
                }

                // Check if the next is comment
                if let Some(next) = iter.peek() {
                    if next.is_comment() && pos == Position::from(curr.span().start()) {
                        iter.next(); // skip the punct token
                        result.push(tokens_to_comment(&mut iter));
                    }
                }
            }
            result
        }

        // Get the comments before the given position
        fn comments_before<P: Into<Position>>(&self, pos: P) -> Vec<Comment> {
            let mut result = vec![];
            let pos = pos.into();
            let mut iter = self.clone().peekable();

            while let Some(curr) = iter.next() {
                if curr.is_comment() {
                    result.push(tokens_to_comment(&mut iter));
                }

                // Check next to see if we should stop gathering comments
                if let Some(next) = iter.peek() {
                    if next.is_comment() {
                        continue;
                    }

                    // If this is never found then we fall off the end and return all found comments
                    // which is nice for a purely comment filled file.
                    if pos == Position::from(next.span().start()) {
                        return result;
                    } else {
                        result.clear();
                    }
                }
            }
            result
        }

        // Count all punct tokens of comment type recursively
        fn comment_count(&self) -> usize {
            let mut iter = self.clone();
            fn recurse(iter: &mut dyn Iterator<Item = TokenTree>) -> usize {
                let mut count = 0;
                for token in iter {
                    match &token {
                        TokenTree::Group(group) => {
                            count += recurse(&mut group.stream().into_iter());
                        }
                        TokenTree::Punct(_) => {
                            if token.is_comment() {
                                count += 1;
                            }
                        }
                        _ => {}
                    }
                }
                count
            }
            recurse(&mut iter)
        }

        /// Recursively count all tokens
        fn recursive_count(&self) -> usize {
            let mut iter = self.clone();
            fn recurse(iter: &mut dyn Iterator<Item = TokenTree>) -> usize {
                let mut count = 0;
                for token in iter {
                    match &token {
                        TokenTree::Group(group) => {
                            count += recurse(&mut group.stream().into_iter());
                        }
                        _ => {
                            count += 1;
                        }
                    }
                }
                count
            }
            recurse(&mut iter)
        }
    }

    #[traced_test]
    #[test]
    fn test_trailing_variant() {
        let source = indoc! {r#"
            enum Foo {
                A, // A field
            }
        "#};
        let tokens = inject(&Config::default(), source).unwrap().into_iter();
        assert_eq!(tokens.comment_count(), 1);
        assert_eq!(tokens.recursive_count(), 10);
        // let group = tokens.get((0, 11)).as_group();
        // let tokens = group.stream().into_iter();
        // assert_eq!(
        //     tokens.comments_after((1, 10)),
        //     vec![Comment::LineTrailing(" A field".into())]
        // );
    }

    #[test]
    fn test_trailing_regular() {
        let source = indoc! {r#"
            struct Foo; // A struct
            println!("Hello");
        "#};
        let tokens = inject(&Config::default(), source).unwrap().into_iter();
        assert_eq!(tokens.comment_count(), 1);
        assert_eq!(tokens.recursive_count(), 11);
        assert_eq!(
            tokens.comments_after((0, 10)),
            vec![Comment::LineTrailing(" A struct".into())]
        );
    }

    #[test]
    fn test_trailing_field() {
        let source = indoc! {r#"
            struct Foo {
                a: i32, // A field
            }
        "#};
        let tokens = inject(&Config::default(), source).unwrap().into_iter();
        assert_eq!(tokens.comment_count(), 1);
        assert_eq!(tokens.recursive_count(), 14); // includes 4 tokens for dummy field
        let group = tokens.get((0, 11)).as_group();
        let tokens = group.stream().into_iter();
        assert_eq!(
            tokens.comments_after((1, 10)),
            vec![Comment::LineTrailing(" A field".into())]
        );
    }

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

        // Check the tokens that were generated
        let tokens = inject(&Config::default(), source).unwrap().into_iter();

        // Check total comments
        assert_eq!(tokens.comment_count(), 5);

        // Check first after the use statement
        assert_eq!(tokens.comments_before((1, 1)), vec![Comment::Empty]);

        // Get the group at postiion which you can see with tracing output
        let group = tokens.get((2, 11)).as_group();
        let tokens = group.stream().into_iter();

        assert_eq!(
            tokens.comments_before((5, 4)),
            vec![
                Comment::Empty,
                Comment::Line("     Indented comment".into())
            ]
        );
        assert_eq!(
            tokens.comments_after((5, 10)),
            vec![Comment::Empty, Comment::Line(" Field b".into())]
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
            struct Foo;
        "#};
        let tokens = inject(&Config::default(), source).unwrap().into_iter();
        assert_eq!(tokens.comment_count(), 1);
        assert_eq!(
            tokens.comments_before((7, 0)),
            vec![Comment::Block(
                "***\\n\\n // Line 1\\n * Block\\n // Line 2\\n\\n **".into()
            )]
        );
    }

    #[test]
    fn test_block_inline() {
        let source = indoc! {r#"
            // Line 1
            struct Foo;

            /* Block line */
            println!("{}", /* Block inline */ other);
        "#};

        let tokens = inject(&Config::default(), source).unwrap().into_iter();
        assert_eq!(tokens.comment_count(), 4);
        assert_eq!(
            tokens.comments_before((1, 0)),
            vec![Comment::Line(" Line 1".into())]
        );
        assert_eq!(
            tokens.comments_after((1, 10)),
            vec![Comment::Empty, Comment::Block(" Block line ".into())]
        );

        // Get inner block group
        let group = tokens.get((4, 8)).as_group();
        let tokens = group.stream().into_iter();
        assert_eq!(
            tokens.comments_after((4, 13)),
            vec![Comment::BlockInline(" Block inline ".into())]
        );
    }

    #[test]
    fn test_trailing_comments() {
        let source = indoc! {r#"
            struct Foo {
                a: i32, // Field a
                b: i32, // Field b
            }
        "#};
        let tokens = inject(&Config::default(), source).unwrap().into_iter();

        // Get the group at postiion which you can see with tracing output
        let group = tokens.get((0, 11)).as_group();
        let tokens = group.stream().into_iter();

        assert_eq!(tokens.comment_count(), 2);
        assert_eq!(tokens.recursive_count(), 20);

        // Check that the first comment
        assert_eq!(
            tokens.comments_after((1, 10)),
            vec![Comment::LineTrailing(" Field a".into())]
        );
        assert_eq!(
            tokens.comments_after((2, 10)),
            vec![Comment::LineTrailing(" Field b".into())]
        );
    }

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
        let tokens = inject(&Config::default(), source).unwrap().into_iter();

        // Check total comments
        assert_eq!(tokens.comment_count(), 4);

        // Check first after the use statement
        assert_eq!(tokens.comments_after((0, 16)), vec![Comment::Empty]);

        // Get the group at postiion which you can see with tracing output
        let group = tokens.get((2, 24)).as_group();
        let tokens = group.stream().into_iter();

        // Check second and third after the subscriber call
        assert_eq!(
            tokens.comments_after((6, 64)),
            vec![Comment::Empty, Comment::Line(" Pass in an example".into())]
        );

        // Check the last before the Ok call
        assert_eq!(tokens.comments_after((9, 33)), vec![Comment::Empty]);
    }

    #[test]
    fn test_simple_outer_comment() {
        let source = indoc! {r#"
            // A foo struct
            struct Foo;
        "#};
        let tokens = inject(&Config::default(), source).unwrap().into_iter();
        assert_eq!(tokens.comment_count(), 1);
        assert_eq!(
            tokens.comments_before((1, 0)),
            vec![Comment::Line(" A foo struct".into())]
        );
    }

    #[test]
    fn test_only_comments() {
        let source = indoc! {r#"
            // Only comments 1
            // Only comments 2
        "#};
        let tokens = inject(&Config::default(), source).unwrap().into_iter();
        assert_eq!(tokens.comment_count(), 2);
        assert_eq!(tokens.recursive_count(), 11);

        assert_eq!(
            // We inject a dummy ident token which will have span 0,0
            tokens.comments_before(Position::default()),
            vec![
                Comment::Line(" Only comments 1".into()),
                Comment::Line(" Only comments 2".into())
            ]
        );
    }

    #[test]
    fn test_mixing_comment_line_and_inner_and_outer_success() {
        let source = indoc! {r#"

            // Line

            /// Outer

            //! Inner
        "#};
        let tokens = inject(&Config::default(), source).unwrap().into_iter();
        assert_eq!(tokens.comment_count(), 4);
        //assert_eq!(tokens.recursive_count(), 11);
        assert_eq!(
            tokens.comments_before((3, 0)),
            vec![
                Comment::Empty,
                Comment::Line(" Line".into()),
                Comment::Empty,
            ]
        );
        assert_eq!(tokens.comments_before((5, 0)), vec![Comment::Empty,]);
    }

    #[test]
    fn test_trailing_newline_is_not_considered_an_empty_line() {
        let source = indoc! {r#"
            struct A;

            struct B;
        "#};

        let tokens = inject(&Config::default(), source).unwrap().into_iter();
        assert_eq!(tokens.comment_count(), 1);
        assert_eq!(tokens.comments_after((0, 8)), vec![Comment::Empty,]);
    }

    #[test]
    fn test_only_allow_single_empty_line_consecutively() {
        let source = indoc! {r#"


            println!("{}", "1");
        "#};

        // Check the string beeing fed in
        assert_eq!(source, "\n\nprintln!(\"{}\", \"1\");\n");

        // Check the resulting comments parsed out
        assert_eq!(from_str(source, false).unwrap(), vec![Comment::Empty]);

        // Check the tokens that were generated
        let tokens = inject(&Config::default(), source).unwrap().into_iter();

        assert_eq!(tokens.comment_count(), 1);
        assert_eq!(tokens.comments_before((2, 0)), vec![Comment::Empty,]);
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
