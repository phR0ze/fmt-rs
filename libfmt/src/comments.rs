use crate::{
    model::{Comment, Config, Position, Source, TokenExt},
    Error, Result,
};
use proc_macro2::{Delimiter, Group, Ident, Literal, Punct, Spacing, Span, TokenStream, TokenTree};
use std::str::FromStr;
use tracing::trace;

enum Doc {
    Inner,
    Outer,
}

impl Doc {
    pub(crate) fn is_inner(&self) -> bool {
        match self {
            Self::Inner => true,
            _ => false,
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

    // If we have no tokens at all we might still have comments
    if tokens.is_empty() && prev.is_none() {
        inject_comments(source, &mut result, None, None, None);
        return result;
    }

    let mut prev = prev;
    let mut tokens = tokens.into_iter().peekable();
    while let Some(token) = tokens.next() {
        let next = tokens.peek();

        match &token {
            TokenTree::Ident(ident) => {
                let span = ident.span();
                inject_comments(source, &mut result, Some(&span), prev.as_ref(), next);
                trace!("{}", token.to_str(&span));
                result.push(token);
            }
            TokenTree::Literal(literal) => {
                let span = literal.span();
                inject_comments(source, &mut result, Some(&span), prev.as_ref(), next);
                trace!("{}", token.to_str(&span));
                result.push(token);
            }
            TokenTree::Group(group) => {
                // Need to account for the group's opening control character
                let span = group.span_open();
                inject_comments(source, &mut result, Some(&span), prev.as_ref(), next);

                // Recurse on group and update previous token to the last one in the group
                trace!("{}", token.to_str(&group.span_open()));
                let mut _tokens = inject_tokens(source, group.stream(), prev.clone());
                prev = _tokens.last().as_deref().cloned();

                // Inject any comments at the end of the group that were not captured
                let span = group.span_close();
                inject_comments(source, &mut _tokens, Some(&span), prev.as_ref(), next);
                trace!("{}", token.to_str(&group.span_close()));

                // Construct new group to capture group token stream changes
                let mut _group = Group::new(group.delimiter(), TokenStream::from_iter(_tokens));
                _group.set_span(group.span());
                result.push(TokenTree::Group(_group));
            }
            TokenTree::Punct(punct) => {
                let span = punct.span();
                inject_comments(source, &mut result, Some(&span), prev.as_ref(), next);
                trace!("{}", token.to_str(&span));

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
                            trace!("{}", token.to_str(&token.span()));
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
/// * ***curr_span***: Current span to process or None if there are no tokens
/// * ***prev_token***: Previous token to take into account for comment injection
/// * ***next_token***: Next token to take into account for comment injection
fn inject_comments(
    source: &mut Source,
    tokens: &mut Vec<TokenTree>,
    curr_span: Option<&Span>,
    prev_token: Option<&TokenTree>,
    next_token: Option<&TokenTree>,
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
                        if curr_span.is_none() {
                            inject_comment(tokens, comment, Doc::Inner);
                        } else {
                            inject_comment(tokens, comment, Doc::Outer);
                        }
                    }

                    // Check for dummy injection or comments that were missed
                    if next_token.is_none() {
                        // Only the first comment can be a trailing one as anything after would be
                        // on a newline.
                        let comment = comments.first().unwrap();

                        // If there is no source token after this we need to inject a placeholder token
                        // or else the syn package will barf.
                        if let Some(prev) = prev_token {
                            // Comments trailing fields or code blocks
                            // e.g. `a: i32, // Field a` or `println!("\n"); // Block comment`
                            if let TokenTree::Punct(punct) = prev {
                                if comment.is_trailing() {
                                    // Dummy field would be valid after a comma as function arguments
                                    // woudn't qualify for trailing comments
                                    if punct.as_char() == ',' {
                                        inject_dummy_field(tokens);

                                    // Dummy struct works after a semicolon
                                    } else if punct.as_char() == ';' {
                                        inject_dummy_struct(tokens);
                                    }
                                }
                            }
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

/// Inject a dummy varient tokens to ensure that the token stream is valid for syn
///
/// * ***tokens***: Token stream to inject the dummy into
fn inject_dummy_variant(tokens: &mut Vec<TokenTree>) {
    let token = TokenTree::from(Ident::new(crate::DUMMY, Span::call_site()));
    trace!("{}", token.to_str(&token.span()));
    tokens.push(token);

    let token = TokenTree::from(Punct::new(',', Spacing::Alone));
    trace!("{}", token.to_str(&token.span()));
    tokens.push(token);
}

/// Inject dummy field tokens to ensure that the token stream is valid for syn. Trailing comments
/// require a dummy field after as they are injected as outer comments and syn checks that there is
/// an associated field.
///
/// * ***tokens***: Token stream to inject the dummy into
fn inject_dummy_field(tokens: &mut Vec<TokenTree>) {
    let token = TokenTree::from(Ident::new(crate::DUMMY, Span::call_site()));
    trace!("{}", token.to_str(&token.span()));
    tokens.push(token);

    let token = TokenTree::from(Punct::new(':', Spacing::Alone));
    trace!("{}", token.to_str(&token.span()));
    tokens.push(token);

    let token = TokenTree::from(Ident::new("i32", Span::call_site()));
    trace!("{}", token.to_str(&token.span()));
    tokens.push(token);

    let token = TokenTree::from(Punct::new(',', Spacing::Alone));
    trace!("{}", token.to_str(&token.span()));
    tokens.push(token);
}

/// Inject a dummy struct tokens to ensure that the token stream is valid for syn
///
/// * ***tokens***: Token stream to inject the dummy into
fn inject_dummy_struct(tokens: &mut Vec<TokenTree>) {
    let token = TokenTree::from(Ident::new("struct", Span::call_site()));
    trace!("{}", token.to_str(&token.span()));
    tokens.push(token);

    let token = TokenTree::from(Ident::new(crate::DUMMY, Span::call_site()));
    trace!("{}", token.to_str(&token.span()));
    tokens.push(token);

    let token = TokenTree::from(Punct::new(';', Spacing::Alone));
    trace!("{}", token.to_str(&token.span()));
    tokens.push(token);
}

/// Translate the comment into inner doc tokens which follows proc_macro2 precedence of storing doc
/// comments as attributes. We are just leveraging this pattern to trick syn into passing through
/// regular comments as inner doc comments which can be allowed anywhere. It abuses the system
/// slightly but we'll strip them back out later during scaning.
///
/// * ***tokens***: Token stream to inject the comment into
/// * ***comment***: Comment to inject
/// * ***doc***: Direction on how to convert the comment
fn inject_comment(tokens: &mut Vec<TokenTree>, comment: &Comment, doc: Doc) {
    // Spans are an optional feature in proc_macro2 that luckily syn doesn't take into account. This
    // means being unable to set them due to to being private doesn't matter.
    let token: TokenTree = Punct::new('#', Spacing::Alone).into();
    trace!("{}", token.to_str(&token.span()));
    tokens.push(token);

    if doc.is_inner() {
        let token: TokenTree = Punct::new('!', Spacing::Alone).into();
        trace!("{}", token.to_str(&token.span()));
        tokens.push(token);
    }

    // Create and log new comment group
    let mut stream = vec![];

    trace!("{: <12}{: <6} {}", "0:0..0:0", "Group", "[");

    let token: TokenTree = Ident::new(&comment.attr_name(), Span::call_site()).into();
    trace!("  {}", token.to_str(&token.span()));
    stream.push(token);

    let token: TokenTree = Punct::new('=', Spacing::Alone).into();
    trace!("  {}", token.to_str(&token.span()));
    stream.push(token);

    let token: TokenTree = Literal::string(&comment.text()).into();
    trace!("  {}", token.to_str(&token.span()));
    stream.push(token);

    tokens.push(
        Group::new(
            Delimiter::Bracket,
            TokenStream::from_iter::<Vec<TokenTree>>(stream),
        )
        .into(),
    );

    trace!("{: <12}{: <6} {}", "0:0..0:0", "Group", "]");
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::{pos, TokenExt};
    use core::panic;
    use indoc::indoc;
    use itertools::{peek_nth, PeekNth};
    use proc_macro2::TokenStream;
    use std::str::FromStr;
    use tracing_test::traced_test;

    trait TokenTestsExt {
        fn as_group(self) -> Group;
        fn as_ident(self) -> Ident;
        fn as_literal(self) -> Literal;
        fn to_comment(&self) -> Option<Comment>;
    }
    impl TokenTestsExt for TokenTree {
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

        /// Check if the token is a group and if so extract the comment if it exists
        fn to_comment(&self) -> Option<Comment> {
            if let TokenTree::Group(group) = self {
                let mut tokens = group.stream().into_iter();

                // Parse the comment type from the ident
                if let Some(token) = tokens.next() {
                    if let TokenTree::Ident(ident) = token {
                        let name = ident.to_string();
                        if name.starts_with("comment") {
                            tokens.next(); // skip punct

                            // Get the comment literal
                            let literal = tokens.next().unwrap().as_literal();
                            let str = literal.to_string();

                            // Trim off the quotes that the tokenization process adds
                            let value = str[1..str.len() - 1].to_string();

                            // Build the comment from the string components
                            return Some(
                                Comment::from_str(&format!("{}:{}", name, value)).unwrap(),
                            );
                        }
                    }
                }
            }
            None
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

        /// Get the comments after the given position
        fn comments_after<P: Into<Position>>(&self, pos: P) -> Vec<Comment> {
            let mut result = vec![];
            let pos = pos.into();
            let mut iter = peek_nth(self.clone());

            while let Some(curr) = iter.next() {
                // Find the given position
                if Position::from(curr.span().start()) == pos {
                    // Gather comments until a non comment is found
                    while let Some(_) = iter.next() {
                        // Check for outer comment case
                        if let Some(comment) = iter.peek_nth(0).and_then(|x| x.to_comment()) {
                            result.push(comment);
                            iter.next(); // consume group

                        // Check inner comment case
                        } else if let Some(comment) = iter.peek_nth(1).and_then(|x| x.to_comment())
                        {
                            result.push(comment);
                            iter.next(); // consume punct
                            iter.next(); // consume group
                        } else {
                            break;
                        }
                    }
                    break;
                }
            }
            result
        }

        /// Get the comments before the given position
        fn comments_before<P: Into<Position>>(&self, pos: P) -> Vec<Comment> {
            let mut result = vec![];
            let pos = pos.into();
            let mut iter = peek_nth(self.clone());

            while let Some(curr) = iter.next() {
                // Trigger on the first comment found
                if let Some(comment) = curr.to_comment() {
                    result.push(comment);

                    // Consume all comments until a non comment is found
                    while let Some(curr) = iter.next() {
                        // Check for outer comment case
                        if let Some(comment) = iter.peek_nth(0).and_then(|x| x.to_comment()) {
                            result.push(comment);
                            iter.next(); // consume group

                        // Check inner comment case
                        } else if let Some(comment) = iter.peek_nth(1).and_then(|x| x.to_comment())
                        {
                            result.push(comment);
                            iter.next(); // consume punct
                            iter.next(); // consume group

                        // If this is never found then we fall off the end and return all found comments
                        // which is nice for a purely comment filled file.
                        } else if Position::from(curr.span().start()) == pos {
                            return result;
                        } else {
                            result.clear();
                            break;
                        }
                    }
                }
            }
            result
        }

        // Count all punct tokens of comment type recursively
        fn comment_count(&self) -> usize {
            let mut iter = peek_nth(self.clone());
            fn recurse<I: Iterator<Item = TokenTree>>(iter: &mut PeekNth<I>) -> usize {
                let mut count = 0;
                while let Some(token) = iter.next() {
                    if let TokenTree::Group(group) = &token {
                        if token.to_comment().is_some() {
                            count += 1;
                        } else {
                            count += recurse(&mut peek_nth(group.stream().into_iter()));
                        }
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
                    count += 1;
                    if let TokenTree::Group(group) = token {
                        count += recurse(&mut group.stream().into_iter());
                    }
                }
                count
            }
            recurse(&mut iter)
        }
    }

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

    #[traced_test]
    #[test]
    fn test_trailing_regular_single() {
        let source = indoc! {r#"
            struct Foo; // A struct
        "#};
        let tokens = inject(&Config::default(), source).unwrap().into_iter();
        assert!(syn::parse2::<syn::File>(TokenStream::from_iter(tokens.clone())).is_ok());
        assert_eq!(tokens.clone().comment_count(), 1);
        // assert_eq!(tokens.recursive_count(), 6);
        // assert_eq!(
        //     tokens.comments_after((0, 10)),
        //     vec![Comment::LineTrailing(" A struct".into())]
        // );
    }

    #[traced_test]
    #[test]
    fn test_trailing_field_multiple() {
        let source = indoc! {r#"
            struct Foo {
                a: i32, // A field
                b: i32, // B field
            }
        "#};
        let tokens = inject(&Config::default(), source).unwrap().into_iter();
        assert!(syn::parse2::<syn::File>(TokenStream::from_iter(tokens.clone())).is_ok());
        assert_eq!(tokens.comment_count(), 2);
        let group = tokens.get((0, 11)).as_group();
        let tokens = group.stream().into_iter();
        assert_eq!(
            tokens.comments_after((1, 10)),
            vec![Comment::LineTrailing(" A field".into())]
        );
        assert_eq!(
            tokens.comments_after((2, 10)),
            vec![Comment::LineTrailing(" B field".into())]
        );
    }

    #[test]
    fn test_trailing_field_single() {
        let source = indoc! {r#"
            struct Foo {
                a: i32, // A field
            }
        "#};
        let tokens = inject(&Config::default(), source).unwrap().into_iter();
        assert!(syn::parse2::<syn::File>(TokenStream::from_iter(tokens.clone())).is_ok());
        assert_eq!(tokens.comment_count(), 1);
        assert_eq!(tokens.recursive_count(), 16);
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
        assert!(syn::parse2::<syn::File>(TokenStream::from_iter(tokens.clone())).is_ok());

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
        assert!(syn::parse2::<syn::File>(TokenStream::from_iter(tokens.clone())).is_ok());
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
        assert!(syn::parse2::<syn::File>(TokenStream::from_iter(tokens.clone())).is_ok());
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
        assert!(syn::parse2::<syn::File>(TokenStream::from_iter(tokens.clone())).is_ok());

        // Get the group at postiion which you can see with tracing output
        let group = tokens.get((0, 11)).as_group();
        let tokens = group.stream().into_iter();

        assert_eq!(tokens.comment_count(), 2);
        assert_eq!(tokens.recursive_count(), 22);

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
        assert!(syn::parse2::<syn::File>(TokenStream::from_iter(tokens.clone())).is_ok());

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
        assert!(syn::parse2::<syn::File>(TokenStream::from_iter(tokens.clone())).is_ok());
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
        assert!(syn::parse2::<syn::File>(TokenStream::from_iter(tokens.clone())).is_ok());

        assert_eq!(tokens.comment_count(), 2);

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
            //! Inner
            
            // Regular
            
            /// Regular
            struct Foo;
        "#};
        let tokens = inject(&Config::default(), source).unwrap().into_iter();
        assert!(syn::parse2::<syn::File>(TokenStream::from_iter(tokens.clone())).is_ok());

        assert_eq!(tokens.comment_count(), 3);
        assert_eq!(
            tokens.comments_before((4, 0)),
            vec![
                Comment::Empty,
                Comment::Line(" Regular".into()),
                Comment::Empty,
            ]
        );
    }

    #[test]
    fn test_trailing_newline_is_not_considered_an_empty_line() {
        let source = indoc! {r#"
            struct A;

            struct B;
        "#};

        let tokens = inject(&Config::default(), source).unwrap().into_iter();
        assert!(syn::parse2::<syn::File>(TokenStream::from_iter(tokens.clone())).is_ok());

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

        // Check the tokens that were generated
        let tokens = inject(&Config::default(), source).unwrap().into_iter();
        assert!(syn::parse2::<syn::File>(TokenStream::from_iter(tokens.clone())).is_ok());

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
