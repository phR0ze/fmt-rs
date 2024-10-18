use crate::{
    model::{
        Comment, Config, OptionTokenExt, Position, Source, TokenExt, TokenGroup, TokenItem,
        TokenWrap,
    },
    Error, Result,
};
use proc_macro2::{Delimiter, Group, Ident, Literal, Punct, Spacing, Span, TokenStream, TokenTree};
use std::{collections::VecDeque, str::FromStr, vec};
use tracing::trace;

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
    // Parse source into a token stream
    let tokens = TokenStream::from_str(source)
        .map_err(|e| Error::new("failed to parse source into token stream").wrap_lex(e))?;

    // Inject comments into the token stream
    let commenter = Commenter::new(config, Source::new(source)).inject(tokens);
    Ok(commenter.into_token_stream())
}

/// Newtype to allow for custom implementations
#[derive(Debug)]
pub(crate) struct Commenter<'a> {
    config: &'a Config,
    source: Source,
    groups: Vec<TokenGroup>,
}

impl<'a> Commenter<'a> {
    /// Create a new instance
    pub(crate) fn new(config: &'a Config, source: Source) -> Self {
        Self {
            config,
            source,
            groups: vec![TokenGroup::new(None)],
        }
    }

    /// Inject comments into the token stream from the source
    pub(crate) fn inject(mut self, stream: TokenStream) -> Self {
        let mut stream = expand_tokens(stream);
        let mut src: Option<String>;

        // Comments file only
        // -----------------------------------------------------------------------------------------
        if self.is_empty() && stream.is_empty() {
            src = self.source.range(None, None);
            if let Some(comments) = parse_comments(src.as_deref(), false) {
                self.append_curr_comments(comments, true);
            }
        }

        while let Some(token0) = stream.pop_front() {
            let (start0, end0) = token0.span_open();

            // Leading comments - haven't processed any tokens yet
            // -------------------------------------------------------------------------------------
            if self.is_empty() {
                src = self.source.range(None, Some(start0));
                if let Some(comments) = parse_comments(src.as_deref(), false) {
                    self.append_curr_comments(comments, false);
                }
            }

            // Start a new group
            // -------------------------------------------------------------------------------------
            if self.is_group_start(&token0) {
                self.append_group(token0.take());

            // End a new group
            // -------------------------------------------------------------------------------------
            } else if self.is_group_end(&token0) {
                self.complete_group(&token0);

            // Pass proc_macro2 parsed doc comments directly through without processing
            // -------------------------------------------------------------------------------------
            } else if self.is_doc_comment(&token0) {
                self.append_curr(token0.take());
                let mut in_group = false;
                while self.pass_doc_comments(&mut in_group, end0, stream.pop_front()) {}
                self.complete_line(true);

            // Store regular tokens
            // -------------------------------------------------------------------------------------
            } else {
                self.append_curr(token0.take());
            }

            // Check for comments between tokens
            // -------------------------------------------------------------------------------------
            if let Some(token1) = stream.front() {
                let (start1, _) = token1.span_open();
                self.inject_comments(end0, start1)
            }
        }

        self.complete();
        self
    }

    /// Check the given range and inject any found comments in the appropriate lines
    ///
    /// * ***start***: Start position of the range
    /// * ***end***: End position of the range
    fn inject_comments(&mut self, start: Position, end: Position) {
        let mut newline = false;
        let src = self.source.range(Some(start), Some(end));

        if let Some(comments) = parse_comments(src.as_deref(), true) {
            newline = comments.iter().any(|x| x.is_break());

            // Add trailing comments to begining of current line
            self.prepend_curr_comments(
                comments
                    .clone()
                    .into_iter()
                    .filter(|x| x.is_trailing())
                    .collect(),
                false,
            );

            // Add regular comments between tokens to next line
            self.append_next_comments(
                comments
                    .clone()
                    .into_iter()
                    .filter(|x| x.is_regular())
                    .collect(),
                false,
            );
        }
        self.complete_line(newline);
    }

    /// Check if the token is a group start
    fn is_group_start(&self, wrap: &TokenWrap) -> bool {
        if let TokenWrap::GroupStart(_) = wrap {
            return true;
        }
        false
    }

    /// Check if the token is a group end
    fn is_group_end(&self, wrap: &TokenWrap) -> bool {
        if let TokenWrap::GroupEnd(_) = wrap {
            return true;
        }
        false
    }

    /// Check if the token is a doc comment
    fn is_doc_comment(&self, wrap: &TokenWrap) -> bool {
        if let TokenWrap::Token(token) = wrap {
            let (start, _) = wrap.span_open();

            if let TokenTree::Punct(punct) = token {
                if punct.as_char() == '#' && self.source.char_at_is(start, '/') {
                    return true;
                }
            }
        }
        false
    }

    /// Pass through doc comments as is
    ///
    /// * ***in_group***: Are we currently in a group
    /// * ***end***: End position of the doc comment
    /// * ***curr***: Current token to check
    /// * ***next***: Next token to check
    /// * ***return***: True if we need to continue processing
    fn pass_doc_comments(
        &mut self,
        in_group: &mut bool,
        end: Position,
        curr: Option<TokenWrap>,
    ) -> bool {
        if let Some(token) = curr {
            let (start, _) = token.span_open();
            if start < end {
                trace!("{}", token.to_str());
                if !*in_group || token.is_group_start() {
                    *in_group = token.is_group_start();
                    self.append_curr(token.take());
                } else if let TokenWrap::GroupEnd(_) = token {
                    *in_group = false;
                    return false; // done
                }
                return true;
            }
        }
        false
    }

    /// Is the token stream empty
    fn is_empty(&self) -> bool {
        if self.groups.len() > 1 || self.groups.iter().any(|x| !x.is_empty()) {
            return false;
        }
        true
    }

    /// Complete out any remaining current line tokens
    fn complete(&mut self) {
        // There will always be at least the root group
        self.groups.last_mut().unwrap().complete_line();
    }

    /// Append group
    ///
    /// * ***group***: Group token to include in the new group
    fn append_group(&mut self, group: TokenTree) {
        trace!("{}", TokenWrap::GroupStart(group.clone()).to_str());
        self.groups.push(TokenGroup::new(Some(group)))
    }

    /// Complete group
    fn complete_group(&mut self, group: &TokenWrap) {
        trace!("{}", group.to_str());

        // Create a new TokenTree::Group to capture group token changes
        if let Some(mut group) = self.groups.pop() {
            if let Some(TokenTree::Group(token)) = group.token.clone() {
                group.complete();
                let mut _group =
                    Group::new(token.delimiter(), TokenStream::from_iter(group.complete));
                _group.set_span(token.span());
                self.append_curr(TokenTree::Group(_group));
            } else {
                panic!("Group end without token");
            }
        } else {
            panic!("Group end without start");
        }
    }

    /// Append comments to the current line
    ///
    /// * ***comments***: Comments to append
    /// * ***inner***: Append the given comments as inner comments
    fn append_curr_comments(&mut self, comments: Vec<Comment>, inner: bool) {
        if self.config.no_comments() {
            return;
        }
        for comment in comments {
            for token in comment_to_tokens(&comment, inner) {
                self.append_curr(token);
            }
        }
    }

    /// Append comments to the next line
    ///
    /// * ***comments***: Comments to append
    /// * ***inner***: Append the given comments as inner comments
    fn append_next_comments(&mut self, comments: Vec<Comment>, inner: bool) {
        if self.config.no_comments() {
            return;
        }
        for comment in comments {
            for token in comment_to_tokens(&comment, inner) {
                self.append_next(token);
            }
        }
    }

    /// Prepend comments to the current line
    ///
    /// * ***comments***: Comments to prepend
    /// * ***inner***: Prepend the given comments as inner comments
    fn prepend_curr_comments(&mut self, comments: Vec<Comment>, inner: bool) {
        if self.config.no_comments() {
            return;
        }
        for comment in comments {
            // Need to reverse the order since we are prepending one at a time
            for token in comment_to_tokens(&comment, inner).into_iter().rev() {
                self.prepend_curr(token);
            }
        }
    }

    /// Append token to the current line
    ///
    /// * ***token***: Token to append
    fn append_curr(&mut self, token: TokenTree) {
        if self.groups.len() > 1 {
            trace!(
                "APPEND_CURR_SUB: {}",
                TokenWrap::Token(token.clone()).to_str()
            );
        } else {
            trace!("APPEND_CURR: {}", TokenWrap::Token(token.clone()).to_str());
        }

        // There will always be at least the root group
        self.groups.last_mut().unwrap().curr_line.push(token);
    }

    /// Append token to the next line
    ///
    /// * ***token***: Token to append
    fn append_next(&mut self, token: TokenTree) {
        if self.groups.len() > 1 {
            trace!(
                "APPEND_NEXT_SUB: {}",
                TokenWrap::Token(token.clone()).to_str()
            );
        } else {
            trace!("APPEND_NEXT: {}", TokenWrap::Token(token.clone()).to_str());
        }

        // There will always be at least the root group
        self.groups.last_mut().unwrap().next_line.push(token);
    }

    /// Prepend token to the current line
    ///
    /// * ***token***: Token to prepend
    fn prepend_curr(&mut self, token: TokenTree) {
        if self.groups.len() > 1 {
            trace!(
                "PREPEND_CURR_SUB: {}",
                TokenWrap::Token(token.clone()).to_str()
            );
        } else {
            trace!("PREPEND_CURR: {}", TokenWrap::Token(token.clone()).to_str());
        }

        // There will always be at least the root group
        self.groups.last_mut().unwrap().curr_line.insert(0, token);
    }

    /// Conditionally handle a line break
    fn complete_line(&mut self, newline: bool) {
        if newline {
            trace!("Code break");

            // There will always be at least the root group
            self.groups.last_mut().unwrap().complete_line();
        }
    }

    /// Convert the token matrix into a TokenStream
    pub(crate) fn into_token_stream(self) -> TokenStream {
        self.groups
            .into_iter()
            .map(|x| x.into_token_stream())
            .flatten()
            .collect()
    }
}

/// Expand tokens into a flattened list of TokenWrap objects which provide more information
///
/// * ***stream*** - The token stream to convert
fn expand_tokens(stream: TokenStream) -> VecDeque<TokenWrap> {
    let mut iter = stream.into_iter();
    let mut buf: Vec<TokenItem> = vec![];
    let mut tokens: VecDeque<TokenWrap> = VecDeque::new();

    loop {
        // Fill the response from the buffer if available
        if let Some(item) = buf.pop() {
            match item {
                TokenItem::Token(token) => {
                    tokens.push_back(TokenWrap::Token(token));
                }
                TokenItem::Group(group, mut deque, visited) => {
                    if !visited {
                        tokens.push_back(TokenWrap::GroupStart(group.clone()));
                        buf.push(TokenItem::Group(group, deque, true));
                        continue;
                    }

                    // Process group tokens
                    if let Some(token) = deque.pop_front() {
                        buf.push(TokenItem::Group(group, deque, true));
                        buf.push(TokenItem::from(token))
                    } else {
                        tokens.push_back(TokenWrap::GroupEnd(group));
                    }
                }
            }
        } else {
            // No more tokens in the buffer so pull from iter
            if let Some(token) = iter.next() {
                buf.push(TokenItem::from(token));
            } else {
                // No more tokens
                break;
            }
        }
    }
    tokens
}

/// Translate the comment into doc tokens which follows proc_macro2 precedence of storing doc
/// comments as attributes. We are just leveraging this pattern to trick syn into passing through
/// regular comments as inner doc comments which can be allowed anywhere. It abuses the system
/// slightly but we'll strip them back out later during scaning.
///
/// * ***comment***: Comment to inject
/// * ***inner***: Inject the given comment as an inner comment
fn comment_to_tokens(comment: &Comment, inner: bool) -> Vec<TokenTree> {
    let mut tokens: Vec<TokenTree> = vec![];

    // Spans are an optional feature in proc_macro2 that luckily syn doesn't take into account. This
    // means being unable to set them due to to being private doesn't matter.
    let token: TokenTree = Punct::new('#', Spacing::Alone).into();
    tokens.push(token);

    if inner {
        let token: TokenTree = Punct::new('!', Spacing::Alone).into();
        tokens.push(token);
    }

    // Create and log new comment group
    let mut stream = vec![];

    let token: TokenTree = Ident::new(&comment.attr_name(), Span::call_site()).into();
    stream.push(token);

    let token: TokenTree = Punct::new('=', Spacing::Alone).into();
    stream.push(token);

    let token: TokenTree = Literal::string(&comment.text()).into();
    stream.push(token);

    tokens.push(
        Group::new(
            Delimiter::Bracket,
            TokenStream::from_iter::<Vec<TokenTree>>(stream),
        )
        .into(),
    );

    tokens
}

/// Parse comments from the given source range
///
/// * ***src***: The source to extract comments from
/// * ***trailing***: Potential for trailing comments
fn parse_comments(src: Option<&str>, trailing: bool) -> Option<Vec<Comment>> {
    let src = src?;
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

    let mut iter = src.chars().peekable();
    while let Some(mut char) = iter.next() {
        line.push(char);

        if char == '\n' {
            // Drop carriage returns in favor of newlines
            if prev_char == '\r' {
                line.pop();
            }

            // It is expected to get a single newline after source which we need to drop
            if !single_expected_newline && empty && trailing {
                comments.push(Comment::Break);
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

                if empty {
                    comments.push(Comment::Empty)
                } else {
                    // A trailing comment won't ever have a preceding comment as the preceding string
                    // is source code if we actually have some preceding source code.
                    if comments.is_empty() && trailing {
                        comments.push(Comment::line_trailing(&line));
                    } else {
                        comments.push(Comment::line(&line));
                    }
                };
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
                        comments.push(Comment::block(&line));
                    } else {
                        comments.push(Comment::block_inline(&line));
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
    use crate::model::{pos, Position};
    use core::panic;
    use indoc::indoc;
    use itertools::{peek_nth, PeekNth};
    use proc_macro2::{Group, Literal, TokenStream};
    use std::str::FromStr;
    use tracing_test::traced_test;

    trait TokenTestsExt {
        fn as_group(self) -> Group;
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
        fn print(&self);
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

        /// Recursively count all tokens
        fn print(&self) {
            let mut iter = self.clone();
            fn recurse(iter: &mut dyn Iterator<Item = TokenTree>) {
                for token in iter {
                    if let TokenTree::Group(group) = &token {
                        println!("{}", TokenWrap::GroupStart(token.clone()).to_str());
                        recurse(&mut group.stream().into_iter());
                        println!("{}", TokenWrap::GroupEnd(token.clone()).to_str());
                    } else {
                        println!("{}", TokenWrap::Token(token).to_str());
                    }
                }
            }
            recurse(&mut iter)
        }
    }

    // #[traced_test]
    // #[test]
    // fn test_trailing_variant() {
    //     let source = indoc! {r#"
    //         enum Foo {
    //             A, // A variant
    //         }
    //     "#};
    //     let tokens = inject(&Config::default(), source).unwrap().into_iter();
    //     assert_eq!(tokens.comment_count(), 1);
    //     let group = tokens.get((0, 9)).as_group();
    //     let tokens = group.stream().into_iter();
    //     assert_eq!(
    //         tokens.comments_after((1, 5)),
    //         vec![Comment::line_trailing(" A variant".into())]
    //     );
    // }

    // #[test]
    // fn test_trailing_regular_single() {
    //     let source = indoc! {r#"
    //         struct Foo; // A struct
    //     "#};
    //     let tokens = inject(&Config::default(), source).unwrap().into_iter();
    //     assert!(syn::parse2::<syn::File>(TokenStream::from_iter(tokens.clone())).is_ok());
    //     assert_eq!(tokens.clone().comment_count(), 1);
    //     assert_eq!(
    //         tokens.comments_after((0, 10)),
    //         vec![Comment::line_trailing(" A struct".into())]
    //     );
    // }

    // #[test]
    // fn test_trailing_field_multiple() {
    //     let source = indoc! {r#"
    //         struct Foo {
    //             a: i32, // A field
    //             b: i32, // B field
    //         }
    //     "#};
    //     let tokens = inject(&Config::default(), source).unwrap().into_iter();
    //     assert!(syn::parse2::<syn::File>(TokenStream::from_iter(tokens.clone())).is_ok());
    //     assert_eq!(tokens.comment_count(), 2);
    //     let group = tokens.get((0, 11)).as_group();
    //     let tokens = group.stream().into_iter();
    //     assert_eq!(
    //         tokens.comments_after((1, 10)),
    //         vec![Comment::line_trailing(" A field".into())]
    //     );
    //     assert_eq!(
    //         tokens.comments_after((2, 10)),
    //         vec![Comment::line_trailing(" B field".into())]
    //     );
    // }

    // #[test]
    // fn test_trailing_field_single() {
    //     let source = indoc! {r#"
    //         struct Foo {
    //             a: i32, // A field
    //         }
    //     "#};
    //     let tokens = inject(&Config::default(), source).unwrap().into_iter();
    //     assert!(syn::parse2::<syn::File>(TokenStream::from_iter(tokens.clone())).is_ok());
    //     assert_eq!(tokens.comment_count(), 1);
    //     assert_eq!(tokens.recursive_count(), 16);
    //     let group = tokens.get((0, 11)).as_group();
    //     let tokens = group.stream().into_iter();
    //     assert_eq!(
    //         tokens.comments_after((1, 10)),
    //         vec![Comment::line_trailing(" A field".into())]
    //     );
    // }

    // #[test]
    // fn test_skip_doc_comments() {
    //     let source = indoc! {r#"

    //          /// A foo struct
    //         struct Foo {

    //             //     Indented comment
    //             a: i32,

    //             // Field b
    //             b: i32,
    //         }
    //     "#};

    //     // Check the tokens that were generated
    //     let tokens = inject(&Config::default(), source).unwrap().into_iter();
    //     assert!(syn::parse2::<syn::File>(TokenStream::from_iter(tokens.clone())).is_ok());

    //     // Check total comments
    //     assert_eq!(tokens.comment_count(), 5);

    //     // Check first after the use statement
    //     assert_eq!(tokens.comments_before((1, 1)), vec![Comment::empty()]);

    //     // Get the group at postiion which you can see with tracing output
    //     let group = tokens.get((2, 11)).as_group();
    //     let tokens = group.stream().into_iter();

    //     assert_eq!(
    //         tokens.comments_before((5, 4)),
    //         vec![
    //             Comment::empty(),
    //             Comment::line("     Indented comment".into())
    //         ]
    //     );
    //     assert_eq!(
    //         tokens.comments_after((5, 10)),
    //         vec![Comment::empty(), Comment::line(" Field b".into())]
    //     );
    // }

    // #[test]
    // fn test_comment_blocks_include_anything_until_end() {
    //     let source = indoc! {r#"
    //         /****

    //          // Line 1
    //          * Block
    //          // Line 2

    //          ***/
    //         struct Foo;
    //     "#};
    //     let tokens = inject(&Config::default(), source).unwrap().into_iter();
    //     assert!(syn::parse2::<syn::File>(TokenStream::from_iter(tokens.clone())).is_ok());
    //     assert_eq!(tokens.comment_count(), 1);
    //     assert_eq!(
    //         tokens.comments_before((7, 0)),
    //         vec![Comment::block(
    //             "***\\n\\n // Line 1\\n * Block\\n // Line 2\\n\\n **".into()
    //         )]
    //     );
    // }

    // #[test]
    // fn test_block_inline() {
    //     let source = indoc! {r#"
    //         // Line 1
    //         struct Foo;

    //         /* Block line */
    //         println!("{}", /* Block inline */ other);
    //     "#};

    //     let tokens = inject(&Config::default(), source).unwrap().into_iter();
    //     assert!(syn::parse2::<syn::File>(TokenStream::from_iter(tokens.clone())).is_ok());
    //     assert_eq!(tokens.comment_count(), 4);
    //     assert_eq!(
    //         tokens.comments_before((1, 0)),
    //         vec![Comment::line(" Line 1".into())]
    //     );
    //     assert_eq!(
    //         tokens.comments_after((1, 10)),
    //         vec![Comment::empty(), Comment::block(" Block line ".into())]
    //     );

    //     // Get inner block group
    //     let group = tokens.get((4, 8)).as_group();
    //     let tokens = group.stream().into_iter();
    //     assert_eq!(
    //         tokens.comments_after((4, 13)),
    //         vec![Comment::block_inline(" Block inline ".into())]
    //     );
    // }

    #[traced_test]
    #[test]
    fn test_trailing_comments() {
        let source = indoc! {r#"
            struct Foo { // A foo struct
                a: i32,  // Field a
                b: i32,  // Field b
            }
        "#};
        let tokens = inject(&Config::default(), source).unwrap().into_iter();
        // assert!(syn::parse2::<syn::File>(TokenStream::from_iter(tokens.clone())).is_ok());
        tokens.print();

        // // Get the group at postiion which you can see with tracing output
        // let group = tokens.get((0, 11)).as_group();
        // let tokens = group.stream().into_iter();

        // assert_eq!(tokens.comment_count(), 2);
        // assert_eq!(tokens.recursive_count(), 22);

        // // Check that the first comment
        // assert_eq!(
        //     tokens.comments_after((1, 10)),
        //     vec![Comment::line_trailing(" Field a".into())]
        // );
        // assert_eq!(
        //     tokens.comments_after((2, 10)),
        //     vec![Comment::line_trailing(" Field b".into())]
        // );
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
        assert_eq!(tokens.comments_after((0, 16)), vec![Comment::empty()]);

        // Get the group at postiion which you can see with tracing output
        let group = tokens.get((2, 24)).as_group();
        let tokens = group.stream().into_iter();

        // Check second and third after the subscriber call
        assert_eq!(
            tokens.comments_after((6, 64)),
            vec![
                Comment::empty(),
                Comment::line(" Pass in an example".into())
            ]
        );

        // Check the last before the Ok call
        assert_eq!(tokens.comments_after((9, 33)), vec![Comment::empty()]);
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
            vec![Comment::line(" A foo struct".into())]
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
                Comment::line(" Only comments 1".into()),
                Comment::line(" Only comments 2".into())
            ]
        );
    }

    #[test]
    fn test_mixing_comment_line_and_inner_and_outer_success() {
        let source = indoc! {r#"
            //! Inner
            
            // Regular
            
            /// Outer
            struct Foo;
        "#};
        let tokens = inject(&Config::default(), source).unwrap().into_iter();
        assert!(syn::parse2::<syn::File>(TokenStream::from_iter(tokens.clone())).is_ok());

        assert_eq!(tokens.comment_count(), 3);
        assert_eq!(
            tokens.comments_before((4, 0)),
            vec![
                Comment::empty(),
                Comment::line(" Regular".into()),
                Comment::empty(),
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
        assert_eq!(tokens.comments_after((0, 8)), vec![Comment::empty()]);
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
        assert_eq!(tokens.comments_before((2, 0)), vec![Comment::empty()]);
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

    fn to_tokens(input: &str) -> VecDeque<TokenWrap> {
        expand_tokens(TokenStream::from_str(input).unwrap())
    }

    #[test]
    fn test_span_spans() {
        // None
        let tokens = to_tokens(indoc! {r#" "#});
        assert_eq!(tokens.len(), 0);

        // Partial tokens
        let tokens = to_tokens(indoc! {r#"
            enum
        "#});
        assert_eq!(tokens.len(), 1);
        matches!(tokens.get(0).unwrap(), TokenWrap::Token(_));
        assert_eq!(tokens.get(0).unwrap().span_open(), (pos(0, 0), pos(0, 4)));
        assert_eq!(tokens.get(1).is_none(), true);

        // Full three tokens
        let tokens = to_tokens(indoc! {r#"
            println!("{}", "1");
        "#});

        assert_eq!(tokens.len(), 8);

        matches!(tokens.get(0).unwrap(), TokenWrap::Token(_));
        assert_eq!(tokens.get(0).unwrap().span_open(), (pos(0, 0), pos(0, 7)));

        matches!(tokens.get(1).unwrap(), TokenWrap::Token(_));
        assert_eq!(tokens.get(1).unwrap().span_open(), (pos(0, 7), pos(0, 8)));

        matches!(tokens.get(2).unwrap(), TokenWrap::GroupStart(_));
        assert_eq!(tokens.get(2).unwrap().span_open(), (pos(0, 8), pos(0, 9)));

        matches!(tokens.get(3).unwrap(), TokenWrap::Token(_));
        assert_eq!(tokens.get(3).unwrap().span_open(), (pos(0, 9), pos(0, 13)));

        matches!(tokens.get(4).unwrap(), TokenWrap::Token(_));
        assert_eq!(tokens.get(4).unwrap().span_open(), (pos(0, 13), pos(0, 14)));

        matches!(tokens.get(5).unwrap(), TokenWrap::Token(_));
        assert_eq!(tokens.get(5).unwrap().span_open(), (pos(0, 15), pos(0, 18)));

        matches!(tokens.get(6).unwrap(), TokenWrap::GroupEnd(_));
        assert_eq!(
            tokens.get(6).unwrap().span_close(),
            (pos(0, 18), pos(0, 19))
        );

        matches!(
            tokens.get(7).unwrap(),
            TokenWrap::Token(TokenTree::Punct(_))
        );
        assert_eq!(
            tokens.get(7).unwrap().span_close(),
            (pos(0, 19), pos(0, 20))
        );

        assert_eq!(tokens.get(8).is_none(), true);
    }
}
