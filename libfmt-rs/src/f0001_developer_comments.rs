// Feature F0001: Developer comments
// ---------------------------------------------------------------------------------------------
// Problem statement: Prettyplease doesn't support developer comments.
//
// Solution: Leverage the proc-macro2 support for attributes to retrofit developer comments.
// ---------------------------------------------------------------------------------------------
use crate::{
    model::{
        Comment, CommentCategory, Config, OptionTokenExt, Position, Source, TokenGroup, TokenItem,
        TokenWrap, TokenWrapExt,
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
/// recognized by the pretty printer with some modifications.
///
/// * ***config***: Configuration settings
/// * ***source***: Original source
/// * ***return***: Tokenized version of the source with injected comment tokens
pub(crate) fn inject(config: &Config, source: &str) -> Result<TokenStream> {
    // Parse source into a token stream
    let tokens = TokenStream::from_str(source)
        .map_err(|e| Error::new("failed to parse source into token stream").wrap_lex(e))?;

    // Inject comments into the token stream
    let commenter = Commenter::new(config, Source::new(source), tokens).inject();
    Ok(commenter.into_token_stream())
}

/// Newtype to allow for custom implementations
#[derive(Debug)]
pub(crate) struct Commenter<'a> {
    config: &'a Config,
    source: Source,
    groups: Vec<TokenGroup>,
    stream: VecDeque<TokenWrap>,
}

impl<'a> Commenter<'a> {
    /// Create a new instance
    pub(crate) fn new(config: &'a Config, source: Source, stream: TokenStream) -> Self {
        Self {
            config,
            source,
            stream: expand_tokens(stream),
            groups: vec![TokenGroup::new(None)],
        }
    }

    /// Inject comments into the token stream from the source
    pub(crate) fn inject(mut self) -> Self {
        // Only comments in file
        if self.is_empty() && self.stream.is_empty() {
            let src = self.source.range(None, None);
            if let Some(comments) = parse_comments(self.config, src.as_deref(), false) {
                self.append_curr_comments(comments, true);
            }
        }

        while let Some(curr) = self.stream.pop_front() {
            let (start, mut end) = curr.span();

            // Leading comments - haven't processed any tokens yet
            if self.is_empty() {
                let src = self.source.range(None, Some(start));
                if let Some(comments) = parse_comments(self.config, src.as_deref(), false) {
                    self.append_curr_comments(comments, false);
                }
            }

            // Start a new group
            if curr.is_group_start() {
                end = self.inject_comments(end, CommentCategory::Trailing);
                self.append_group(curr.take());
                self.inject_comments(end, CommentCategory::Regular);
            } else {
                // Pass proc_macro2 parsed doc comments directly through without processing
                if self.is_doc_comment(&curr) {
                    self.append_curr(curr.take());
                    self.pass_doc_comments(end);
                    self.complete_line(true);

                // End a group
                } else if curr.is_group_end() {
                    self.complete_group(&curr);

                // Store regular tokens
                } else {
                    self.append_curr(curr.take());
                }
                self.inject_comments(end, CommentCategory::Both);
            }
        }

        self.complete();
        self
    }

    /// Check the given range and inject any found comments in the appropriate lines. If only
    /// searching for trailing comments then advance the trailing comments line end to avoid
    /// duplicating them later.
    ///
    /// * ***curr***: Current token
    /// * ***start***: Start position of the range
    /// * ***return***: End position of the current line or if no comments are found the original start
    fn inject_comments(&mut self, start: Position, category: CommentCategory) -> Position {
        // Determine the correct end of this range based on the next tokens's start.
        let end = self.stream.front().span().0;
        let src = self.source.range(Some(start), end);

        if let Some(comments) = parse_comments(self.config, src.as_deref(), true) {
            // Trailing comments also act as line breaks
            let newline = comments.iter().any(|x| x.is_break() || x.is_trailing());

            // Add trailing comments to begining of current line
            if category == CommentCategory::Trailing || category == CommentCategory::Both {
                let comments: Vec<Comment> = comments
                    .clone()
                    .into_iter()
                    .filter(|x| x.is_trailing())
                    .collect();
                if !comments.is_empty() {
                    self.prepend_curr_comments(comments, false);

                    // Advance in the trailing only case
                    if category == CommentCategory::Trailing {
                        if let Some(end) = end {
                            return self.source.inc_past_newline(start, end);
                        }
                    }
                }
            }

            // Add regular comments between tokens to next line
            if category == CommentCategory::Regular || category == CommentCategory::Both {
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
        start
    }

    /// Check if the token is a doc comment
    fn is_doc_comment(&self, wrap: &TokenWrap) -> bool {
        if let TokenWrap::Token(token) = wrap {
            let (start, _) = wrap.span();

            if let TokenTree::Punct(punct) = token {
                if punct.as_char() == '#' && self.source.char_at_is(start, '/') {
                    return true;
                }
            }
        }
        false
    }

    /// Pass through doc comments as is
    /// * ***end***: End position of the doc comment
    fn pass_doc_comments(&mut self, end: Position) {
        let mut in_group = false;

        while let Some(token) = self.stream.pop_front() {
            let (start, _) = token.span();
            if start < end {
                trace!("{}", token.to_str());
                if !in_group || token.is_group_start() {
                    in_group = token.is_group_start();
                    self.append_curr(token.take());
                } else if let TokenWrap::GroupEnd(_) = token {
                    break;
                }
            } else {
                break;
            }
        }
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
        let group_end_str = group.to_str();

        // Create a new TokenTree::Group to capture group token changes
        if let Some(mut group) = self.groups.pop() {
            if let Some(TokenTree::Group(token)) = group.token.clone() {
                group.complete();
                trace!("{}", group_end_str);
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
        if !self.config.developer_comments() {
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
        if !self.config.developer_comments() {
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
        if !self.config.developer_comments() {
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
/// * ***config***: The configuration to honor
/// * ***src***: The source to extract comments from
/// * ***some***: There is at least one token in the source
fn parse_comments(config: &Config, src: Option<&str>, some: bool) -> Option<Vec<Comment>> {
    let src = src?;
    let mut comments: Vec<Comment> = vec![]; // final results
    let mut line = String::new(); // temp buffer

    // Track throughout
    let mut single_expected_newline = false;
    let mut prev_empty_lines = 0;
    let mut comment_block = false;

    // Reset on each newline
    let mut comment_line = false;
    let mut only_space = true;
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

        // Drop carriage returns in favor of newlines
        if char == '\r' {
            line.pop();
            line.push('\n');
        } else if prev_char == '\r' && char == '\n' {
            line.pop();
            continue;
        }

        // Newline signals action needs taken
        if char == '\n' {
            // Block comment has not be closed yet, so everything is part of it
            if comment_block {
                continue;

            // Detect comments first
            } else if comment_line {
                line.pop(); // drop newline

                // A trailing comment won't ever have a preceding comment as the preceding string.
                // The some param indicates we have some source code preceding.
                if comments.is_empty() && some {
                    comments.push(Comment::line_trailing(&line));
                    comments.push(Comment::Break);
                    single_expected_newline = true;
                } else {
                    comments.push(Comment::line(&line));
                }
            } else if only_space {
                // It is expected to get a single newline after source which we need to drop
                if !single_expected_newline && some {
                    comments.push(Comment::Break);
                    single_expected_newline = true;

                // Only allow the specified amount of empty consecutive lines
                } else {
                    let allowed = match &config.num_empty_lines_allowed {
                        Some(n) => match *n {
                            n if prev_empty_lines < n => true,
                            _ => false,
                        },
                        _ => true,
                    };

                    prev_empty_lines += 1;
                    if allowed {
                        comments.push(Comment::Empty);
                    }
                }
            } else {
                prev_empty_lines = 0;
            }

            reset(
                &mut line,
                &mut only_space,
                &mut comment_line,
                &mut prev_char,
            );
            continue;
        } else if char != ' ' {
            only_space = false;
            prev_empty_lines = 0;

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
                    reset(
                        &mut line,
                        &mut only_space,
                        &mut comment_line,
                        &mut prev_char,
                    );
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
    use crate::model::{pos, Position, TokenTreeExt};
    use core::panic;
    use indoc::indoc;
    use itertools::{peek_nth, PeekNth};
    use proc_macro2::{Group, Literal, TokenStream};
    use std::str::FromStr;
    use tracing_test::traced_test;

    #[test]
    fn if_else_block_comment() {
        let source = indoc! {r#"
            fn main() {
                // If comment
                if true {
                    foo();

                // Else comment
                } else {
                    foo();
                }
            }
        "#};

        // Test comment injection
        let tokens = inject(&Config::default(), source).unwrap().into_iter();
        assert!(syn::parse2::<syn::File>(TokenStream::from_iter(tokens.clone())).is_ok());

        assert_eq!(tokens.comment_count(), 3);
        let group = tokens.get((2, 12)).as_group();
        let tokens: Vec<TokenTree> = group.stream().into_iter().collect();
        assert_eq!(tokens.len(), 10);

        assert_eq!(tokens[0].is_ident("foo"), true);
        assert_eq!(tokens[1].is_group(), true);
        assert_eq!(tokens[2].is_punct(';'), true);
        assert_eq!(tokens[3].is_comment_punct(), true);
        assert_eq!(tokens[4].is_comment_group(), true);
        assert_eq!(tokens[5].is_comment_punct(), true);
        assert_eq!(tokens[6].get_comment(), " Else comment");
        assert_eq!(tokens[7].is_ident("struct"), true);
        assert_eq!(tokens[8].is_ident(crate::DUMMY_STRUCT), true);
        assert_eq!(tokens[9].is_punct(';'), true);

        // Test final comment printing
        assert_eq!(
            crate::format_str(None, source).unwrap(),
            indoc! {r#"
                fn main() {
                    // If comment
                    if true {
                        foo();

                    // Else comment
                    } else {
                        foo();
                    }
                }
             "#},
        );
    }

    #[test]
    fn block_scope_comment() {
        let source = indoc! {r#"
            fn main() {
                // Block comment
                {
                    foo();
                }
            }
        "#};

        // Test comment injection
        let tokens = inject(&Config::default(), source).unwrap().into_iter();
        assert!(syn::parse2::<syn::File>(TokenStream::from_iter(tokens.clone())).is_ok());

        assert_eq!(tokens.comment_count(), 1);
        let group = tokens.get((0, 10)).as_group();
        let tokens = group.stream().into_iter();
        assert_eq!(
            tokens.comments_before((2, 4)),
            vec![Comment::line(" Block comment".into())]
        );

        // Test final comment printing
        assert_eq!(
            crate::format_str(None, source).unwrap(),
            indoc! {r#"
                fn main() {
                    // Block comment
                    {
                        foo();
                    }
                }
             "#},
        );
    }

    #[test]
    fn trailing_fn_call() {
        let source = indoc! {r#"
            fn main() {
                foo(); // Trailing fn
            }
        "#};

        // Test comment injection
        let tokens = inject(&Config::default(), source).unwrap().into_iter();
        assert!(syn::parse2::<syn::File>(TokenStream::from_iter(tokens.clone())).is_ok());

        assert_eq!(tokens.comment_count(), 1);
        let group = tokens.get((0, 10)).as_group();
        let tokens = group.stream().into_iter();
        assert_eq!(
            tokens.comments_before((1, 4)),
            vec![Comment::line_trailing(" Trailing fn".into())]
        );

        // Test final comment printing
        assert_eq!(
            crate::format_str(None, source).unwrap(),
            indoc! {r#"
                fn main() {
                    foo(); // Trailing fn
                }
            "#},
        );
    }

    #[test]
    fn trailing_subexpr_method_call() {
        let source = indoc! {r#"
            fn main() {
                self.foo(); // Trailing self.fn
            }
        "#};

        // Test comment injection
        let tokens = inject(&Config::default(), source).unwrap().into_iter();
        assert!(syn::parse2::<syn::File>(TokenStream::from_iter(tokens.clone())).is_ok());

        assert_eq!(tokens.comment_count(), 1);
        let group = tokens.get((0, 10)).as_group();
        let tokens = group.stream().into_iter();
        assert_eq!(
            tokens.comments_before((1, 4)),
            vec![Comment::line_trailing(" Trailing self.fn".into())]
        );

        // Test final comment printing
        assert_eq!(
            crate::format_str(None, source).unwrap(),
            indoc! {r#"
                fn main() {
                    self.foo(); // Trailing self.fn
                }
            "#},
        );
    }

    #[test]
    fn comment_after_group_pp() {
        let source = indoc! {r#"
            fn main() {
                if true {
                    std::process::exit(1);
                }

                // Comment after group
                struct foo;
            }
        "#};
        assert_eq!(
            crate::format_str(None, source).unwrap(),
            indoc! {r#"
                fn main() {
                    if true {
                        std::process::exit(1);
                    }

                    // Comment after group
                    struct foo;
                }
            "#},
        );
    }

    #[test]
    fn inject_dummy_struct_nested_pp() {
        let source = indoc! {r#"
            fn main() {
                {
                    // placeholder
                }
            }
        "#};
        assert_eq!(
            crate::format_str(None, source).unwrap(),
            indoc! {r#"
                fn main() {
                    {
                        // placeholder
                    }
                }
            "#},
        );
    }

    #[test]
    fn inject_dummy_struct_pp() {
        let source = indoc! {r#"
            fn main() {
                if args.len() != 2 {
                    // placeholder
                }
            }
        "#};
        assert_eq!(
            crate::format_str(None, source).unwrap(),
            indoc! {r#"
                fn main() {
                    if args.len() != 2 {
                        // placeholder
                    }
                }
            "#},
        );
    }

    #[test]
    fn allow_func_separation_from_body() {
        // Honor my style - don't remove my newline
        let source = indoc! {r#"
            fn print() {

                // A newline above this first block of code is often a nice visual separation
                println!("Hello");
            }
        "#};
        assert_eq!(
            crate::format_str(None, source).unwrap(),
            indoc! {r#"
                fn print() {

                    // A newline above this first block of code is often a nice visual separation
                    println!("Hello");
                }
            "#},
        );

        // Honor my style - don't add a newline, just do what I want
        let source = indoc! {r#"
            fn print() {
                println!("Hello");
            }
        "#};
        assert_eq!(
            crate::format_str(None, source).unwrap(),
            indoc! {r#"
                fn print() {
                    println!("Hello");
                }
            "#},
        );
    }

    #[test]
    fn comment_trailing_item_macro() {
        let source = indoc! {r#"
            println!("Hello"); // Hello
        "#};
        assert_eq!(
            crate::format_str(None, source).unwrap(),
            indoc! {r#"
                println!("Hello"); // Hello
            "#},
        );
    }

    #[test]
    fn comment_trailing_item_trait() {
        let source = indoc! {r#"
            trait Foo { // Foo
                type Item; // Bar

                const FOO: i32 = 42; // Foo
                fn foo(); // Func foo
                fn foo2() { // Func foo2
                    println!("Hello");
                }
            }
        "#};
        assert_eq!(
            crate::format_str(None, source).unwrap(),
            indoc! {r#"
                trait Foo { // Foo
                    type Item; // Bar

                    const FOO: i32 = 42; // Foo
                    fn foo(); // Func foo
                    fn foo2() { // Func foo2
                        println!("Hello");
                    }
                }
            "#},
        );
    }

    #[test]
    fn comment_trailing_item_struct() {
        let source = indoc! {r#"
            struct Foo1; // Foo1 struct
            struct Foo2 { // Foo2 struct
                a: i32,     // A field
                b: i32,     // B field
            }
        "#};
        assert_eq!(
            crate::format_str(None, source).unwrap(),
            indoc! {r#"
                struct Foo1; // Foo1 struct
                struct Foo2 { // Foo2 struct
                    a: i32, // A field
                    b: i32, // B field
                }
            "#},
        );
    }

    #[test]
    fn comment_trailing_item_static() {
        let source = indoc! {r#"
            static FOO: i32 = 42; // Foo
        "#};
        assert_eq!(
            crate::format_str(None, source).unwrap(),
            indoc! {r#"
                static FOO: i32 = 42; // Foo
            "#},
        );
    }

    #[test]
    fn comment_trailing_item_mod() {
        let source = indoc! {r#"
            mod foo; // Foo
            mod foo2 { // Foo
                fn foo() {
                    println!("Hello");
                }
            }
        "#};
        assert_eq!(
            crate::format_str(None, source).unwrap(),
            indoc! {r#"
                mod foo; // Foo
                mod foo2 { // Foo
                    fn foo() {
                        println!("Hello");
                    }
                }
            "#},
        );
    }

    #[test]
    fn comment_trailing_item_impl() {
        let source = indoc! {r#"
            struct Foo;

            impl Foo { // Foo
                fn foo() {
                    println!("Hello");
                }
            }
        "#};
        assert_eq!(
            crate::format_str(None, source).unwrap(),
            indoc! {r#"
                struct Foo;

                impl Foo { // Foo
                    fn foo() {
                        println!("Hello");
                    }
                }
            "#},
        );
    }

    #[test]
    fn comment_trailing_item_func() {
        let source = indoc! {r#"
            fn foo() { // Hello
                println!("Hello");
            }
        "#};
        assert_eq!(
            crate::format_str(None, source).unwrap(),
            indoc! {r#"
                fn foo() { // Hello
                    println!("Hello");
                }
            "#},
        );
    }

    #[test]
    fn comment_trailing_item_enum() {
        let source = indoc! {r#"
            enum Enum2 { // Enum2
                A, // A variant
                B, // B variant
            }
        "#};
        assert_eq!(
            crate::format_str(None, source).unwrap(),
            indoc! {r#"
                enum Enum2 { // Enum2
                    A, // A variant
                    B, // B variant
                }
            "#},
        );
    }

    #[test]
    fn comment_trailing_const() {
        let source = indoc! {r#"
            const FOO: i32 = 42; // Foo
        "#};
        assert_eq!(
            crate::format_str(None, source).unwrap(),
            indoc! {r#"
                const FOO: i32 = 42; // Foo
            "#},
        );
    }

    #[test]
    fn comment_only_comments() {
        let source = indoc! {r#"
            // foo
        "#};
        assert_eq!(
            crate::format_str(None, source).unwrap(),
            indoc! {r#"
                // foo
            "#},
        );
    }

    #[test]
    fn multi_comment_types() {
        let source = indoc! {r#"
            use libfmt_rs::Result;

            fn main() -> Result<()> {
                let subscriber = FmtSubscriber::builder()
                    .with_max_level(Level::TRACE)
                    .finish();
                tracing::subscriber::set_global_default(subscriber).unwrap();

                // Pass in an example
                let path = "examples/dump.rs";
                let formatted = libfmt_rs::format_file(path)?;
                print!("{}", formatted);

                Ok(())
            }
        "#};
        assert_eq!(
            crate::format_str(None, source).unwrap(),
            indoc! {r#"
                use libfmt_rs::Result;

                fn main() -> Result<()> {
                    let subscriber = FmtSubscriber::builder().with_max_level(Level::TRACE).finish();
                    tracing::subscriber::set_global_default(subscriber).unwrap();

                    // Pass in an example
                    let path = "examples/dump.rs";
                    let formatted = libfmt_rs::format_file(path)?;
                    print!("{}", formatted);

                    Ok(())
                }
        "#}
        );
    }

    #[test]
    fn block_comment() {
        let source = indoc! {r#"
            /**************************
             * ///  A foo struct  \\\ *
             *  - one                 *
             *  - two                 *
             *************************/
            struct Foo;
        "#};
        assert_eq!(
            crate::format_str(None, source).unwrap(),
            indoc! {r#"
                /**************************
                 * ///  A foo struct  \\\ *
                 *  - one                 *
                 *  - two                 *
                 *************************/
                struct Foo;
            "#},
        );
    }

    #[test]
    fn struct_definition_with_comments_and_whitespace() {
        let source = indoc! {r#"

            /// A foo struct
            struct Foo {

                // Field a
                a: i32,

                // Field b
                b: i32,
            }
        "#};
        assert_eq!(
            crate::format_str(None, source).unwrap(),
            indoc! {r#"

                /// A foo struct
                struct Foo {

                    // Field a
                    a: i32,

                    // Field b
                    b: i32,
                }
            "#},
        );
    }

    #[test]
    fn allow_any_num_of_empty_lines() {
        let source = indoc! {r#"



            println!("{}", "1",);
        "#};

        assert_eq!(
            crate::format_str(None, source).unwrap(),
            indoc! {r#"



                println!("{}", "1");
            "#}
        );
    }

    #[test]
    fn only_allow_one_empty_line() {
        let source = indoc! {r#"


            println!("{}", "1",);
        "#};

        assert_eq!(
            crate::format_str(
                Some(Config::new().with_num_empty_lines_allowed(Some(1))),
                source
            )
            .unwrap(),
            indoc! {r#"

                println!("{}", "1");
            "#}
        );
    }

    trait TokenTestsExt {
        fn is_ident(&self, name: &str) -> bool;
        fn is_punct(&self, name: char) -> bool;
        fn is_group(&self) -> bool;
        fn get_comment(&self) -> String;
        fn as_group(self) -> Group;
        fn as_literal(self) -> Literal;
        fn to_comment(&self) -> Option<Comment>;
    }
    impl TokenTestsExt for TokenTree {
        fn is_punct(&self, name: char) -> bool {
            if let TokenTree::Punct(punct) = self {
                punct.as_char() == name
            } else {
                false
            }
        }
        fn is_ident(&self, name: &str) -> bool {
            if let TokenTree::Ident(ident) = self {
                ident.to_string() == name
            } else {
                false
            }
        }
        fn is_group(&self) -> bool {
            if let TokenTree::Group(_) = self {
                true
            } else {
                false
            }
        }
        fn get_comment(&self) -> String {
            if let TokenTree::Group(group) = self {
                let tokens: Vec<TokenTree> = group.stream().into_iter().collect();
                if tokens.len() == 3 {
                    if let TokenTree::Literal(lit) = &tokens[2] {
                        let str = lit.to_string();
                        if str.len() > 1 {
                            return str[1..str.len() - 1].to_string();
                        }
                        return str;
                    }
                }
            }
            "".into()
        }
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
        #[allow(dead_code)]
        fn print(&self);
    }
    impl<I: Iterator<Item = TokenTree> + Clone> TokensExt for I {
        fn get<P: Into<Position>>(&self, pos: P) -> TokenTree {
            let pos = pos.into();
            let stream = TokenStream::from_iter(self.clone());
            let flattened = expand_tokens(stream);
            for x in flattened.into_iter() {
                let pos2 = x.span().0;
                if pos == pos2 {
                    return (*x).clone();
                }
            }
            panic!("Group at position {:?} not found", pos);
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

    // Originally I was seeing the comment show up in the group
    #[test]
    fn comment_after_group() {
        let source = indoc! {r#"
            fn main() {
                if true {
                    std::process::exit(1);
                }

                // Comment after group
                struct foo;
            }
        "#};

        let tokens = inject(&Config::default(), source).unwrap().into_iter();
        assert!(syn::parse2::<syn::File>(TokenStream::from_iter(tokens.clone())).is_ok());
        tokens.print();

        assert_eq!(tokens.comment_count(), 2);
        let group = tokens.get((0, 10)).as_group();
        let tokens = group.stream().into_iter();
        assert_eq!(
            tokens.comments_before((6, 4)),
            vec![
                Comment::empty(),
                Comment::line(" Comment after group".into())
            ]
        );
    }

    #[test]
    fn inject_dummy_struct_nested() {
        let source = indoc! {r#"
            fn main() {
                {
                    // placeholder
                }
            }
        "#};

        let tokens = inject(&Config::default(), source).unwrap().into_iter();
        assert!(syn::parse2::<syn::File>(TokenStream::from_iter(tokens.clone())).is_ok());

        assert_eq!(tokens.comment_count(), 1);
        let group = tokens.get((1, 4)).as_group();
        let tokens: Vec<TokenTree> = group.stream().into_iter().collect();
        assert_eq!(tokens.len(), 5);

        assert_eq!(tokens[0].is_comment_punct(), true);
        assert_eq!(tokens[1].get_comment(), " placeholder");
        assert_eq!(tokens[2].is_ident("struct"), true);
        assert_eq!(tokens[3].is_ident(crate::DUMMY_STRUCT), true);
        assert_eq!(tokens[4].is_punct(';'), true);
    }

    #[test]
    fn inject_dummy_struct() {
        let source = indoc! {r#"
            fn main() {
                // placeholder
            }
        "#};

        let tokens = inject(&Config::default(), source).unwrap().into_iter();
        assert!(syn::parse2::<syn::File>(TokenStream::from_iter(tokens.clone())).is_ok());

        assert_eq!(tokens.comment_count(), 1);
        let group = tokens.get((0, 10)).as_group();
        let tokens: Vec<TokenTree> = group.stream().into_iter().collect();
        assert_eq!(tokens.len(), 5);

        assert_eq!(tokens[0].is_comment_punct(), true);
        assert_eq!(tokens[1].is_comment_group(), true);
        assert_eq!(tokens[2].is_ident("struct"), true);
        assert_eq!(tokens[3].is_ident(crate::DUMMY_STRUCT), true);
        assert_eq!(tokens[4].is_punct(';'), true);
    }

    #[test]
    fn trailing_variant() {
        let source = indoc! {r#"
            enum Foo {
                A, // A variant
            }
        "#};
        let tokens = inject(&Config::default(), source).unwrap().into_iter();
        assert!(syn::parse2::<syn::File>(TokenStream::from_iter(tokens.clone())).is_ok());

        assert_eq!(tokens.comment_count(), 1);
        let group = tokens.get((0, 9)).as_group();
        let tokens = group.stream().into_iter();
        assert_eq!(
            tokens.comments_before((1, 4)),
            vec![Comment::line_trailing(" A variant".into())]
        );
    }

    #[test]
    fn trailing_regular_single() {
        let source = indoc! {r#"
            struct Foo; // A struct
        "#};
        let tokens = inject(&Config::default(), source).unwrap().into_iter();
        assert!(syn::parse2::<syn::File>(TokenStream::from_iter(tokens.clone())).is_ok());
        assert_eq!(tokens.clone().comment_count(), 1);
        assert_eq!(
            tokens.comments_before((0, 0)),
            vec![Comment::line_trailing(" A struct".into())]
        );
    }

    #[test]
    fn trailing_field_multiple() {
        let source = indoc! {r#"
            struct Foo {
                a: i32, // A field

                b: i32, // B field
            }
        "#};
        let tokens = inject(&Config::default(), source).unwrap().into_iter();
        assert!(syn::parse2::<syn::File>(TokenStream::from_iter(tokens.clone())).is_ok());

        assert_eq!(tokens.comment_count(), 3);
        let group = tokens.get((0, 11)).as_group();
        let tokens = group.stream().into_iter();
        assert_eq!(
            tokens.comments_before((1, 4)),
            vec![Comment::line_trailing(" A field".into())]
        );
        assert_eq!(
            tokens.comments_before((3, 4)),
            vec![Comment::line_trailing(" B field".into()), Comment::Empty]
        );
    }

    #[test]
    fn trailing_field_single() {
        let source = indoc! {r#"
            struct Foo {
                a: i32, // A field
            }
        "#};
        let tokens = inject(&Config::default(), source).unwrap().into_iter();
        assert!(syn::parse2::<syn::File>(TokenStream::from_iter(tokens.clone())).is_ok());

        assert_eq!(tokens.comment_count(), 1);
        let group = tokens.get((0, 11)).as_group();
        let tokens = group.stream().into_iter();
        assert_eq!(
            tokens.comments_before((1, 4)),
            vec![Comment::line_trailing(" A field".into())]
        );
    }

    #[test]
    fn skip_doc_comments() {
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
        assert_eq!(tokens.comments_before((1, 1)), vec![Comment::empty()]);

        // Get the group at postiion which you can see with tracing output
        let group = tokens.get((2, 11)).as_group();
        let tokens = group.stream().into_iter();

        assert_eq!(
            tokens.comments_before((5, 4)),
            vec![
                Comment::empty(),
                Comment::line("     Indented comment".into())
            ]
        );
        assert_eq!(
            tokens.comments_after((5, 10)),
            vec![Comment::empty(), Comment::line(" Field b".into())]
        );
    }

    #[test]
    fn comment_blocks_include_anything_until_end() {
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
            vec![Comment::block(
                "***\\n\\n // Line 1\\n * Block\\n // Line 2\\n\\n **".into()
            )]
        );
    }

    #[test]
    fn block_inline() {
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
            vec![Comment::line(" Line 1".into())]
        );
        assert_eq!(
            tokens.comments_after((1, 10)),
            vec![Comment::empty(), Comment::block(" Block line ".into())]
        );

        // Get inner block group
        let group = tokens.get((4, 8)).as_group();
        let tokens = group.stream().into_iter();
        assert_eq!(
            tokens.comments_after((4, 13)),
            vec![Comment::block_inline(" Block inline ".into())]
        );
    }

    #[test]
    fn trailing_comments() {
        let source = indoc! {r#"
            struct Foo { // A foo struct
                a: i32,  // Field a
                b: i32,  // Field b
            }
        "#};
        let tokens = inject(&Config::default(), source).unwrap().into_iter();
        assert!(syn::parse2::<syn::File>(TokenStream::from_iter(tokens.clone())).is_ok());
        // tokens.print();
        assert_eq!(tokens.comment_count(), 3);

        assert_eq!(
            tokens.comments_before((0, 0)),
            vec![Comment::line_trailing(" A foo struct".into())]
        );

        // Get the group at postiion which you can see with tracing output
        let group = tokens.get((0, 11)).as_group();
        let tokens = group.stream().into_iter();
        assert_eq!(tokens.comment_count(), 2);
        assert_eq!(
            tokens.comments_before((1, 4)),
            vec![Comment::line_trailing(" Field a".into())]
        );
        assert_eq!(
            tokens.comments_before((2, 4)),
            vec![Comment::line_trailing(" Field b".into())]
        );
    }

    #[test]
    fn multi_comment_types_token_level() {
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
    fn simple_outer_comment() {
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
    fn only_comments() {
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
    fn mix_with_doc_comments() {
        let source = indoc! {r#"
            //! Inner
            
            // Regular
            
            /// Outer
            struct Foo;
        "#};
        let tokens = inject(&Config::default(), source).unwrap().into_iter();
        assert!(syn::parse2::<syn::File>(TokenStream::from_iter(tokens.clone())).is_ok());
        tokens.print();

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
    fn trailing_newline_is_not_considered_an_empty_line() {
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
    fn allow_any_number_empty_lines_consecutively() {
        let source = indoc! {r#"



            println!("{}", "1");
        "#};

        // Check the tokens that were generated
        let tokens = inject(&Config::default(), source).unwrap().into_iter();
        assert!(syn::parse2::<syn::File>(TokenStream::from_iter(tokens.clone())).is_ok());

        assert_eq!(tokens.comment_count(), 3);
        assert_eq!(
            tokens.comments_before((3, 0)),
            vec![Comment::empty(), Comment::empty(), Comment::empty()]
        );
    }

    #[test]
    fn only_allow_single_empty_line_consecutively() {
        let source = indoc! {r#"


            println!("{}", "1");
        "#};

        // Check the tokens that were generated
        let tokens = inject(&Config::new().with_num_empty_lines_allowed(Some(1)), source)
            .unwrap()
            .into_iter();
        assert!(syn::parse2::<syn::File>(TokenStream::from_iter(tokens.clone())).is_ok());

        assert_eq!(tokens.comment_count(), 1);
        assert_eq!(tokens.comments_before((2, 0)), vec![Comment::empty()]);
    }

    #[test]
    fn tokenstream_counts_starting_from_1() {
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
    fn span_spans() {
        // None
        let tokens = to_tokens(indoc! {r#" "#});
        assert_eq!(tokens.len(), 0);

        // Partial tokens
        let tokens = to_tokens(indoc! {r#"
            enum
        "#});
        assert_eq!(tokens.len(), 1);
        matches!(tokens.get(0).unwrap(), TokenWrap::Token(_));
        assert_eq!(tokens.get(0).unwrap().span(), (pos(0, 0), pos(0, 4)));
        assert_eq!(tokens.get(1).is_none(), true);

        // Full three tokens
        let tokens = to_tokens(indoc! {r#"
            println!("{}", "1");
        "#});

        assert_eq!(tokens.len(), 8);

        matches!(tokens.get(0).unwrap(), TokenWrap::Token(_));
        assert_eq!(tokens.get(0).unwrap().span(), (pos(0, 0), pos(0, 7)));

        matches!(tokens.get(1).unwrap(), TokenWrap::Token(_));
        assert_eq!(tokens.get(1).unwrap().span(), (pos(0, 7), pos(0, 8)));

        matches!(tokens.get(2).unwrap(), TokenWrap::GroupStart(_));
        assert_eq!(tokens.get(2).unwrap().span(), (pos(0, 8), pos(0, 9)));

        matches!(tokens.get(3).unwrap(), TokenWrap::Token(_));
        assert_eq!(tokens.get(3).unwrap().span(), (pos(0, 9), pos(0, 13)));

        matches!(tokens.get(4).unwrap(), TokenWrap::Token(_));
        assert_eq!(tokens.get(4).unwrap().span(), (pos(0, 13), pos(0, 14)));

        matches!(tokens.get(5).unwrap(), TokenWrap::Token(_));
        assert_eq!(tokens.get(5).unwrap().span(), (pos(0, 15), pos(0, 18)));

        matches!(tokens.get(6).unwrap(), TokenWrap::GroupEnd(_));
        assert_eq!(tokens.get(6).unwrap().span(), (pos(0, 18), pos(0, 19)));

        matches!(
            tokens.get(7).unwrap(),
            TokenWrap::Token(TokenTree::Punct(_))
        );
        assert_eq!(tokens.get(7).unwrap().span(), (pos(0, 19), pos(0, 20)));

        assert_eq!(tokens.get(8).is_none(), true);
    }
}
