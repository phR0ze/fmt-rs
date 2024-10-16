use super::{Comment, Position, Source};
use proc_macro2::{Delimiter, Group, Ident, Literal, Punct, Spacing, Span, TokenStream, TokenTree};
use std::{collections::VecDeque, iter::Fuse, ops::Mul};
use syn::token::Token;
use tracing::trace;

/// Extension trait for proc_macro2::TokenTree
pub(crate) trait TokenExt {
    fn is_comment_ident(&self) -> bool;
    fn is_comment_group(&self) -> bool;
    fn is_punct(&self, char: char) -> bool;
    fn to_str(&self, span: &Span) -> String;
    fn span_open(&self) -> Option<Span>;
    fn span_close(&self) -> Option<Span>;
}

impl TokenExt for Option<&TokenTree> {
    fn is_comment_ident(&self) -> bool {
        if let Some(token) = self {
            return token.is_comment_ident();
        }
        false
    }

    fn is_comment_group(&self) -> bool {
        if let Some(token) = self {
            return token.is_comment_group();
        }
        false
    }

    fn is_punct(&self, char: char) -> bool {
        if let Some(token) = self {
            return token.is_punct(char);
        }
        false
    }
    fn span_open(&self) -> Option<Span> {
        self.and_then(|x| x.span_open())
    }
    fn span_close(&self) -> Option<Span> {
        self.and_then(|x| x.span_close())
    }
    fn to_str(&self, span: &Span) -> String {
        if let Some(token) = self {
            return token.to_str(span);
        }
        "".into()
    }
}

impl TokenExt for TokenTree {
    /// Determine if the token is a comment identifier
    fn is_comment_ident(&self) -> bool {
        if let TokenTree::Ident(i) = self {
            return i.to_string().starts_with("comment_");
        }
        false
    }

    /// Determine if the token is a comment group
    fn is_comment_group(&self) -> bool {
        if let TokenTree::Group(g) = self {
            let stream = g.stream().into_iter().collect::<Vec<_>>();
            if stream.len() == 3 && stream[0].is_comment_ident() {
                return true;
            }
        }
        false
    }

    /// Determine if the token is the given punctuation character
    fn is_punct(&self, char: char) -> bool {
        match self {
            TokenTree::Punct(p) => p.as_char() == char,
            _ => false,
        }
    }

    /// Get the open span
    fn span_open(&self) -> Option<Span> {
        Some(match self {
            TokenTree::Ident(ident) => ident.span(),
            TokenTree::Literal(literal) => literal.span(),
            TokenTree::Group(group) => group.span_open(),
            TokenTree::Punct(punct) => punct.span(),
        })
    }

    /// Get the close span
    fn span_close(&self) -> Option<Span> {
        Some(match self {
            TokenTree::Ident(ident) => ident.span(),
            TokenTree::Literal(literal) => literal.span(),
            TokenTree::Group(group) => group.span_close(),
            TokenTree::Punct(punct) => punct.span(),
        })
    }

    /// Convert the token and span into a string for debugging purposes.  The span is passed in
    /// becasue we don't know in the group case if we are dealing with the open or close span from
    /// this context.
    ///
    /// * ***span*** - The span of the token to
    fn to_str(&self, span: &Span) -> String {
        let start: Position = span.start().into();
        let end: Position = span.end().into();
        let mut out = String::from(format!("{: <12}", format!("{}..{}", start, end)));

        match self {
            TokenTree::Ident(i) => out.push_str(&format!("{: <6} {}", "Ident", i.to_string())),
            TokenTree::Punct(p) => out.push_str(&format!("{: <6} {}", "Punct", p.to_string())),
            TokenTree::Literal(l) => out.push_str(&format!("{: <6} {}", "Lit", l.to_string())),
            TokenTree::Group(g) => {
                if start == Position::from(g.span_open().start()) {
                    let open = match g.delimiter() {
                        proc_macro2::Delimiter::Parenthesis => "(",
                        proc_macro2::Delimiter::Brace => "{",
                        proc_macro2::Delimiter::Bracket => "[",
                        proc_macro2::Delimiter::None => "",
                    };
                    out.push_str(&format!("{: <6} {}", "Group", open));
                } else {
                    let close = match g.delimiter() {
                        proc_macro2::Delimiter::Parenthesis => ")",
                        proc_macro2::Delimiter::Brace => "}",
                        proc_macro2::Delimiter::Bracket => "]",
                        proc_macro2::Delimiter::None => "",
                    };
                    out.push_str(&format!("{: <6} {}", "Group", close));
                }
            }
        }
        out
    }
}

/// Newtype to allow for custom implementations
pub(crate) struct TokenMatrix(Vec<Tokens>);

impl TokenMatrix {
    pub(crate) fn new() -> Self {
        TokenMatrix(Vec::new())
    }

    /// Get the given token at the given index.  This allows for negative indexing
    pub(crate) fn get(&self, i: isize) -> Option<&Tokens> {
        if i < 0 {
            self.0.get(self.0.len().checked_sub(i.abs() as usize)?)
        } else {
            self.0.get(i as usize)
        }
    }

    /// Is the token stream empty
    pub(crate) fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    /// Get the last token in the stream
    pub(crate) fn last(&self) -> Option<&Tokens> {
        self.0.last()
    }

    /// Push a new token onto the end of the token stream
    pub(crate) fn push(&mut self, tokens: Tokens) {
        self.0.push(tokens);
    }

    /// Convert the token matrix into a TokenStream
    pub(crate) fn into_token_stream(self) -> TokenStream {
        self.0.into_iter().map(|x| x.0).flatten().collect()
    }
}

/// Newtype to allow for custom implementations
pub(crate) struct Tokens(Vec<TokenTree>);

impl Tokens {
    pub(crate) fn new() -> Self {
        Tokens(Vec::new())
    }

    /// Get the given token at the given index.  This allows for negative indexing
    pub(crate) fn get(&self, i: isize) -> Option<&TokenTree> {
        if i < 0 {
            self.0.get(self.0.len().checked_sub(i.abs() as usize)?)
        } else {
            self.0.get(i as usize)
        }
    }

    /// Is the token stream empty
    pub(crate) fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    /// Insert the given token
    pub(crate) fn insert(&mut self, token: TokenTree) {
        self.0.insert(0, token);
    }

    /// Get the last token in the stream
    pub(crate) fn last(&self) -> Option<&TokenTree> {
        self.0.last()
    }

    /// Push a new token onto the end of the token stream
    pub(crate) fn push(&mut self, token: TokenTree) {
        self.0.push(token);
    }

    /// Decrement the given index while the most recent tokens are a comment as we are traversing
    /// backwards through the token stream.
    ///
    /// * ***i*** - index tracking our location in the token stream
    fn dec_while_is_comment(&self, i: &mut isize) {
        loop {
            if !self.is_comment(i) {
                break;
            }
        }
    }

    /// Determine if the most recent tokens are a comment and if so decrement the given index to
    /// consume the comment as we are traversing backwards through the token stream.
    ///
    /// * ***i*** - index tracking our location in the token stream
    /// * ***return*** - true if the most recent tokens are a comment
    fn is_comment(&self, i: &mut isize) -> bool {
        // Comments are composed of a minimum of 2 tokens
        if self.0.len() >= 2 {
            // Check for group with comment ident
            if self.get(*i).is_comment_group() {
                if self.get(*i - 1).is_punct('!') || self.get(*i - 1).is_punct('#') {
                    if self.get(*i - 1).is_punct('#') {
                        *i -= 2; // skip group and punct
                    } else {
                        *i -= 3; // skip group and (2) puncts
                    }
                    return true;
                }
            }
        }
        false
    }

    /// Determine if the most recent tokens are a statement
    pub(crate) fn is_statement(&self) -> bool {
        if self.0.len() > 0 {
            // Skip comments
            let mut i = -1;
            self.dec_while_is_comment(&mut i);

            // Check following token
            if let Some(TokenTree::Punct(p)) = self.get(i) {
                if p.as_char() == ';' {
                    return true;
                }
            }
        }
        false
    }

    /// Determine if the most recent tokens are a field
    pub(crate) fn is_field(&self) -> bool {
        // Fields are composed of a minimum of 4 tokens: "a: i32,"
        if self.0.len() >= 4 {
            // Skip comments
            let mut i = -1;
            self.dec_while_is_comment(&mut i);

            // Check the following token for comma
            if let Some(TokenTree::Punct(p)) = self.get(i) {
                if p.as_char() == ',' {
                    // Consume until we find the colon or end
                    i -= 1;
                    while let Some(token) = self.get(i) {
                        if let TokenTree::Punct(p) = token {
                            // An addtional comma means we have a different kind
                            if p.as_char() == ',' {
                                return false;
                            } else if p.as_char() == ':' {
                                return true;
                            }
                        }
                        i -= 1;
                    }
                }
            }
        }
        false
    }

    /// Determine if the most recent tokens are a variant
    pub(crate) fn is_variant(&self) -> bool {
        // Variants are composed of a minimum of 2 tokens: "A,"
        if self.0.len() >= 2 {
            // Skip comments
            let mut i = -1;
            self.dec_while_is_comment(&mut i);

            // Check for the following token for comma
            if let Some(TokenTree::Punct(p)) = self.get(i) {
                if p.as_char() == ',' {
                    // Consume until we find a comma or the end
                    i -= 1;
                    while let Some(token) = self.get(i) {
                        if let TokenTree::Punct(p) = token {
                            if p.as_char() == ',' {
                                return true;
                            } else if p.as_char() == ';' || p.as_char() == ':' {
                                return false;
                            }
                        }
                        i -= 1;

                        // Hit the end without getting an invalid token
                        if self.get(i).is_none() {
                            return true;
                        }
                    }
                }
            }
        }
        false
    }

    /// Inject a dummy varient tokens to ensure that the token stream is valid for syn
    ///
    /// * ***tokens***: Token stream to inject the dummy into
    pub(crate) fn inject_dummy_variant(&mut self) {
        let token = TokenTree::from(Ident::new(crate::DUMMY_VARIANT, Span::call_site()));
        trace!("{}", token.to_str(&token.span()));
        self.push(token);

        let token = TokenTree::from(Punct::new(',', Spacing::Alone));
        trace!("{}", token.to_str(&token.span()));
        self.push(token);
    }

    /// Inject dummy field tokens to ensure that the token stream is valid for syn. Trailing comments
    /// require a dummy field after as they are injected as outer comments and syn checks that there is
    /// an associated field.
    ///
    /// * ***tokens***: Token stream to inject the dummy into
    pub(crate) fn inject_dummy_field(&mut self) {
        let token = TokenTree::from(Ident::new(crate::DUMMY_FIELD, Span::call_site()));
        trace!("{}", token.to_str(&token.span()));
        self.push(token);

        let token = TokenTree::from(Punct::new(':', Spacing::Alone));
        trace!("{}", token.to_str(&token.span()));
        self.push(token);

        let token = TokenTree::from(Ident::new("i32", Span::call_site()));
        trace!("{}", token.to_str(&token.span()));
        self.push(token);

        let token = TokenTree::from(Punct::new(',', Spacing::Alone));
        trace!("{}", token.to_str(&token.span()));
        self.push(token);
    }

    /// Inject a dummy struct tokens to ensure that the token stream is valid for syn
    ///
    /// * ***tokens***: Token stream to inject the dummy into
    pub(crate) fn inject_dummy_struct(&mut self) {
        let token = TokenTree::from(Ident::new("struct", Span::call_site()));
        trace!("{}", token.to_str(&token.span()));
        self.push(token);

        let token = TokenTree::from(Ident::new(crate::DUMMY_STRUCT, Span::call_site()));
        trace!("{}", token.to_str(&token.span()));
        self.push(token);

        let token = TokenTree::from(Punct::new(';', Spacing::Alone));
        trace!("{}", token.to_str(&token.span()));
        self.push(token);
    }

    /// Append the comment to the token stream.
    ///
    /// * ***comment***: Comment to inject
    /// * ***inner***: Inject the given comment as an inner comment
    pub(crate) fn append_comment(&mut self, comment: &Comment, inner: bool) {
        for token in self.create_comment(comment, inner) {
            self.push(token);
        }
    }

    /// Prepend the comment to the token stream.
    ///
    /// * ***comment***: Comment to inject
    /// * ***inner***: Inject the given comment as an inner comment
    pub(crate) fn prepend_comment(&mut self, comment: &Comment, inner: bool) {
        for token in self.create_comment(comment, inner).into_iter().rev() {
            self.insert(token);
        }
    }

    /// Translate the comment into doc tokens which follows proc_macro2 precedence of storing doc
    /// comments as attributes. We are just leveraging this pattern to trick syn into passing through
    /// regular comments as inner doc comments which can be allowed anywhere. It abuses the system
    /// slightly but we'll strip them back out later during scaning.
    ///
    /// * ***comment***: Comment to inject
    /// * ***inner***: Inject the given comment as an inner comment
    fn create_comment(&self, comment: &Comment, inner: bool) -> Vec<TokenTree> {
        let mut tokens: Vec<TokenTree> = vec![];

        // Spans are an optional feature in proc_macro2 that luckily syn doesn't take into account. This
        // means being unable to set them due to to being private doesn't matter.
        let token: TokenTree = Punct::new('#', Spacing::Alone).into();
        trace!("{}", token.to_str(&token.span()));
        tokens.push(token);

        if inner {
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

        tokens
    }

    /// Convert the token stream into a string for debugging purposes
    pub(crate) fn to_str(&self) -> String {
        fn recurse<'a>(iter: impl Iterator<Item = &'a TokenTree>) -> String {
            let mut str = String::new();
            for token in iter {
                match token {
                    TokenTree::Group(g) => {
                        str.push_str(&format!("{}\n", token.to_str(&g.span_open())));
                        str.push_str(&recurse(g.stream().into_iter().collect::<Vec<_>>().iter()));
                        str.push_str(&format!("{}\n", token.to_str(&g.span_close())));
                    }
                    _ => str.push_str(&format!("{}\n", token.to_str(&token.span()))),
                }
            }
            str
        }
        recurse(&mut self.0.iter())
    }

    /// Convert the token stream into a TokenStream
    pub(crate) fn into_token_stream(self) -> TokenStream {
        self.0.into_iter().collect()
    }
}

/// Implements index operator for Tokens
impl std::ops::Index<usize> for Tokens {
    type Output = TokenTree;
    fn index(&self, index: usize) -> &Self::Output {
        &self.0[index]
    }
}

/// Implements from Vec<TokenTree> for Tokens
impl From<Vec<TokenTree>> for Tokens {
    fn from(tokens: Vec<TokenTree>) -> Self {
        Tokens(tokens)
    }
}

/// Implements into Vec<TokenTree> for Tokens
impl Into<Vec<TokenTree>> for Tokens {
    fn into(self) -> Vec<TokenTree> {
        self.0
    }
}

/// Newtype to allow for custom TokenStream manipulation
pub(crate) struct TokenStreamExt(TokenStream);

/// Provide multiple peeking capabilities to an iterator
#[derive(Clone, Debug)]
pub struct MultiPeek<I>
where
    I: Iterator<Item = TokenTree>,
{
    src: Fuse<I>,           // source iterator
    buf: VecDeque<I::Item>, // buffer of items
}
impl<I> MultiPeek<I>
where
    I: Iterator<Item = TokenTree>,
{
    /// Provide multiple peeking capabilities to an iterator with `peek()`.
    ///
    /// * ***iterable*** - The iterator to peek into
    pub fn new(iterable: I) -> Self {
        MultiPeek {
            src: iterable.into_iter().fuse(),
            buf: VecDeque::new(),
        }
    }

    /// Get the next three tokens in the iterator
    fn next(&mut self) -> (Option<&I::Item>, Option<&I::Item>, Option<&I::Item>) {
        self.buf.clear();

        for _ in 0..3 {
            if let Some(x) = self.src.next() {
                self.buf.push_back(x);
            }
        }

        // Return the next three tokens
        (self.buf.get(0), self.buf.get(1), self.buf.get(2))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;
    use proc_macro2::{TokenStream, TokenTree};
    use std::str::FromStr;

    trait TokenTestsExt {
        fn tokens(&self) -> Tokens;
    }
    impl TokenTestsExt for TokenTree {
        fn tokens(&self) -> Tokens {
            if let TokenTree::Group(group) = self {
                group.stream().into_iter().collect::<Vec<_>>().into()
            } else {
                panic!("Expected a group token, found: {:?}", self);
            }
        }
    }

    /// Convert the input string into a token stream and drop the last n tokens
    fn to_tokens(input: &str) -> Tokens {
        TokenStream::from_str(input)
            .unwrap()
            .into_iter()
            .collect::<Vec<TokenTree>>()
            .into()
    }

    #[test]
    fn test_tokens_next() {
        // None
        let stream = to_tokens(indoc! {r#" "#});
        let mut tokens = MultiPeek::new(stream.0.into_iter());
        let (next0, next1, next2) = tokens.next();

        assert_eq!(next0.is_none(), true);
        assert_eq!(next1.is_none(), true);
        assert_eq!(next2.is_none(), true);

        // Partial tokens
        let stream = to_tokens(indoc! {r#"
            enum Foo
        "#});
        let mut tokens = MultiPeek::new(stream.0.into_iter());
        let (next0, next1, next2) = tokens.next();

        assert_eq!(next0.unwrap().to_string(), "enum".to_string());
        assert_eq!(next1.unwrap().to_string(), "Foo".to_string());
        assert_eq!(next2.is_none(), true);

        // Full three tokens
        let stream = to_tokens(indoc! {r#"
            enum Foo {
                A(String),
                B((&'a i32, i32)),
            }
        "#});
        let mut tokens = MultiPeek::new(stream.0.into_iter());
        let (next0, next1, next2) = tokens.next();

        assert_eq!(next0.unwrap().to_string(), "enum".to_string());
        assert_eq!(next1.unwrap().to_string(), "Foo".to_string());
        assert_eq!(next2.unwrap().to_string().starts_with("{"), true);
    }

    #[test]
    fn test_is_variant_complicated() {
        let stream = to_tokens(indoc! {r#"
            enum Foo {
                A(String),
                B((&'a i32, i32)),
            }
        "#});
        // println!("{}", stream.to_str());
        let group = stream[2].tokens();
        assert_eq!(group.is_variant(), true);

        let stream = to_tokens(indoc! {r#"
            enum Foo {
                B((&'a i32, i32)),
            }
        "#});
        let group = stream[2].tokens();
        assert_eq!(group.is_variant(), true);

        let stream = to_tokens(indoc! {r#"
            enum Foo {
                A(String),
            }
        "#});
        let group = stream[2].tokens();
        assert_eq!(group.is_variant(), true);
    }

    #[test]
    fn test_is_variant() {
        let stream = to_tokens(indoc! {r#"
            enum Foo {
                A,
            }
        "#});
        let group = stream[2].tokens();
        assert_eq!(group.is_variant(), true);

        // Multiples
        let stream = to_tokens(indoc! {r#"
            enum Foo {
                A,
                B,
            }
        "#});
        let mut group = stream[2].tokens();
        group.append_comment(&Comment::line_trailing("trailing"), false);
        assert_eq!(group.is_variant(), true);

        // Fail
        let stream = to_tokens(indoc! {r#"
            enum Foo {
                A,
                B: i32,
            }
        "#});
        let group = stream[2].tokens();
        assert_eq!(group.is_variant(), false);
    }

    #[test]
    fn test_is_field_multiple() {
        let stream = to_tokens(indoc! {r#"
            struct Foo {
                bar: i32,
                blah: i32,
            }
        "#});
        let group = stream[2].tokens();
        assert_eq!(group.is_field(), true);
    }

    #[test]
    fn test_is_field_generics() {
        let stream = to_tokens(indoc! {r#"
            struct Foo<T> {
                bar: Box<Option<('a &i32, T)>>,
            }
        "#});
        let group = stream[5].tokens();
        assert_eq!(group.is_field(), true);
    }

    #[test]
    fn test_is_field_tuple_type() {
        // the tuple () is considered a group and will get skipped in checking
        let stream = to_tokens(indoc! {r#"
            struct Foo {
                bar: Box<Option<(i32, i32)>>,
            }
        "#});
        let group = stream[2].tokens();
        assert_eq!(group.is_field(), true);
    }

    #[test]
    fn test_is_field() {
        // Success
        let stream = to_tokens(indoc! {r#"
            struct Foo {
                bar: i32,
            }
        "#});
        let group = stream[2].tokens();
        assert_eq!(group.is_field(), true);

        // More Complex
        let stream = to_tokens(indoc! {r#"
            struct Foo {
                bar: Box<Option<i32>>,
            }
        "#});
        let group = stream[2].tokens();
        assert_eq!(group.is_field(), true);

        // With comment first
        let stream = to_tokens(indoc! {r#"
            struct Foo {
                bar: Box<Option<i32>>,
            }
        "#});
        let mut group = stream[2].tokens();
        group.append_comment(&Comment::line_trailing("trailing"), false);
        assert_eq!(group.is_field(), true);

        // Fail
        let stream = to_tokens(indoc! {r#"
            struct Foo {
                bar i32,
            }
        "#});
        let group = stream[2].tokens();
        assert_eq!(group.is_field(), false);
    }

    #[test]
    fn test_is_statement() {
        // Success
        let stream = to_tokens(indoc! {r#"
            struct Foo;
        "#});
        assert_eq!(stream.is_statement(), true);

        // Comment first
        let mut stream = to_tokens(indoc! {r#"
            struct Foo;
        "#});
        stream.append_comment(&Comment::line_trailing("trailing"), false);
        assert_eq!(stream.is_statement(), true);

        // Fail
        let stream = to_tokens(indoc! {r#"
            struct Foo
        "#});
        assert_eq!(stream.is_statement(), false);
    }

    #[test]
    fn test_get() {
        let stream = to_tokens(indoc! {r#"
            struct Foo;
        "#});

        // Positive
        assert_eq!(stream.get(0).map(|x| x.to_string()), Some("struct".into()));
        assert_eq!(stream.get(1).map(|x| x.to_string()), Some("Foo".into()));
        assert_eq!(stream.get(2).map(|x| x.to_string()), Some(";".into()));
        assert!(stream.get(3).is_none());

        // Positive
        assert!(stream.get(-4).is_none());
        assert_eq!(stream.get(-3).map(|x| x.to_string()), Some("struct".into()));
        assert_eq!(stream.get(-2).map(|x| x.to_string()), Some("Foo".into()));
        assert_eq!(stream.get(-1).map(|x| x.to_string()), Some(";".into()));
    }
}
