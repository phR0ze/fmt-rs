use super::{Comment, Position, Source};
use proc_macro2::{Delimiter, Group, Ident, Literal, Punct, Spacing, Span, TokenStream, TokenTree};
use std::{collections::VecDeque, iter::Fuse, ops::Mul};
use syn::token::{self, Token};
use tracing::trace;

/// Extension trait for proc_macro2::TokenTree
pub(crate) trait TokenExt {
    fn span_open(&self) -> (Position, Position);
    fn span_close(&self) -> (Position, Position);
}

pub(crate) trait OptionTokenExt {
    fn span_open(&self) -> (Option<Position>, Option<Position>);
    fn span_close(&self) -> (Option<Position>, Option<Position>);
}

impl TokenExt for TokenBox {
    fn span_open(&self) -> (Position, Position) {
        let x = &**self;
        match x {
            TokenTree::Group(group) => {
                let span = group.span_open();
                (span.start().into(), span.end().into())
            }
            _ => {
                let span = self.span();
                (span.start().into(), span.end().into())
            }
        }
    }
    fn span_close(&self) -> (Position, Position) {
        let x = &**self;
        match x {
            TokenTree::Group(group) => {
                let span = group.span_close();
                (span.start().into(), span.end().into())
            }
            _ => {
                let span = self.span();
                (span.start().into(), span.end().into())
            }
        }
    }
}

impl TokenExt for &TokenBox {
    fn span_open(&self) -> (Position, Position) {
        (**self).span_open()
    }
    fn span_close(&self) -> (Position, Position) {
        (**self).span_close()
    }
}

impl OptionTokenExt for Option<TokenBox> {
    fn span_open(&self) -> (Option<Position>, Option<Position>) {
        self.as_ref().span_open()
    }

    fn span_close(&self) -> (Option<Position>, Option<Position>) {
        self.as_ref().span_close()
    }
}

impl OptionTokenExt for Option<&TokenBox> {
    fn span_open(&self) -> (Option<Position>, Option<Position>) {
        match *self {
            Some(x) => match x.span_open() {
                (start, end) => (Some(start), Some(end)),
            },
            None => (None, None),
        }
    }

    fn span_close(&self) -> (Option<Position>, Option<Position>) {
        match *self {
            Some(x) => match x.span_close() {
                (start, end) => (Some(start), Some(end)),
            },
            None => (None, None),
        }
    }
}

// impl OptionTokenExt for Option<TokenTree> {
//     fn is_comment_ident(&self) -> bool {
//         self.as_ref().is_comment_ident()
//     }

//     fn is_comment_group(&self) -> bool {
//         self.as_ref().is_comment_group()
//     }

//     fn is_punct(&self, char: char) -> bool {
//         self.as_ref().is_punct(char)
//     }

//     fn span_open(&self) -> (Option<Position>, Option<Position>) {
//         self.as_ref().span_open()
//     }

//     fn span_close(&self) -> (Option<Position>, Option<Position>) {
//         self.as_ref().span_close()
//     }
// }

// impl TokenExt for TokenTree {
//     /// Determine if the token is a comment identifier
//     fn is_comment_ident(&self) -> bool {
//         if let TokenTree::Ident(i) = self {
//             return i.to_string().starts_with("comment_");
//         }
//         false
//     }

//     /// Determine if the token is a comment group
//     fn is_comment_group(&self) -> bool {
//         if let TokenTree::Group(g) = self {
//             let stream = g.stream().into_iter().collect::<Vec<_>>();
//             if stream.len() == 3 && stream[0].is_comment_ident() {
//                 return true;
//             }
//         }
//         false
//     }

//     /// Determine if the token is the given punctuation character
//     fn is_punct(&self, char: char) -> bool {
//         match self {
//             TokenTree::Punct(p) => p.as_char() == char,
//             _ => false,
//         }
//     }

//     /// Get the open span
//     fn span_open(&self) -> (Position, Position) {
//         match self {
//             TokenTree::Ident(ident) => {
//                 let span = ident.span();
//                 (span.start().into(), span.end().into())
//             }
//             TokenTree::Literal(literal) => {
//                 let span = literal.span();
//                 (span.start().into(), span.end().into())
//             }
//             TokenTree::Group(group) => {
//                 let span = group.span_open();
//                 (span.start().into(), span.end().into())
//             }
//             TokenTree::Punct(punct) => {
//                 let span = punct.span();
//                 (span.start().into(), span.end().into())
//             }
//         }
//     }

//     /// Get the close span
//     fn span_close(&self) -> (Position, Position) {
//         match self {
//             TokenTree::Ident(ident) => {
//                 let span = ident.span();
//                 (span.start().into(), span.end().into())
//             }
//             TokenTree::Literal(literal) => {
//                 let span = literal.span();
//                 (span.start().into(), span.end().into())
//             }
//             TokenTree::Group(group) => {
//                 let span = group.span_close();
//                 (span.start().into(), span.end().into())
//             }
//             TokenTree::Punct(punct) => {
//                 let span = punct.span();
//                 (span.start().into(), span.end().into())
//             }
//         }
//     }
// }

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

    // /// Decrement the given index while the most recent tokens are a comment as we are traversing
    // /// backwards through the token stream.
    // ///
    // /// * ***i*** - index tracking our location in the token stream
    // fn dec_while_is_comment(&self, i: &mut isize) {
    //     loop {
    //         if !self.is_comment(i) {
    //             break;
    //         }
    //     }
    // }

    // /// Determine if the most recent tokens are a comment and if so decrement the given index to
    // /// consume the comment as we are traversing backwards through the token stream.
    // ///
    // /// * ***i*** - index tracking our location in the token stream
    // /// * ***return*** - true if the most recent tokens are a comment
    // fn is_comment(&self, i: &mut isize) -> bool {
    //     // Comments are composed of a minimum of 2 tokens
    //     if self.0.len() >= 2 {
    //         // Check for group with comment ident
    //         if self.get(*i).is_comment_group() {
    //             if self.get(*i - 1).is_punct('!') || self.get(*i - 1).is_punct('#') {
    //                 if self.get(*i - 1).is_punct('#') {
    //                     *i -= 2; // skip group and punct
    //                 } else {
    //                     *i -= 3; // skip group and (2) puncts
    //                 }
    //                 return true;
    //             }
    //         }
    //     }
    //     false
    // }

    // /// Determine if the most recent tokens are a statement
    // pub(crate) fn is_statement(&self) -> bool {
    //     if self.0.len() > 0 {
    //         // Skip comments
    //         let mut i = -1;
    //         self.dec_while_is_comment(&mut i);

    //         // Check following token
    //         if let Some(TokenTree::Punct(p)) = self.get(i) {
    //             if p.as_char() == ';' {
    //                 return true;
    //             }
    //         }
    //     }
    //     false
    // }

    // /// Determine if the most recent tokens are a field
    // pub(crate) fn is_field(&self) -> bool {
    //     // Fields are composed of a minimum of 4 tokens: "a: i32,"
    //     if self.0.len() >= 4 {
    //         // Skip comments
    //         let mut i = -1;
    //         self.dec_while_is_comment(&mut i);

    //         // Check the following token for comma
    //         if let Some(TokenTree::Punct(p)) = self.get(i) {
    //             if p.as_char() == ',' {
    //                 // Consume until we find the colon or end
    //                 i -= 1;
    //                 while let Some(token) = self.get(i) {
    //                     if let TokenTree::Punct(p) = token {
    //                         // An addtional comma means we have a different kind
    //                         if p.as_char() == ',' {
    //                             return false;
    //                         } else if p.as_char() == ':' {
    //                             return true;
    //                         }
    //                     }
    //                     i -= 1;
    //                 }
    //             }
    //         }
    //     }
    //     false
    // }

    // /// Determine if the most recent tokens are a variant
    // pub(crate) fn is_variant(&self) -> bool {
    //     // Variants are composed of a minimum of 2 tokens: "A,"
    //     if self.0.len() >= 2 {
    //         // Skip comments
    //         let mut i = -1;
    //         self.dec_while_is_comment(&mut i);

    //         // Check for the following token for comma
    //         if let Some(TokenTree::Punct(p)) = self.get(i) {
    //             if p.as_char() == ',' {
    //                 // Consume until we find a comma or the end
    //                 i -= 1;
    //                 while let Some(token) = self.get(i) {
    //                     if let TokenTree::Punct(p) = token {
    //                         if p.as_char() == ',' {
    //                             return true;
    //                         } else if p.as_char() == ';' || p.as_char() == ':' {
    //                             return false;
    //                         }
    //                     }
    //                     i -= 1;

    //                     // Hit the end without getting an invalid token
    //                     if self.get(i).is_none() {
    //                         return true;
    //                     }
    //                 }
    //             }
    //         }
    //     }
    //     false
    // }

    /// Inject a dummy varient tokens to ensure that the token stream is valid for syn
    ///
    /// * ***tokens***: Token stream to inject the dummy into
    pub(crate) fn inject_dummy_variant(&mut self) {
        let token = TokenTree::from(Ident::new(crate::DUMMY_VARIANT, Span::call_site()));
        trace!("{}", TokenBox::Token(token.clone()).to_str());
        self.push(token);

        let token = TokenTree::from(Punct::new(',', Spacing::Alone));
        trace!("{}", TokenBox::Token(token.clone()).to_str());
        self.push(token);
    }

    /// Inject dummy field tokens to ensure that the token stream is valid for syn. Trailing comments
    /// require a dummy field after as they are injected as outer comments and syn checks that there is
    /// an associated field.
    ///
    /// * ***tokens***: Token stream to inject the dummy into
    pub(crate) fn inject_dummy_field(&mut self) {
        let token = TokenTree::from(Ident::new(crate::DUMMY_FIELD, Span::call_site()));
        trace!("{}", TokenBox::Token(token.clone()).to_str());
        self.push(token);

        let token = TokenTree::from(Punct::new(':', Spacing::Alone));
        trace!("{}", TokenBox::Token(token.clone()).to_str());
        self.push(token);

        let token = TokenTree::from(Ident::new("i32", Span::call_site()));
        trace!("{}", TokenBox::Token(token.clone()).to_str());
        self.push(token);

        let token = TokenTree::from(Punct::new(',', Spacing::Alone));
        trace!("{}", TokenBox::Token(token.clone()).to_str());
        self.push(token);
    }

    /// Inject a dummy struct tokens to ensure that the token stream is valid for syn
    ///
    /// * ***tokens***: Token stream to inject the dummy into
    pub(crate) fn inject_dummy_struct(&mut self) {
        let token = TokenTree::from(Ident::new("struct", Span::call_site()));
        trace!("{}", TokenBox::Token(token.clone()).to_str());
        self.push(token);

        let token = TokenTree::from(Ident::new(crate::DUMMY_STRUCT, Span::call_site()));
        trace!("{}", TokenBox::Token(token.clone()).to_str());
        self.push(token);

        let token = TokenTree::from(Punct::new(';', Spacing::Alone));
        trace!("{}", TokenBox::Token(token.clone()).to_str());
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

    /// Append comments to the token stream.
    ///
    /// * ***comments***: Comments to inject
    /// * ***inner***: Inject the given comment as an inner comment
    pub(crate) fn append<'a, I>(&mut self, comments: I, inner: bool)
    where
        I: Iterator<Item = &'a Comment>,
    {
        for comment in comments {
            for token in self.create_comment(comment, inner) {
                self.push(token);
            }
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
        trace!("{}", TokenBox::Token(token.clone()).to_str());
        tokens.push(token);

        if inner {
            let token: TokenTree = Punct::new('!', Spacing::Alone).into();
            trace!("{}", TokenBox::Token(token.clone()).to_str());
            tokens.push(token);
        }

        // Create and log new comment group
        let mut stream = vec![];

        trace!("{: <12}{: <6} {}", "0:0..0:0", "Group", "[");

        let token: TokenTree = Ident::new(&comment.attr_name(), Span::call_site()).into();
        trace!("{}", TokenBox::Token(token.clone()).to_str());
        stream.push(token);

        let token: TokenTree = Punct::new('=', Spacing::Alone).into();
        trace!("{}", TokenBox::Token(token.clone()).to_str());
        stream.push(token);

        let token: TokenTree = Literal::string(&comment.text()).into();
        trace!("{}", TokenBox::Token(token.clone()).to_str());
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
                        str.push_str(&format!(
                            "{}\n",
                            TokenBox::GroupStart(token.clone()).to_str()
                        ));
                        str.push_str(&recurse(g.stream().into_iter().collect::<Vec<_>>().iter()));
                        str.push_str(&format!("{}\n", TokenBox::GroupEnd(token.clone()).to_str()));
                    }
                    _ => str.push_str(&format!("{}\n", TokenBox::Token(token.clone()).to_str())),
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

/// Helper type for storing TokenTree variants
#[derive(Debug, Clone)]
pub(crate) enum TokenBox {
    Token(TokenTree),
    GroupStart(TokenTree),
    GroupEnd(TokenTree),
}
impl TokenBox {
    pub(crate) fn is_group_start(&self) -> bool {
        match self {
            TokenBox::GroupStart(_) => true,
            _ => false,
        }
    }

    /// Convert the token into a string for debugging purposes
    pub(crate) fn to_str(&self) -> String {
        let (start, end) = self.span_open();
        let mut out = String::from(format!("{: <12}", format!("{}..{}", start, end)));

        if let TokenBox::Token(TokenTree::Ident(i)) = self {
            out.push_str(&format!("{: <6} {}", "Ident", i.to_string()))
        } else if let TokenBox::Token(TokenTree::Punct(p)) = self {
            out.push_str(&format!("{: <6} {}", "Punct", p.to_string()))
        } else if let TokenBox::Token(TokenTree::Literal(l)) = self {
            out.push_str(&format!("{: <6} {}", "Lit", l.to_string()))
        } else if let TokenBox::GroupStart(TokenTree::Group(g)) = self {
            let open = match g.delimiter() {
                proc_macro2::Delimiter::Parenthesis => "(",
                proc_macro2::Delimiter::Brace => "{",
                proc_macro2::Delimiter::Bracket => "[",
                proc_macro2::Delimiter::None => "",
            };
            out.push_str(&format!("{: <6} {}", "Group", open));
        } else if let TokenBox::GroupEnd(TokenTree::Group(g)) = self {
            let close = match g.delimiter() {
                proc_macro2::Delimiter::Parenthesis => ")",
                proc_macro2::Delimiter::Brace => "}",
                proc_macro2::Delimiter::Bracket => "]",
                proc_macro2::Delimiter::None => "",
            };
            out.push_str(&format!("{: <6} {}", "Group", close));
        }
        out
    }
}

/// Implement deref for TokenBox
impl std::ops::Deref for TokenBox {
    type Target = TokenTree;
    fn deref(&self) -> &Self::Target {
        match self {
            TokenBox::Token(token) => token,
            TokenBox::GroupStart(token) => token,
            TokenBox::GroupEnd(token) => token,
        }
    }
}

/// Helper type for storing Group expansion
#[derive(Debug)]
enum TokenItem {
    Token(TokenTree),
    Group(TokenTree, VecDeque<TokenTree>, bool),
}
impl TokenItem {
    /// Wrap the given token
    pub(crate) fn from(token: TokenTree) -> Self {
        if let TokenTree::Group(group) = &token {
            let mut tokens = VecDeque::new();
            for token in group.stream().into_iter() {
                tokens.push_back(token);
            }
            TokenItem::Group(token, tokens, false)
        } else {
            TokenItem::Token(token)
        }
    }
}

/// Provide multiple peeking capabilities to an iterator
#[derive(Debug)]
pub struct TokenWrapper<I>
where
    I: Iterator<Item = TokenTree>,
{
    src: Fuse<I>,               // source iterator
    buf: Vec<TokenItem>,        // buffer of group expansions
    slider: VecDeque<TokenBox>, // track the last two tokens
}
impl<I> TokenWrapper<I>
where
    I: Iterator<Item = TokenTree>,
{
    /// Provide multiple peeking capabilities to an iterator with `peek()`.
    ///
    /// * ***iterable*** - The token stream to iterate over
    pub(crate) fn from(iterable: I) -> Self {
        TokenWrapper {
            src: iterable.into_iter().fuse(),
            buf: Vec::new(),
            slider: VecDeque::new(),
        }
    }

    /// Queue the next two token wrappers from the iterator
    pub(crate) fn next(&mut self) -> &Self {
        self.slider.pop_front(); // shift first off

        while self.slider.len() < 2 {
            // Fill the response from the buffer if available
            if let Some(item) = self.buf.pop() {
                match item {
                    TokenItem::Token(token) => {
                        self.slider.push_back(TokenBox::Token(token));
                    }
                    TokenItem::Group(group, mut tokens, visited) => {
                        if !visited {
                            self.slider.push_back(TokenBox::GroupStart(group.clone()));
                            self.buf.push(TokenItem::Group(group, tokens, true));
                            continue;
                        }

                        // Process group tokens
                        if let Some(token) = tokens.pop_front() {
                            self.buf.push(TokenItem::Group(group, tokens, true));
                            self.buf.push(TokenItem::from(token))
                        } else {
                            self.slider.push_back(TokenBox::GroupEnd(group));
                        }
                    }
                }
            } else {
                // No more tokens in the buffer so pull from iter
                if let Some(token) = self.src.next() {
                    self.buf.push(TokenItem::from(token));
                } else {
                    // No more tokens
                    break;
                }
            }
        }
        self
    }

    /// Return the current three tokens
    pub(crate) fn peek(&self) -> [Option<&TokenBox>; 2] {
        [self.slider.get(0), self.slider.get(1)]
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        model::{pos, Position},
        token::Token,
    };
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
    fn test_span_spans() {
        // None
        let stream = to_tokens(indoc! {r#" "#});
        let mut tokens = TokenWrapper::from(stream.0.into_iter());
        let [token0, token1] = tokens.next().peek();

        assert_eq!(token0.is_none(), true);
        assert_eq!(token1.is_none(), true);

        // Partial tokens
        let stream = to_tokens(indoc! {r#"
            enum
        "#});
        let mut tokens = TokenWrapper::from(stream.0.into_iter());
        let [token0, token1] = tokens.next().peek();

        matches!(token0.as_ref().unwrap(), TokenBox::Token(_));
        assert_eq!(token0.unwrap().span_open(), (pos(0, 0), pos(0, 4)));
        assert_eq!(token1.is_none(), true);

        // Full three tokens
        let stream = to_tokens(indoc! {r#"
            println!("{}", "1");
        "#});
        let mut tokens = TokenWrapper::from(stream.0.into_iter());

        // println!(
        let [token0, token1] = tokens.next().peek();
        matches!(token0.as_ref().unwrap(), TokenBox::Token(_));
        assert_eq!(token0.unwrap().span_open(), (pos(0, 0), pos(0, 7)));
        matches!(token1.as_ref().unwrap(), TokenBox::Token(_));
        assert_eq!(token1.unwrap().span_open(), (pos(0, 7), pos(0, 8)));

        // !("{}"
        let [token0, token1] = tokens.next().peek();
        matches!(token0.as_ref().unwrap(), TokenBox::Token(_));
        assert_eq!(token0.unwrap().span_open(), (pos(0, 7), pos(0, 8)));
        matches!(token1.as_ref().unwrap(), TokenBox::GroupStart(_));
        assert_eq!(token1.unwrap().span_open(), (pos(0, 8), pos(0, 9)));

        // ("{}",
        let [token0, token1] = tokens.next().peek();
        matches!(token0.as_ref().unwrap(), TokenBox::GroupStart(_));
        assert_eq!(token0.unwrap().span_open(), (pos(0, 8), pos(0, 9)));
        matches!(token1.as_ref().unwrap(), TokenBox::Token(_));
        assert_eq!(token1.unwrap().span_open(), (pos(0, 9), pos(0, 13)));

        // "{}", "1"
        let [token0, token1] = tokens.next().peek();
        matches!(token0.as_ref().unwrap(), TokenBox::Token(_));
        assert_eq!(token0.unwrap().span_open(), (pos(0, 9), pos(0, 13)));
        matches!(token1.as_ref().unwrap(), TokenBox::Token(_));
        assert_eq!(token1.unwrap().span_open(), (pos(0, 13), pos(0, 14)));

        // , "1")
        let [token0, token1] = tokens.next().peek();
        matches!(token0.as_ref().unwrap(), TokenBox::Token(_));
        assert_eq!(token0.unwrap().span_open(), (pos(0, 13), pos(0, 14)));
        matches!(token1.as_ref().unwrap(), TokenBox::Token(_));
        assert_eq!(token1.unwrap().span_open(), (pos(0, 15), pos(0, 18)));

        // "1");
        let [token0, token1] = tokens.next().peek();
        matches!(token0.as_ref().unwrap(), TokenBox::Token(_));
        assert_eq!(token0.unwrap().span_open(), (pos(0, 15), pos(0, 18)));
        matches!(token1.as_ref().unwrap(), TokenBox::GroupEnd(_));
        assert_eq!(token1.unwrap().span_close(), (pos(0, 18), pos(0, 19)));

        // );
        let [token0, token1] = tokens.next().peek();
        matches!(token0.as_ref().unwrap(), TokenBox::GroupEnd(_));
        assert_eq!(token0.unwrap().span_close(), (pos(0, 18), pos(0, 19)));
        matches!(
            token1.as_ref().unwrap(),
            TokenBox::Token(TokenTree::Punct(_))
        );
        assert_eq!(token1.unwrap().span_close(), (pos(0, 19), pos(0, 20)));

        // ;
        let [token0, token1] = tokens.next().peek();
        matches!(
            token0.as_ref().unwrap(),
            TokenBox::Token(TokenTree::Punct(_))
        );
        assert_eq!(token0.unwrap().span_close(), (pos(0, 19), pos(0, 20)));
        matches!(token1.is_none(), true);

        //
        let [token0, token1] = tokens.next().peek();
        matches!(token0.is_none(), true);
        matches!(token1.is_none(), true);
    }

    // #[test]
    // fn test_is_variant_complicated() {
    //     let stream = to_tokens(indoc! {r#"
    //         enum Foo {
    //             A(String),
    //             B((&'a i32, i32)),
    //         }
    //     "#});
    //     // println!("{}", stream.to_str());
    //     let group = stream[2].tokens();
    //     assert_eq!(group.is_variant(), true);

    //     let stream = to_tokens(indoc! {r#"
    //         enum Foo {
    //             B((&'a i32, i32)),
    //         }
    //     "#});
    //     let group = stream[2].tokens();
    //     assert_eq!(group.is_variant(), true);

    //     let stream = to_tokens(indoc! {r#"
    //         enum Foo {
    //             A(String),
    //         }
    //     "#});
    //     let group = stream[2].tokens();
    //     assert_eq!(group.is_variant(), true);
    // }

    // #[test]
    // fn test_is_variant() {
    //     let stream = to_tokens(indoc! {r#"
    //         enum Foo {
    //             A,
    //         }
    //     "#});
    //     let group = stream[2].tokens();
    //     assert_eq!(group.is_variant(), true);

    //     // Multiples
    //     let stream = to_tokens(indoc! {r#"
    //         enum Foo {
    //             A,
    //             B,
    //         }
    //     "#});
    //     let mut group = stream[2].tokens();
    //     group.append_comment(&Comment::line_trailing("trailing"), false);
    //     assert_eq!(group.is_variant(), true);

    //     // Fail
    //     let stream = to_tokens(indoc! {r#"
    //         enum Foo {
    //             A,
    //             B: i32,
    //         }
    //     "#});
    //     let group = stream[2].tokens();
    //     assert_eq!(group.is_variant(), false);
    // }

    // #[test]
    // fn test_is_field_multiple() {
    //     let stream = to_tokens(indoc! {r#"
    //         struct Foo {
    //             bar: i32,
    //             blah: i32,
    //         }
    //     "#});
    //     let group = stream[2].tokens();
    //     assert_eq!(group.is_field(), true);
    // }

    // #[test]
    // fn test_is_field_generics() {
    //     let stream = to_tokens(indoc! {r#"
    //         struct Foo<T> {
    //             bar: Box<Option<('a &i32, T)>>,
    //         }
    //     "#});
    //     let group = stream[5].tokens();
    //     assert_eq!(group.is_field(), true);
    // }

    // #[test]
    // fn test_is_field_tuple_type() {
    //     // the tuple () is considered a group and will get skipped in checking
    //     let stream = to_tokens(indoc! {r#"
    //         struct Foo {
    //             bar: Box<Option<(i32, i32)>>,
    //         }
    //     "#});
    //     let group = stream[2].tokens();
    //     assert_eq!(group.is_field(), true);
    // }

    // #[test]
    // fn test_is_field() {
    //     // Success
    //     let stream = to_tokens(indoc! {r#"
    //         struct Foo {
    //             bar: i32,
    //         }
    //     "#});
    //     let group = stream[2].tokens();
    //     assert_eq!(group.is_field(), true);

    //     // More Complex
    //     let stream = to_tokens(indoc! {r#"
    //         struct Foo {
    //             bar: Box<Option<i32>>,
    //         }
    //     "#});
    //     let group = stream[2].tokens();
    //     assert_eq!(group.is_field(), true);

    //     // With comment first
    //     let stream = to_tokens(indoc! {r#"
    //         struct Foo {
    //             bar: Box<Option<i32>>,
    //         }
    //     "#});
    //     let mut group = stream[2].tokens();
    //     group.append_comment(&Comment::line_trailing("trailing"), false);
    //     assert_eq!(group.is_field(), true);

    //     // Fail
    //     let stream = to_tokens(indoc! {r#"
    //         struct Foo {
    //             bar i32,
    //         }
    //     "#});
    //     let group = stream[2].tokens();
    //     assert_eq!(group.is_field(), false);
    // }

    // #[test]
    // fn test_is_statement() {
    //     // Success
    //     let stream = to_tokens(indoc! {r#"
    //         struct Foo;
    //     "#});
    //     assert_eq!(stream.is_statement(), true);

    //     // Comment first
    //     let mut stream = to_tokens(indoc! {r#"
    //         struct Foo;
    //     "#});
    //     stream.append_comment(&Comment::line_trailing("trailing"), false);
    //     assert_eq!(stream.is_statement(), true);

    //     // Fail
    //     let stream = to_tokens(indoc! {r#"
    //         struct Foo
    //     "#});
    //     assert_eq!(stream.is_statement(), false);
    // }

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
