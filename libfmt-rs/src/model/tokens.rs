use super::{Comment, Config, Position, Source};
use proc_macro2::{
    token_stream, Delimiter, Group, Ident, Literal, Punct, Spacing, Span, TokenStream, TokenTree,
};
use std::{collections::VecDeque, iter::Fuse, vec};
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

impl TokenExt for TokenWrap {
    fn span_open(&self) -> (Position, Position) {
        let x = &**self;
        match x {
            TokenTree::Group(group) => {
                let span = group.span_open();
                (span.start().into(), span.end().into())
            }
            _ => {
                let span = x.span();
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
                let span = x.span();
                (span.start().into(), span.end().into())
            }
        }
    }
}

impl TokenExt for &TokenWrap {
    fn span_open(&self) -> (Position, Position) {
        (**self).span_open()
    }
    fn span_close(&self) -> (Position, Position) {
        (**self).span_close()
    }
}

impl OptionTokenExt for Option<TokenWrap> {
    fn span_open(&self) -> (Option<Position>, Option<Position>) {
        self.as_ref().span_open()
    }

    fn span_close(&self) -> (Option<Position>, Option<Position>) {
        self.as_ref().span_close()
    }
}

impl OptionTokenExt for Option<&TokenWrap> {
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

/// Token Group wrapper for managing token lines
#[derive(Debug)]
pub(crate) struct TokenGroup {
    pub(crate) token: Option<TokenTree>,  // optional group token
    pub(crate) complete: Vec<TokenTree>,  // tokens that have been processed
    pub(crate) curr_line: Vec<TokenTree>, // current token line
    pub(crate) next_line: Vec<TokenTree>, // next token line
}

impl TokenGroup {
    pub(crate) fn new(group: Option<TokenTree>) -> Self {
        Self {
            token: group,
            complete: vec![],
            curr_line: vec![],
            next_line: vec![],
        }
    }

    /// Check if the current group is empty
    pub(crate) fn is_empty(&self) -> bool {
        self.complete.is_empty() && self.curr_line.is_empty() && self.next_line.is_empty()
    }

    /// Complete all lines
    pub(crate) fn complete(&mut self) {
        self.complete.append(&mut self.curr_line);
        self.complete.append(&mut self.next_line);
    }

    /// Complete the current line
    pub(crate) fn complete_line(&mut self) {
        self.complete.append(&mut self.curr_line);
        std::mem::swap(&mut self.curr_line, &mut self.next_line);
    }

    pub(crate) fn into_token_stream(self) -> TokenStream {
        TokenStream::from_iter(self.complete.into_iter())
    }
}

//     /// Inject a dummy varient tokens to ensure that the token stream is valid for syn
//     ///
//     /// * ***tokens***: Token stream to inject the dummy into
//     pub(crate) fn inject_dummy_variant(&mut self) {
//         let token = TokenTree::from(Ident::new(crate::DUMMY_VARIANT, Span::call_site()));
//         trace!("{}", TokenWrap::Token(token.clone()).to_str());
//         self.push(token);

//         let token = TokenTree::from(Punct::new(',', Spacing::Alone));
//         trace!("{}", TokenWrap::Token(token.clone()).to_str());
//         self.push(token);
//     }

//     /// Inject dummy field tokens to ensure that the token stream is valid for syn. Trailing comments
//     /// require a dummy field after as they are injected as outer comments and syn checks that there is
//     /// an associated field.
//     ///
//     /// * ***tokens***: Token stream to inject the dummy into
//     pub(crate) fn inject_dummy_field(&mut self) {
//         let token = TokenTree::from(Ident::new(crate::DUMMY_FIELD, Span::call_site()));
//         trace!("{}", TokenWrap::Token(token.clone()).to_str());
//         self.push(token);

//         let token = TokenTree::from(Punct::new(':', Spacing::Alone));
//         trace!("{}", TokenWrap::Token(token.clone()).to_str());
//         self.push(token);

//         let token = TokenTree::from(Ident::new("i32", Span::call_site()));
//         trace!("{}", TokenWrap::Token(token.clone()).to_str());
//         self.push(token);

//         let token = TokenTree::from(Punct::new(',', Spacing::Alone));
//         trace!("{}", TokenWrap::Token(token.clone()).to_str());
//         self.push(token);
//     }

//     /// Inject a dummy struct tokens to ensure that the token stream is valid for syn
//     ///
//     /// * ***tokens***: Token stream to inject the dummy into
//     pub(crate) fn inject_dummy_struct(&mut self) {
//         let token = TokenTree::from(Ident::new("struct", Span::call_site()));
//         trace!("{}", TokenWrap::Token(token.clone()).to_str());
//         self.push(token);

//         let token = TokenTree::from(Ident::new(crate::DUMMY_STRUCT, Span::call_site()));
//         trace!("{}", TokenWrap::Token(token.clone()).to_str());
//         self.push(token);

//         let token = TokenTree::from(Punct::new(';', Spacing::Alone));
//         trace!("{}", TokenWrap::Token(token.clone()).to_str());
//         self.push(token);
//     }

//     /// Convert the token stream into a string for debugging purposes
//     pub(crate) fn to_str(&self) -> String {
//         fn recurse<'a>(iter: impl Iterator<Item = &'a TokenTree>) -> String {
//             let mut str = String::new();
//             for token in iter {
//                 match token {
//                     TokenTree::Group(g) => {
//                         str.push_str(&format!(
//                             "{}\n",
//                             TokenWrap::GroupStart(token.clone()).to_str()
//                         ));
//                         str.push_str(&recurse(g.stream().into_iter().collect::<Vec<_>>().iter()));
//                         str.push_str(&format!(
//                             "{}\n",
//                             TokenWrap::GroupEnd(token.clone()).to_str()
//                         ));
//                     }
//                     _ => str.push_str(&format!("{}\n", TokenWrap::Token(token.clone()).to_str())),
//                 }
//             }
//             str
//         }
//         recurse(&mut self.0.iter())
//     }

//     /// Convert the token stream into a TokenStream
//     pub(crate) fn into_token_stream(self) -> TokenStream {
//         self.0.into_iter().collect()
//     }
// }

/// Helper type for storing TokenTree variants
#[derive(Debug, Clone)]
pub(crate) enum TokenWrap {
    Token(TokenTree),
    GroupStart(TokenTree),
    GroupEnd(TokenTree),
}
impl TokenWrap {
    pub(crate) fn is_group_start(&self) -> bool {
        match self {
            TokenWrap::GroupStart(_) => true,
            _ => false,
        }
    }

    /// Take the token out of the wrapper
    pub(crate) fn take(self) -> TokenTree {
        match self {
            TokenWrap::Token(token) => token,
            TokenWrap::GroupStart(token) => token,
            TokenWrap::GroupEnd(token) => token,
        }
    }

    /// Convert the token into a string for debugging purposes
    pub(crate) fn to_str(&self) -> String {
        fn recurse(token: &TokenWrap, depth: &mut usize) -> String {
            *depth += 1;
            if *depth > 4 {
                return "".to_string();
            }

            let (start, end) = token.span_open();
            let mut out = if *depth > 1 {
                String::new()
            } else {
                String::from(format!("{: <12}", format!("{}..{}", start, end)))
            };

            if let TokenWrap::Token(TokenTree::Ident(i)) = token {
                if *depth > 1 {
                    out.push_str(&format!("{}: {}", "Ident", i.to_string()))
                } else {
                    out.push_str(&format!("{: <6} {}", "Ident", i.to_string()))
                }
            } else if let TokenWrap::Token(TokenTree::Punct(p)) = token {
                if *depth > 1 {
                    out.push_str(&format!("{}: {}", "Punct", p.to_string()))
                } else {
                    out.push_str(&format!("{: <6} {}", "Punct", p.to_string()))
                }
            } else if let TokenWrap::Token(TokenTree::Literal(l)) = token {
                if *depth > 1 {
                    out.push_str(&format!("{}: {}", "Lit", l.to_string()))
                } else {
                    out.push_str(&format!("{: <6} {}", "Lit", l.to_string()))
                }
            } else if let TokenWrap::Token(TokenTree::Group(g)) = token {
                let (open, close) = (open_delim(g.delimiter()), close_delim(g.delimiter()));
                out.push_str(&format!("{: <6} {}", "Group", open));

                let mut components = vec![];
                for token in g.stream().into_iter() {
                    components.push(recurse(&TokenWrap::Token(token), depth))
                }
                out.push_str(&components.join(&", "));

                out.push_str(&format!("{}", close));
            } else if let TokenWrap::GroupStart(TokenTree::Group(g)) = token {
                out.push_str(&format!("{: <6} {}", "Group", open_delim(g.delimiter())));
            } else if let TokenWrap::GroupEnd(TokenTree::Group(g)) = token {
                out.push_str(&format!("{: <6} {}", "Group", close_delim(g.delimiter())));
            }
            out
        }
        recurse(&self, &mut 0)
    }
}

/// Implement deref for TokenWrap
impl std::ops::Deref for TokenWrap {
    type Target = TokenTree;

    fn deref(&self) -> &Self::Target {
        match self {
            TokenWrap::Token(token) => token,
            TokenWrap::GroupStart(token) => token,
            TokenWrap::GroupEnd(token) => token,
        }
    }
}

/// Get the open delimiter for the group
fn open_delim(delim: Delimiter) -> &'static str {
    match delim {
        proc_macro2::Delimiter::Parenthesis => "(",
        proc_macro2::Delimiter::Brace => "{",
        proc_macro2::Delimiter::Bracket => "[",
        proc_macro2::Delimiter::None => "",
    }
}

/// Get the close delimiter for the group
fn close_delim(delim: Delimiter) -> &'static str {
    match delim {
        proc_macro2::Delimiter::Parenthesis => ")",
        proc_macro2::Delimiter::Brace => "}",
        proc_macro2::Delimiter::Bracket => "]",
        proc_macro2::Delimiter::None => "",
    }
}

/// Helper type for storing Group expansion
#[derive(Debug)]
pub(crate) enum TokenItem {
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::pos;
    use indoc::indoc;
    use proc_macro2::{TokenStream, TokenTree};
    use std::str::FromStr;

    // #[test]
    // fn is_variant_complicated() {
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
    // fn is_variant() {
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
    // fn is_field_multiple() {
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
    // fn is_field_generics() {
    //     let stream = to_tokens(indoc! {r#"
    //         struct Foo<T> {
    //             bar: Box<Option<('a &i32, T)>>,
    //         }
    //     "#});
    //     let group = stream[5].tokens();
    //     assert_eq!(group.is_field(), true);
    // }

    // #[test]
    // fn is_field_tuple_type() {
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
    // fn is_field() {
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
    // fn is_statement() {
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

    // #[test]
    // fn get() {
    //     let stream = to_tokens(indoc! {r#"
    //         struct Foo;
    //     "#});

    //     // Positive
    //     assert_eq!(stream.get(0).map(|x| x.to_string()), Some("struct".into()));
    //     assert_eq!(stream.get(1).map(|x| x.to_string()), Some("Foo".into()));
    //     assert_eq!(stream.get(2).map(|x| x.to_string()), Some(";".into()));
    //     assert!(stream.get(3).is_none());

    //     // Positive
    //     assert!(stream.get(-4).is_none());
    //     assert_eq!(stream.get(-3).map(|x| x.to_string()), Some("struct".into()));
    //     assert_eq!(stream.get(-2).map(|x| x.to_string()), Some("Foo".into()));
    //     assert_eq!(stream.get(-1).map(|x| x.to_string()), Some(";".into()));
    // }
}
