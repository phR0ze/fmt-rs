use super::Position;
use proc_macro2::{Span, TokenStream, TokenTree};

/// Extension trait for proc_macro2::TokenTree
pub(crate) trait TokenExt {
    fn is_ident(&self) -> Option<()>;
    fn is_punct(&self, char: char) -> Option<()>;
    fn to_str(&self, span: &Span) -> String;
}

impl TokenExt for TokenTree {
    /// Determine if the token is an identifier
    fn is_ident(&self) -> Option<()> {
        match self {
            TokenTree::Ident(_) => Some(()),
            _ => None,
        }
    }

    /// Determine if the token is the given punctuation character
    fn is_punct(&self, char: char) -> Option<()> {
        match self {
            TokenTree::Punct(p) => p.as_char() == char,
            _ => false,
        }
        .then_some(())
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

    /// Get the last token in the stream
    pub(crate) fn last(&self) -> Option<&TokenTree> {
        self.0.last()
    }

    /// Push a new token onto the end of the token stream
    pub(crate) fn push(&mut self, token: TokenTree) {
        self.0.push(token);
    }

    /// Determine if the most recent tokens are a statement
    pub(crate) fn is_statement(&self) -> bool {
        if self.0.len() > 0 {
            if let Some(TokenTree::Punct(p)) = self.get(-1) {
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
            // Check for the comma
            if let Some(TokenTree::Punct(p)) = self.get(-1) {
                if p.as_char() == ',' {
                    // Consume until we find the colon or end
                    let mut i = -2;
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
            // Check for the comma
            if let Some(TokenTree::Punct(p)) = self.get(-1) {
                if p.as_char() == ',' {
                    // Consume until we find a comma or the end
                    let mut i = -2;
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
    fn test_is_variant_complicated() {
        let stream = to_tokens(indoc! {r#"
            enum Foo {
                A(String),
                B((&'a i32, i32)),
            }
        "#});
        println!("{}", stream.to_str());
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
        let group = stream[2].tokens();
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
