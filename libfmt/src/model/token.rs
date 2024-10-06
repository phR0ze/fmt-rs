use super::Position;
use proc_macro2::{Span, TokenTree};

/// Extension trait for proc_macro2::TokenTree
pub(crate) trait TokenExt {
    fn to_str(&self, span: &Span) -> String;
}

impl TokenExt for TokenTree {
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
            TokenTree::Ident(i) => {
                //
                out.push_str(&format!("{: <6} {}", "Ident", i.to_string()))
            }
            TokenTree::Punct(p) => {
                //
                out.push_str(&format!("{: <6} {}", "Punct", p.to_string()))
            }
            TokenTree::Literal(l) => {
                //
                out.push_str(&format!("{: <6} {}", "Lit", l.to_string()))
            }
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
        //        span.source_text().unwrap_or("<None>".into()),
        out
    }
}
