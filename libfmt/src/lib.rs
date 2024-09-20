mod attr;
mod config;
mod convenience;
mod data;
mod engine;
mod error;
mod expr;
mod generics;
mod item;
mod iter;
mod lifetime;
mod lit;
mod r#macro;
mod model;
mod pat;
mod path;
mod stmt;
mod token;
mod ty;

pub(crate) use engine::Engine;
use std::io;
use tracing::trace;

// Re-export the public API
pub use config::*;
pub use error::{Error, Result};

/// Format the given source
pub fn format<T: io::Write>(src: &mut T) -> Result<()> {
    src.write_all(b"Hello, world!")
        .map_err(|e| Error::new("failed to write to file").wrap_io(e))
}

pub fn format_syn_file(file: &syn::File) -> Result<String> {
    let mut p = Engine::new();
    p.format(file);
    Ok(p.eof())
}

impl Engine {
    pub(crate) fn format(&mut self, file: &syn::File) {
        trace!("Formatting");

        self.scan_begin_consistent(0);
        self.inner_attrs(&file.attrs);
        for item in &file.items {
            self.item(item);
        }
        self.scan_end();
    }

    #[cfg(debug_assertions)]
    pub(crate) fn debug_dump(&self) {
        println!("{}", self);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use core::str::FromStr;
    use indoc::indoc;
    use proc_macro2::{LineColumn, Span, TokenStream, TokenTree};
    use quote::quote;
    use std::{collections::HashMap, ops::Range};

    struct Comment {
        src: &'static str,   // Comment string
        range: Range<usize>, // Location of next token in the source code
        start: LineColumn,   // Start of the next token in the source code
        end: LineColumn,     // End of the next token in the source code
    }

    struct Code {
        src: &'static str,                        // Original source code
        offset: usize,                            // Track the offset in the source code
        comments: HashMap<Range<usize>, Comment>, // Comments in the source code
    }

    impl Code {
        fn from_str(text: &'static str) -> Result<Self> {
            // Parse the tokens
            let tokens = TokenStream::from_str(text)
                .map_err(|e| Error::new("failed to parse tokens").wrap_lex(e))?;

            // Build our formatting object
            let mut code = Self {
                src: text,
                offset: 0,
                comments: HashMap::new(),
            };

            // Collect comments
            code.collect_comments(tokens, &mut 0);

            Ok(code)
        }

        /// Collect comments from the original source using the token stream to provide location
        /// information relative the associated tokens. Comments are being defined as any lines
        /// of text that were dropped during the conversion into tokens. This can be newlines,
        /// regular comments, and inner and outer doc comments.
        fn collect_comments(&mut self, tokens: TokenStream, level: &mut usize) {
            for token in tokens {
                match token {
                    TokenTree::Ident(ident) => {
                        self.process_span(ident.span());
                        println!("{}", self.span_to_str("Ident:", ident.span(), *level));
                    }
                    TokenTree::Literal(literal) => {
                        self.process_span(literal.span());
                        println!("{}", self.span_to_str("Literal:", literal.span(), *level));
                    }
                    TokenTree::Group(group) => {
                        let open = group.delim_span().open();
                        self.process_span(group.delim_span().open());
                        println!("{}", self.span_to_str("Group:", open, *level));
                        *level += 1;
                        let close = group.delim_span().close();
                        self.collect_comments(group.stream(), level);
                        self.process_span(group.delim_span().close());
                        println!("{}", self.span_to_str("Group:", close, *level));
                    }
                    TokenTree::Punct(punct) => {
                        self.process_span(punct.span());
                        println!("{}", self.span_to_str("Punct:", punct.span(), *level));
                    }
                }
            }
        }

        // Determine if the given span would indicate an associated comment and if so
        // store it. In either case advance the offset to account for the span.
        fn process_span(&mut self, span: Span) {
            let range = span.byte_range();

            // Comments exist if offset is not equal to the start of the range
            if range.start > self.offset {
                self.comments.insert(
                    range.clone(),
                    Comment {
                        src: &self.src[self.offset..range.start],
                        range: range.clone(),
                        start: span.start(),
                        end: span.end(),
                    },
                );
            };

            // Update the offset to the end of the token
            self.offset += range.end - self.offset;
        }

        fn span_to_str(&self, name: &str, span: Span, level: usize) -> String {
            let range = span.byte_range();
            let start = span.start();
            let end = span.end();
            let mut out = String::new();

            // Optionally print any comment that might exist
            if self.comments.contains_key(&range) {
                let comment = self.comments.get(&range).unwrap();
                out.push_str(&format!(
                    "Comment: {:indent$} ({})",
                    "",
                    comment.src,
                    indent = level * 2
                ));
            }

            // Create the string for the span
            //self.out.reserve(self.pending_indentation);
            out.push_str(&format!(
                "{:indent$}{: <8} {: <7} {: <7} {: <7} ({})",
                "",
                name,
                format!("{}..{}", start.line, end.line),
                format!("{}..{}", start.column, end.column),
                format!("{:?}", range),
                span.source_text().unwrap_or("<None>".into()),
                indent = level * 2
            ));
            out
        }
    }

    #[test]
    fn test_process_token_stream() {
        Code::from_str("// foo\nprintln!(\"{}\", \"1\");").unwrap();
    }

    fn fmt(tokens: TokenStream) -> String {
        let syntax_tree: syn::File = syn::parse2(tokens).unwrap();
        format_syn_file(&syntax_tree).unwrap()
    }

    // // Rustfmt will align the parameters vertically
    // // libfmt will align the parameters horizontally and wrap intelligently
    // #[test]
    // fn test_params_align_horizontally() {
    //     // rustfmt: aligns vertically regardless of readability
    //     let out = fmt(quote! {
    //         impl fmt::Display for Example {
    //             fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    //                 write!(
    //                     f,
    //                     "{} {} {} {} {} {} {} {} {} {} {} {} {} {} {} {} {}",
    //                     "1",
    //                     "2",
    //                     "3",
    //                     "4",
    //                     "5",
    //                     "6",
    //                     "7",
    //                     "8",
    //                     "9",
    //                     "10",
    //                     "11",
    //                     "12",
    //                     "13",
    //                     "14",
    //                     "15",
    //                     "16",
    //                     "17"
    //                 )?;
    //             }
    //         }
    //     });
    //     println!("{}", out);
    //     assert_eq!(
    //         out,
    //         // libfmt: breaks at limt and wraps intelligently
    //         indoc! {r#"
    //             impl fmt::Display for Example {
    //                 fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    //                     write!(f, "{} {} {} {} {} {} {} {} {} {} {} {} {} {} {} {} {}", "1", "2",
    //                         "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15",
    //                         "16", "17"
    //                     )
    //                 }
    //             }
    //         "#},
    //     );
    // }

    // Rustfmt will align macro parameters vertically
    // libfmt will align macro parameters horizontally and wrap intelligently
    // Issues:
    // - trailing comma is left on and looks ugly
    // - line length exceeds 100 characters
    #[test]
    fn test_macro_horizontal_aligment() {
        // rustfmt: aligns vertically regardless of readability
        let out = fmt(quote! {
            impl fmt::Display for Example {
                fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                    write!(
                        f,
                        "{} {} {} {} {} {} {} {}",
                        "1", "2", "3", "4", "5", "6", "7", "8"
                    )?;
                }
            }
        });
        println!("{}", out);
        assert_eq!(
            out,
            indoc! {r#"
                impl fmt::Display for Example {
                    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                        write!(f, "{} {} {} {} {} {} {} {}", "1", "2", "3", "4", "5", "6", "7", "8")?;
                    }
                }
            "#},
        );
    }

    #[test]
    fn test_struct_definition() {
        let out = fmt(quote! {
            struct Foo {
                a: i32,
                b: i32,
            }
        });
        assert_eq!(
            out,
            indoc! {r#"
                struct Foo {
                    a: i32,
                    b: i32,
                }
            "#},
        );
    }

    #[test]
    fn test_allow_empty_line_before_comment() {
        let out = fmt(quote! {

            // This is a test
            println!("{}", "1",);
        });
        assert_eq!(out, "println!(\"{}\", \"1\");\n");
    }

    #[test]
    fn test_allow_one_empty_line() {
        let out = fmt(quote! {

            println!("{}", "1",);
        });
        assert_eq!(out, "\nprintln!(\"{}\", \"1\");\n");
    }

    #[test]
    fn test_skip_trailing_comma() {
        let out = fmt(quote! {
            println!("{}", "1",);
        });
        assert_eq!(out, "println!(\"{}\", \"1\");\n");
    }
}
