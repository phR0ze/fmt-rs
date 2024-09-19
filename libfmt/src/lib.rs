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
    use proc_macro2::{LexError, Span, TokenStream, TokenTree};
    use quote::quote;

    struct Code {
        text: &'static str,
    }

    impl Code {
        fn from_str(text: &'static str) -> Result<Self> {
            // Parse the tokens
            let tokens = TokenStream::from_str(text)
                .map_err(|e| Error::new("failed to parse tokens").wrap_lex(e))?;

            // Build our formatting object
            let mut code = Self { text };

            // Format the tokens
            code.format(tokens, &mut 0);

            Ok(code)
        }

        fn format(&mut self, tokens: TokenStream, level: &mut usize) {
            for token in tokens {
                match token {
                    TokenTree::Ident(ident) => {
                        println!("{}", span_to_str("Ident:", ident.span(), *level));
                    }
                    TokenTree::Literal(literal) => {
                        println!("{}", span_to_str("Literal:", literal.span(), *level));
                    }
                    TokenTree::Group(group) => {
                        *level += 1;
                        self.format(group.stream(), level);
                    }
                    TokenTree::Punct(punct) => {
                        println!("{}", span_to_str("Punct:", punct.span(), *level));
                    }
                }
            }
        }
    }

    #[test]
    fn test_process_token_stream() {
        Code::from_str("// foo\nprintln!(\"{}\", \"1\");").unwrap();
    }

    fn span_to_str(name: &str, span: Span, level: usize) -> String {
        let start = span.start();
        let end = span.end();
        format!(
            "{:indent$}{: <8} {: <7} {: <7} {: <7} ({})",
            "",
            name,
            format!("{}..{}", start.line, end.line),
            format!("{}..{}", start.column, end.column),
            format!("{:?}", span.byte_range()),
            span.source_text().unwrap_or("<None>".into()),
            indent = level * 2
        )
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
