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

use core::str::FromStr;
pub(crate) use engine::Engine;
use proc_macro2::TokenStream;
use tracing::trace;

// Re-export the public API
pub use config::*;
pub use error::{Error, Result};

/// Format the given source string
pub fn format_str(source: &str) -> Result<String> {
    let mut engine = Engine::new(source);
    engine.format()?;
    Ok(engine.print())
}

impl Engine {
    pub(crate) fn format(&mut self) -> Result<()> {
        trace!("Formatting");

        // Parse source into token stream
        let tokens = TokenStream::from_str(&self.src)
            .map_err(|e| Error::new("failed to parse source into token stream").wrap_lex(e))?;

        // Pre-process the token stream for comment locational information
        self.pre_process_comments(tokens.clone(), &mut 0);

        // Parse the syntax tree
        let ast: syn::File = syn::parse2(tokens)
            .map_err(|e| Error::new("failed to parse token stream into syntax tree").wrap_syn(e))?;
        self.scan_begin_consistent(0);
        self.inner_attrs(&ast.attrs);
        for item in &ast.items {
            self.item(item);
            // test
            // item.to_token_stream().to_string();
        }
        self.scan_end();

        Ok(())
    }

    #[cfg(debug_assertions)]
    pub(crate) fn debug_dump(&self) {
        println!("{}", self);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;

    #[test]
    fn test_allow_one_empty_line() {
        let source = indoc! {r#"

            println!("{}", "1",);
        "#};

        assert_eq!(
            format_str(source).unwrap(),
            indoc! {r#"
                
                println!("{}", "1");
            "#}
        );
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
    // #[test]
    // fn test_macro_horizontal_aligment() {
    //     // rustfmt: aligns vertically regardless of readability
    //     let out = fmt(quote! {
    //         impl fmt::Display for Example {
    //             fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    //                 write!(
    //                     f,
    //                     "{} {} {} {} {} {} {} {}",
    //                     "1", "2", "3", "4", "5", "6", "7", "8"
    //                 )?;
    //             }
    //         }
    //     });
    //     println!("{}", out);
    //     assert_eq!(
    //         out,
    //         indoc! {r#"
    //             impl fmt::Display for Example {
    //                 fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    //                     write!(f, "{} {} {} {} {} {} {} {}", "1", "2", "3", "4", "5", "6", "7", "8")?;
    //                 }
    //             }
    //         "#},
    //     );
    // }

    // #[test]
    // fn test_struct_definition() {
    //     let out = fmt(quote! {
    //         struct Foo {
    //             a: i32,
    //             b: i32,
    //         }
    //     });
    //     assert_eq!(
    //         out,
    //         indoc! {r#"
    //             struct Foo {
    //                 a: i32,
    //                 b: i32,
    //             }
    //         "#},
    //     );
    // }

    // #[test]
    // fn test_allow_empty_line_before_comment() {
    //     let out = fmt(quote! {

    //         // This is a test
    //         println!("{}", "1",);
    //     });
    //     assert_eq!(out, "println!(\"{}\", \"1\");\n");
    // }

    // #[test]
    // fn test_allow_one_empty_line() {
    //     let out = fmt(quote! {

    //         println!("{}", "1",);
    //     });
    //     assert_eq!(out, "\nprintln!(\"{}\", \"1\");\n");
    // }

    #[test]
    fn test_skip_trailing_comma() {
        let source = indoc! {r#"
            println!("{}", "1",);
        "#};

        assert_eq!(
            format_str(source).unwrap(),
            indoc! {r#"
                println!("{}", "1");
            "#}
        );
    }
}
