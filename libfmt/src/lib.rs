mod algorithm;
mod attr;
mod config;
mod convenience;
mod data;
mod error;
mod expr;
mod generics;
mod item;
mod iter;
mod lifetime;
mod lit;
mod r#macro;
mod pat;
mod path;
mod ring;
mod stmt;
mod token;
mod ty;

use std::io;

pub use config::*;
pub use error::{Error, Result};

use crate::algorithm::Engine;

/// Format the given source
pub fn format<T: io::Write>(src: &mut T) -> Result<()> {
    src.write_all(b"Hello, world!")
        .map_err(|e| Error::new("failed to write to file").wrap_io(e))
}

pub fn format_syn_file(file: &syn::File) -> Result<String> {
    let mut p = Engine::new();
    p.file(file);
    Ok(p.eof())
}

impl Engine {
    pub(crate) fn file(&mut self, file: &syn::File) {
        self.cbox(0);
        self.inner_attrs(&file.attrs);
        for item in &file.items {
            self.item(item);
        }
        self.end();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;
    use proc_macro2::{Delimiter, Group, TokenStream};
    use quote::quote;

    // fn test(tokens: TokenStream, expected: &str) {
    //     let syntax_tree: syn::File = syn::parse2(tokens).unwrap();
    //     let pretty = format_syn_file(&syntax_tree).unwrap();
    //     assert_eq!(pretty, expected);
    // }

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
}
