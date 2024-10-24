pub(crate) mod attrs;
pub(crate) mod comments;
mod convenience;
mod data;
mod engine;
mod error;
mod expr;
mod feature_comments_tests;
mod feature_smart_wrap;
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
use model::Config;
use std::path::Path;
use tracing::trace;

// Re-export the public API
pub use error::{Error, Result};

/// Format the given source file
pub fn format_file<T: AsRef<Path>>(path: T) -> Result<String> {
    let path = path.as_ref();
    let source = std::fs::read_to_string(path)
        .map_err(|e| Error::new("failed to read source file").wrap_io(e))?;
    format_str(None, &source)
}

/// Format the given source string
pub fn format_str(config: Option<Config>, source: &str) -> Result<String> {
    let mut engine = Engine::new(source, config.unwrap_or_default());
    engine.format()?;
    Ok(engine.print())
}

impl Engine {
    pub(crate) fn format(&mut self) -> Result<()> {
        trace!("Formatting");

        // Pre-process the token stream for comment locational information
        let tokens = comments::inject(&self.config, &self.src)?;

        // Parse the syntax tree from the token stream
        let ast: syn::File = syn::parse2(tokens)
            .map_err(|e| Error::new("failed to parse token stream into syntax tree").wrap_syn(e))?;
        self.scan_begin_vertical(0);

        // Check for any inner attributes for the file
        self.inner_attrs(&ast.attrs);
        for item in &ast.items {
            self.item(item);
        }
        self.scan_end();

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;
    use tracing_test::traced_test;

    // #[test]
    // fn test_learning_breaks() {
    //     let source = indoc! {r#"
    //         static NUMBERS: &'static [i32] = &[
    //             1,
    //             2,
    //             3,
    //             4,
    //             5,
    //             6,
    //             7,
    //             8,
    //             9,
    //             10,
    //             11,
    //             12,
    //             13,
    //             14,
    //             15,
    //             16,
    //             17,
    //             18
    //         ];
    //     "#};

    //     assert_eq!(
    //         format_str(None, source).unwrap(),
    //         indoc! {r#"
    //             static NUMBERS: &'static [i32] = &[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18];
    //         "#}
    //     );
    // }

    // Feature C0000: Skip trailing comma
    // ---------------------------------------------------------------------------------------------
    // rustfmt: leaves a trailing comma in parameter lists
    // Prettyplease: leaves a trailing comma in parameter lists
    #[test]
    fn test_skip_trailing_comma() {
        let source = indoc! {r#"
            println!("{}", "1",);
        "#};

        assert_eq!(
            format_str(None, source).unwrap(),
            indoc! {r#"
                println!("{}", "1");
            "#}
        );
    }
}
