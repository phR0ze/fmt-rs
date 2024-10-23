pub(crate) mod attrs;
pub(crate) mod comments;
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
        self.scan_begin_consistent(0);

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

    // Feature C0002: Smart Wrapping
    // * rustfmt: aligns vertically regardless of readability
    // * libfmt: smart wraps at line limits improving readability
    #[test]
    fn test_smart_wrapping_for_macro_params() {
        // rustfmt formatting
        let source = indoc! {r#"
            println!(
                "{} {} {} {} {} {} {} {} {} {} {} {} {} {} {} {} {} {}",
                "1",
                "2",
                "3",
                "4",
                "5",
                "6",
                "7",
                "8",
                "9",
                "10",
                "11",
                "12",
                "13",
                "14",
                "15",
                "16",
                "17",
                "18",
            );
        "#};

        // libfmt formatting
        assert_eq!(
            format_str(None, source).unwrap(),
            indoc! {r#"
                println!("{} {} {} {} {} {} {} {} {} {} {} {} {} {} {} {} {} {}", "1", "2", "3", "4",
                    "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18");
            "#},
        );

        let source = indoc! {r#"
            impl fmt::Display for Example {
                fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                    write!(
                        f,
                        "{} {} {} {} {} {} {} {}",
                        "1", "2", "3", "4", "5", "6", "7", "8",
                    )?;
                }
            }
        "#};
        assert_eq!(
            format_str(None, source).unwrap(),
            indoc! {r#"
                impl fmt::Display for Example {
                    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                        write!(f, "{} {} {} {} {} {} {} {}", "1", "2", "3", "4", "5", "6", "7", "8")?;
                    }
                }
            "#},
        );

        // Original macro formatting to ensure I don't break compatibility
        let source = indoc! {r#"
            macro_rules! add{
                ($a:expr)=>{
                    $a
                };
                ($a:expr,$b:expr)=>{
                    {
                        $a+$b
                    }
                };
                ($a:expr,$($b:tt)*)=>{
                    {
                        $a+add!($($b)*)
                    }
                }
            }
        "#};
        assert_eq!(
            format_str(None, source).unwrap(),
            indoc! {r#"
                macro_rules! add {
                    ($a:expr) => {
                        $a
                    };
                    ($a:expr,$b:expr) => {
                        { $a +$b }
                    };
                    ($a:expr,$($b:tt)*) => {
                        { $a + add!($($b)*) }
                    };
                }
            "#},
        );
    }

    // Feature C0001: Comments
    #[test]
    fn test_comment_trailing_item_macro() {
        let source = indoc! {r#"
            println!("Hello"); // Hello
        "#};
        assert_eq!(
            format_str(None, source).unwrap(),
            indoc! {r#"
                println!("Hello"); // Hello
            "#},
        );
    }

    // Feature C0001: Comments
    #[test]
    fn test_comment_trailing_item_trait() {
        let source = indoc! {r#"
            trait Foo { // Foo
                type Item; // Bar

                const FOO: i32 = 42; // Foo
                fn foo(); // Func foo
                fn foo2() { // Func foo2
                    println!("Hello");
                }
            }
        "#};
        assert_eq!(
            format_str(None, source).unwrap(),
            indoc! {r#"
                trait Foo { // Foo
                    type Item; // Bar

                    const FOO: i32 = 42; // Foo
                    fn foo(); // Func foo
                    fn foo2() { // Func foo2
                        println!("Hello");
                    }
                }
            "#},
        );
    }

    // Feature C0001: Comments
    #[test]
    fn test_comment_trailing_item_struct() {
        let source = indoc! {r#"
            struct Foo1; // Foo1 struct
            struct Foo2 { // Foo2 struct
                a: i32,     // A field
                b: i32,     // B field
            }
        "#};
        assert_eq!(
            format_str(None, source).unwrap(),
            indoc! {r#"
                struct Foo1; // Foo1 struct
                struct Foo2 { // Foo2 struct
                    a: i32, // A field
                    b: i32, // B field
                }
            "#},
        );
    }

    // Feature C0001: Comments
    #[test]
    fn test_comment_trailing_item_static() {
        let source = indoc! {r#"
            static FOO: i32 = 42; // Foo
        "#};
        assert_eq!(
            format_str(None, source).unwrap(),
            indoc! {r#"
                static FOO: i32 = 42; // Foo
            "#},
        );
    }

    // Feature C0001: Comments
    #[test]
    fn test_comment_trailing_item_mod() {
        let source = indoc! {r#"
            mod foo; // Foo
            mod foo2 { // Foo
                fn foo() {
                    println!("Hello");
                }
            }
        "#};
        assert_eq!(
            format_str(None, source).unwrap(),
            indoc! {r#"
                mod foo; // Foo
                mod foo2 { // Foo
                    fn foo() {
                        println!("Hello");
                    }
                }
            "#},
        );
    }

    // Feature C0001: Comments
    #[test]
    fn test_comment_trailing_item_impl() {
        let source = indoc! {r#"
            struct Foo;

            impl Foo { // Foo
                fn foo() {
                    println!("Hello");
                }
            }
        "#};
        assert_eq!(
            format_str(None, source).unwrap(),
            indoc! {r#"
                struct Foo;

                impl Foo { // Foo
                    fn foo() {
                        println!("Hello");
                    }
                }
            "#},
        );
    }

    // Feature C0001: Comments
    #[test]
    fn test_comment_trailing_item_func() {
        let source = indoc! {r#"
            fn foo() { // Hello
                println!("Hello");
            }
        "#};
        assert_eq!(
            format_str(None, source).unwrap(),
            indoc! {r#"
                fn foo() { // Hello
                    println!("Hello");
                }
            "#},
        );
    }

    // Feature C0001: Comments
    #[test]
    fn test_comment_trailing_item_enum() {
        let source = indoc! {r#"
            enum Enum2 { // Enum2
                A, // A variant
                B, // B variant
            }
        "#};
        assert_eq!(
            format_str(None, source).unwrap(),
            indoc! {r#"
                enum Enum2 { // Enum2
                    A, // A variant
                    B, // B variant
                }
            "#},
        );
    }

    // Feature C0001: Comments
    #[test]
    fn test_comment_trailing_const() {
        let source = indoc! {r#"
            const FOO: i32 = 42; // Foo
        "#};
        assert_eq!(
            format_str(None, source).unwrap(),
            indoc! {r#"
                const FOO: i32 = 42; // Foo
            "#},
        );
    }

    // Feature C0001: Comments
    #[test]
    fn test_comment_only_comments() {
        let source = indoc! {r#"
            // foo
        "#};
        assert_eq!(
            format_str(None, source).unwrap(),
            indoc! {r#"
                // foo
            "#},
        );
    }

    // Feature C0001: Comments
    #[test]
    fn test_multi_comment_types() {
        let source = indoc! {r#"
            use libfmt::Result;

            fn main() -> Result<()> {
                let subscriber = FmtSubscriber::builder()
                    .with_max_level(Level::TRACE)
                    .finish();
                tracing::subscriber::set_global_default(subscriber).unwrap();

                // Pass in an example
                let path = "examples/dump.rs";
                let formatted = libfmt::format_file(path)?;
                print!("{}", formatted);

                Ok(())
            }
        "#};
        assert_eq!(
            format_str(None, source).unwrap(),
            indoc! {r#"
                use libfmt::Result;

                fn main() -> Result<()> {
                    let subscriber = FmtSubscriber::builder().with_max_level(Level::TRACE).finish();
                    tracing::subscriber::set_global_default(subscriber).unwrap();

                    // Pass in an example
                    let path = "examples/dump.rs";
                    let formatted = libfmt::format_file(path)?;
                    print!("{}", formatted);

                    Ok(())
                }
        "#}
        );
    }

    // Feature C0001: Comments
    #[test]
    fn test_block_comment() {
        let source = indoc! {r#"
            /**************************
             * ///  A foo struct  \\\ *
             *  - one                 *
             *  - two                 *
             *************************/
            struct Foo;
        "#};
        assert_eq!(
            format_str(None, source).unwrap(),
            indoc! {r#"
                /**************************
                 * ///  A foo struct  \\\ *
                 *  - one                 *
                 *  - two                 *
                 *************************/
                struct Foo;
            "#},
        );
    }

    // Feature C0001: Comments
    #[test]
    fn test_struct_definition_with_comments_and_whitespace() {
        let source = indoc! {r#"

            /// A foo struct
            struct Foo {

                // Field a
                a: i32,

                // Field b
                b: i32,
            }
        "#};
        assert_eq!(
            format_str(None, source).unwrap(),
            indoc! {r#"

                /// A foo struct
                struct Foo {

                    // Field a
                    a: i32,

                    // Field b
                    b: i32,
                }
            "#},
        );
    }

    // Feature C0001: Comments
    #[test]
    fn test_only_allow_one_empty_line() {
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

    // Feature C0000: Skip trailing comma
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
