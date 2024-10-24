#[cfg(test)]
mod tests {
    use indoc::indoc;
    use tracing_test::traced_test;

    // Feature F0001: Comments
    // ---------------------------------------------------------------------------------------------
    #[test]
    fn comment_trailing_item_macro() {
        let source = indoc! {r#"
            println!("Hello"); // Hello
        "#};
        assert_eq!(
            crate::format_str(None, source).unwrap(),
            indoc! {r#"
                println!("Hello"); // Hello
            "#},
        );
    }

    // Feature F0001: Comments
    #[test]
    fn comment_trailing_item_trait() {
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
            crate::format_str(None, source).unwrap(),
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

    // Feature F0001: Comments
    #[test]
    fn comment_trailing_item_struct() {
        let source = indoc! {r#"
            struct Foo1; // Foo1 struct
            struct Foo2 { // Foo2 struct
                a: i32,     // A field
                b: i32,     // B field
            }
        "#};
        assert_eq!(
            crate::format_str(None, source).unwrap(),
            indoc! {r#"
                struct Foo1; // Foo1 struct
                struct Foo2 { // Foo2 struct
                    a: i32, // A field
                    b: i32, // B field
                }
            "#},
        );
    }

    // Feature F0001: Comments
    #[test]
    fn comment_trailing_item_static() {
        let source = indoc! {r#"
            static FOO: i32 = 42; // Foo
        "#};
        assert_eq!(
            crate::format_str(None, source).unwrap(),
            indoc! {r#"
                static FOO: i32 = 42; // Foo
            "#},
        );
    }

    // Feature F0001: Comments
    #[test]
    fn comment_trailing_item_mod() {
        let source = indoc! {r#"
            mod foo; // Foo
            mod foo2 { // Foo
                fn foo() {
                    println!("Hello");
                }
            }
        "#};
        assert_eq!(
            crate::format_str(None, source).unwrap(),
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

    // Feature F0001: Comments
    #[test]
    fn comment_trailing_item_impl() {
        let source = indoc! {r#"
            struct Foo;

            impl Foo { // Foo
                fn foo() {
                    println!("Hello");
                }
            }
        "#};
        assert_eq!(
            crate::format_str(None, source).unwrap(),
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

    // Feature F0001: Comments
    #[test]
    fn comment_trailing_item_func() {
        let source = indoc! {r#"
            fn foo() { // Hello
                println!("Hello");
            }
        "#};
        assert_eq!(
            crate::format_str(None, source).unwrap(),
            indoc! {r#"
                fn foo() { // Hello
                    println!("Hello");
                }
            "#},
        );
    }

    // Feature F0001: Comments
    #[test]
    fn comment_trailing_item_enum() {
        let source = indoc! {r#"
            enum Enum2 { // Enum2
                A, // A variant
                B, // B variant
            }
        "#};
        assert_eq!(
            crate::format_str(None, source).unwrap(),
            indoc! {r#"
                enum Enum2 { // Enum2
                    A, // A variant
                    B, // B variant
                }
            "#},
        );
    }

    // Feature F0001: Comments
    #[test]
    fn comment_trailing_const() {
        let source = indoc! {r#"
            const FOO: i32 = 42; // Foo
        "#};
        assert_eq!(
            crate::format_str(None, source).unwrap(),
            indoc! {r#"
                const FOO: i32 = 42; // Foo
            "#},
        );
    }

    // Feature F0001: Comments
    #[test]
    fn comment_only_comments() {
        let source = indoc! {r#"
            // foo
        "#};
        assert_eq!(
            crate::format_str(None, source).unwrap(),
            indoc! {r#"
                // foo
            "#},
        );
    }

    // Feature F0001: Comments
    #[test]
    fn multi_comment_types() {
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
            crate::format_str(None, source).unwrap(),
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

    // Feature F0001: Comments
    #[test]
    fn block_comment() {
        let source = indoc! {r#"
            /**************************
             * ///  A foo struct  \\\ *
             *  - one                 *
             *  - two                 *
             *************************/
            struct Foo;
        "#};
        assert_eq!(
            crate::format_str(None, source).unwrap(),
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

    // Feature F0001: Comments
    #[test]
    fn struct_definition_with_comments_and_whitespace() {
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
            crate::format_str(None, source).unwrap(),
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

    // Feature F0001: Comments
    #[test]
    fn only_allow_one_empty_line() {
        let source = indoc! {r#"


            println!("{}", "1",);
        "#};

        assert_eq!(
            crate::format_str(None, source).unwrap(),
            indoc! {r#"

                println!("{}", "1");
            "#}
        );
    }
}
