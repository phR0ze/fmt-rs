use crate::engine::Engine;

impl Engine {
    /// Smart wrapping for block begin falling back on original behavior if disabled.
    /// * Features C0002: Smart wrapping
    pub fn smart_wrap_begin(&mut self) {
        if !self.config.smart_wrapping() {
            self.scan_begin_vertical(self.config.indent);
        } else {
            self.scan_begin_horizontal(self.config.indent);
        }
    }

    /// Smart wrap for block body begin falling back on original behavior if disabled.
    /// * Features C0002: Smart wrapping
    pub fn smart_wrap_body_begin(&mut self) {
        if !self.config.smart_wrapping() {
            self.scan_begin_horizontal(0);
        }
    }

    /// Smart wrapper for zerobreak. In some of the original cases a zero break was used.
    /// * Features C0002: Smart wrapping
    pub fn smart_wrap_break_spaces(&mut self) {
        if !self.config.smart_wrapping() {
            self.scan_zero_break();
        }
    }

    /// Smart wrap for block body end falling back on original behavior if disabled.
    /// * Features C0002: Smart wrapping
    pub fn smart_wrap_body_end(&mut self) {
        if !self.config.smart_wrapping() {
            self.scan_end();
        }
    }
}

#[cfg(test)]
mod tests {
    use indoc::indoc;
    use tracing_test::traced_test;

    // Feature C0002: Smart Wrapping
    // ---------------------------------------------------------------------------------------------
    // * rustfmt: aligns vertically regardless of readability
    // * libfmt: smart wraps at line limits improving readability

    // #[test]
    // fn test_smart_wrapping_for_fn_params() {
    //     // rustfmt: aligns vertically regardless of readability
    //     let source = indoc! {r#"
    //         fn reset(&mut line: String, &mut only_space: String, &mut comment_line: String, &mut prev_char: String) {
    //             println!("{}{}{}{}", &line, &only_space, &comment_line, &prev_char);
    //         }
    //     "#};
    //     assert_eq!(
    //         crate::format_str(None, source).unwrap(),
    //         indoc! {r#"
    //             fn reset(&mut line: String, &mut only_space: String, &mut comment_line: String,
    //                 &mut prev_char: String) {
    //                 println!("{}{}{}{}", & line, & only_space, & comment_line, & prev_char);
    //             }
    //         "#},
    //     );

    //     // zero param case
    //     let source = indoc! {r#"
    //         impl Foo {
    //             fn reset() {
    //                 println!("{}", "1");
    //             }
    //         }
    //     "#};
    //     assert_eq!(
    //         crate::format_str(None, source).unwrap(),
    //         indoc! {r#"
    //             impl Foo {
    //                 fn reset() {
    //                     println!("{}", "1");
    //                 }
    //             }
    //         "#},
    //     );
    // }

    #[test]
    fn test_smart_wrapping_for_macro_params() {
        // rustfmt: aligns vertically regardless of readability
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
            crate::format_str(None, source).unwrap(),
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
            crate::format_str(None, source).unwrap(),
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
            crate::format_str(None, source).unwrap(),
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
}
