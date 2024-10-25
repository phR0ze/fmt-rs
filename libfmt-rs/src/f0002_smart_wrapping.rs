// Feature F0002: Smart wrapping
// ---------------------------------------------------------------------------------------------
// Problem statement: Rustfmt and Prettyplease both favor extreme vertical formatting.
//
// Solution: Default flow control to horizontal formatting thus using more of the available
// screen real estate.
// ---------------------------------------------------------------------------------------------
use crate::engine::Engine;

impl Engine {
    /// Check if we just wrapped.
    ///
    /// * **WARNING** only works once as it clears the value
    /// * **WARNING** this only works in as much as the accumulation in block of code is tracking it
    pub fn smart_wrapping_wrapped(&mut self) -> bool {
        if self.config.smart_wrapping() {
            if self.right_total.delta() > self.config.max_line_width {
                self.right_total.reset_delta();
                return true;
            }
        }
        false
    }

    /// Smart wrapping for block begin falling back on original behavior if disabled.
    /// * Features F0002: Smart wrapping
    pub fn smart_wrap_begin_default(&mut self) {
        self.smart_wrap_begin_indent(self.config.indent);
    }

    /// Smart wrapping for block begin falling back on original behavior if disabled.
    /// * Features F0002: Smart wrapping
    pub fn smart_wrap_begin_zero(&mut self) {
        self.smart_wrap_begin_indent(0);
    }

    /// Smart wrapping for block begin falling back on original behavior if disabled.
    /// * Features F0002: Smart wrapping
    pub fn smart_wrap_no_begin_zero(&mut self) {
        if !self.config.smart_wrapping() {
            self.scan_begin_vertical(0);
        }
    }

    /// Smart wrapping for block begin falling back on original behavior if disabled.
    /// * Features F0002: Smart wrapping
    pub fn smart_wrap_begin_indent(&mut self, n: isize) {
        if !self.config.smart_wrapping() {
            self.scan_begin_vertical(n);
        } else {
            self.scan_begin_horizontal(n);
        }
    }

    /// Smart wrap for block body end falling back on original behavior if disabled.
    /// * Features F0002: Smart wrapping
    pub fn smart_wrap_no_end(&mut self) {
        if !self.config.smart_wrapping() {
            self.scan_end();
        }
    }
}

#[cfg(test)]
mod tests {
    use indoc::indoc;

    #[test]
    fn fn_params() {
        let source = indoc! {r#"
            fn reset(
                &mut line: String,
                &mut only_space: String,
                &mut comment_line: String,
                &mut prev_char: String,
            ) {
                println!("{}{}{}{}", &line, &only_space, &comment_line, &prev_char);
            }
        "#};
        assert_eq!(
            crate::format_str(None, source).unwrap(),
            indoc! {r#"
                fn reset(&mut line: String, &mut only_space: String, &mut comment_line: String,
                    &mut prev_char: String)
                {
                    println!("{}{}{}{}", &line, &only_space, &comment_line, &prev_char);
                }
            "#},
        );
    }

    // Test fixes for rustfmt
    // * aligns vertically regardless of readability
    // * trailing comma when unnecessary
    //
    // TODO
    // * spaces between the reference indicators and the type
    #[test]
    fn fn_where() {
        let source = indoc! {r#"
            fn reset<T, U, V, W, X>(
                &mut newline: T,
                &mut only_space: U,
                &mut comment_line: V,
                &mut prev_char: W,
                &mut next_char: X,
            ) where
                T: Display,
                U: Display,
                V: Display,
                W: Display,
                X: Display,
            {
                println!(
                    "{}{}{}{}{}",
                    &line, &only_space, &comment_line, &prev_char, &next_char
                );
            }
        "#};
        assert_eq!(
            crate::format_str(None, source).unwrap(),
            indoc! {r#"
                fn reset<T, U, V, W, X>(&mut newline: T, &mut only_space: U, &mut comment_line: V,
                    &mut prev_char: W, &mut next_char: X)
                where T: Display, U: Display, V: Display, W: Display, X: Display
                {
                    println!("{}{}{}{}{}", &line, &only_space, &comment_line, &prev_char, &next_char);
                }
            "#},
        );
    }

    #[test]
    fn zero_param_for_regression_checking() {
        let source = indoc! {r#"
            impl Foo {
                fn reset() {
                    println!("{}", "1");
                }
            }
        "#};
        assert_eq!(
            crate::format_str(None, source).unwrap(),
            indoc! {r#"
                impl Foo {
                    fn reset() {
                        println!("{}", "1");
                    }
                }
            "#},
        );
    }

    #[test]
    fn item_static() {
        let source = indoc! {r#"
            static NUMBERS: &'static [i32] = &[
                1,
                2,
                3,
                4,
                5,
                6,
                7,
                8,
                9,
                10,
                11,
                12,
                13,
                14,
                15,
                16,
                17,
                18
            ];
        "#};
        assert_eq!(
            crate::format_str(None, source).unwrap(),
            indoc! {r#"
                static NUMBERS: &'static [i32] = &[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18];
            "#}
        );
    }

    #[test]
    fn item_macro() {
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

        // libfmt-rs formatting
        assert_eq!(
            crate::format_str(None, source).unwrap(),
            indoc! {r#"
                println!("{} {} {} {} {} {} {} {} {} {} {} {} {} {} {} {} {} {}", "1", "2", "3", "4", "5", "6", "7",
                    "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18");
            "#},
        );
    }

    #[test]
    fn item_nested_macro() {
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
    }

    #[test]
    fn standard_macro_for_detecting_regressions() {
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
