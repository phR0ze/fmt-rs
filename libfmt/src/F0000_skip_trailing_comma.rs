use crate::{engine::Engine, model::Scan};

impl Engine {
    /// If the given value is a comma and the scan buffer still has more tokens and the next token
    /// is an End token, then skip the trailing comma.
    /// * Feature: F0000
    pub fn skip_trailing_comma(&self, value: &str) -> bool {
        if !self.config.skip_trailing_comma() {
            return false;
        }

        if value == "," && !self.scan_buf.is_empty() {
            if !self.config.smart_wrapping() {
                if let Some(Scan::End) = self.scan_buf.get(0).map(|x| &x.token) {
                    if let Some(Scan::Break(_)) = self.scan_buf.get(0).map(|x| &x.token) {
                        if let Some(Scan::End) = self.scan_buf.get(1).map(|x| &x.token) {
                            return true;
                        }
                    }
                }
            } else {
                if let Some(Scan::Break(_)) = self.scan_buf.get(0).map(|x| &x.token) {
                    if let Some(Scan::End) = self.scan_buf.get(1).map(|x| &x.token) {
                        return true;
                    }
                }
            }
        }
        return false;
    }
}

#[cfg(test)]
mod tests {
    use crate::model::Config;
    use indoc::indoc;
    use tracing_test::traced_test;

    // Feature F0000: Skip trailing comma
    // ---------------------------------------------------------------------------------------------
    // rustfmt: leaves a trailing comma in parameter lists
    // Prettyplease: leaves a trailing comma in parameter lists
    #[test]
    fn item_macro() {
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