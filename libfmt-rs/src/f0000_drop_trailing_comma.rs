// Feature F0000: Drop trailing comma
// ---------------------------------------------------------------------------------------------
// Problem statement: Rustfmt and Prettyplease both leave a trailing comma in parameter lists.
//
// Solution: Detect and drop the trailing comma when not in vertical listing format.
// ---------------------------------------------------------------------------------------------
use crate::{engine::Engine, model::Scan};

impl Engine {
    /// If the given value is a comma and the scan buffer still has more tokens and the next token
    /// is an End token, then skip the trailing comma.
    /// * Feature: F0000
    pub fn drop_trailing_comma(&self, value: &str) -> bool {
        if !self.config.drop_trailing_comma() {
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
    use indoc::indoc;

    #[test]
    fn item_macro() {
        // rustfmt
        let source = indoc! {r#"
            println!("{}", "1",);
        "#};

        // libfmt-rs
        assert_eq!(
            crate::format_str(None, source).unwrap(),
            indoc! {r#"
                println!("{}", "1");
            "#}
        );
    }
}
