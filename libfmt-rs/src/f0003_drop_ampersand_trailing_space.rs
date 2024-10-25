// Feature F0003: Drop ampersand trailing space
// ---------------------------------------------------------------------------------------------
// Problem statement: Prettyplease is injecting a space after references in function parameters
//
// Solution: Detect and drop this space
// ---------------------------------------------------------------------------------------------
use crate::engine::Engine;

impl Engine {
    //
}

#[cfg(test)]
mod tests {
    use indoc::indoc;

    // Prettyplease is injecting spaces after references in function parameters
    // e.g. println!("{}{}{}{}", & line, & only_space, & comment_line, & prev_char);
    #[test]
    fn fn_params_references() {
        let source = indoc! {r#"
            println!("{}", & line);
        "#};
        assert_eq!(
            crate::format_str(None, source).unwrap(),
            indoc! {r#"
                println!("{}", &line);
            "#},
        );
    }
}
