use super::Break;

#[derive(Debug, Copy, Clone)]
pub(crate) enum PrintFrame {
    Fits(Break),
    Broken(usize, Break),
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_print_frame_debug() {
        assert_eq!(
            format!("{:?}", PrintFrame::Fits(Break::Consistent)),
            "Fits(Consistent)",
        );
        assert_eq!(
            format!("{:?}", PrintFrame::Fits(Break::Inconsistent)),
            "Fits(Inconsistent)",
        );
        assert_eq!(
            format!("{:?}", PrintFrame::Broken(4, Break::Consistent)),
            "Broken(4, Consistent)",
        );
        assert_eq!(
            format!("{:?}", PrintFrame::Broken(4, Break::Inconsistent)),
            "Broken(4, Inconsistent)",
        );
    }
}
