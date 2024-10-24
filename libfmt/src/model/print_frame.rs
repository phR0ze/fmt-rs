use super::Flow;

#[derive(Debug, Copy, Clone)]
pub(crate) enum PrintFrame {
    Fits(Flow),
    Broken(usize, Flow),
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_print_frame_debug() {
        assert_eq!(
            format!("{:?}", PrintFrame::Fits(Flow::Vertical)),
            "Fits(Vertical)",
        );
        assert_eq!(
            format!("{:?}", PrintFrame::Fits(Flow::Horizontal)),
            "Fits(Horizontal)",
        );
        assert_eq!(
            format!("{:?}", PrintFrame::Broken(4, Flow::Vertical)),
            "Broken(4, Vertical)",
        );
        assert_eq!(
            format!("{:?}", PrintFrame::Broken(4, Flow::Horizontal)),
            "Broken(4, Horizontal)",
        );
    }
}
