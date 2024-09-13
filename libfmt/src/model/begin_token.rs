use super::Break;

/// Begin tokens can carry an offset, saying "how far to indent when you break inside this block",
/// as well as a flag indicating whether the block is "consistent" or "inconsistent".
#[derive(Debug, Clone, Copy)]
pub(crate) struct BeginToken {
    pub offset: isize,
    pub breaks: Break,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_begin_token_debug() {
        let token = BeginToken {
            offset: 4,
            breaks: Break::Consistent,
        };
        assert_eq!(
            format!("{:?}", token),
            "BeginToken { offset: 4, breaks: Consistent }",
        );
    }
}
