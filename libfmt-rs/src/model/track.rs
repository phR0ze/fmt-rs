pub(crate) struct Track {
    pub(crate) elses: bool,
    pub(crate) comment: bool,
}

impl Default for Track {
    fn default() -> Self {
        Self {
            elses: false,
            comment: false,
        }
    }
}
