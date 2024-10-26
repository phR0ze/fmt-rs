// Experimenting with better error handling
// -------------------------------------------------------------------------------------------------

// Tier 2 error type
// -------------------------------------------------------------------------------------------------
#[derive(Debug)]
pub struct SecondLevelError {
    // Send and Sync are required to be compatible with anyhow
    source: Option<Box<dyn std::error::Error + Send + Sync>>,
}
impl From<std::io::Error> for SecondLevelError {
    fn from(source: std::io::Error) -> Self {
        Self {
            source: Some(Box::new(source)),
        }
    }
}
impl std::fmt::Display for SecondLevelError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "Second level error")
    }
}
impl std::error::Error for SecondLevelError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        self.source
            .as_ref()
            // Need to cast to a lessor trait to satisfy the return type
            .map(|x| x.as_ref() as &(dyn std::error::Error + 'static))
    }
}

// Tier 3 error type
// -------------------------------------------------------------------------------------------------
#[derive(Debug)]
pub struct ThirdLevelError {
    source: SecondLevelError,
}
impl From<SecondLevelError> for ThirdLevelError {
    fn from(source: SecondLevelError) -> Self {
        Self { source }
    }
}
impl std::fmt::Display for ThirdLevelError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "Third level error")
    }
}
impl std::error::Error for ThirdLevelError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        Some(&self.source)
    }
}

// Tier 1 error handling
// -------------------------------------------------------------------------------------------------
fn first_level_error() -> Result<(), std::io::Error> {
    let e = std::io::Error::new(std::io::ErrorKind::Other, "First level error");
    Err(e)
}

// Tier 2 error handling
// -------------------------------------------------------------------------------------------------
fn second_level_error() -> Result<(), SecondLevelError> {
    first_level_error()?;
    Ok(())
}

// Tier 3 error handling
// -------------------------------------------------------------------------------------------------
fn third_level_error() -> Result<(), ThirdLevelError> {
    second_level_error()?;
    Ok(())
}

// Tier 3 error handling
// -------------------------------------------------------------------------------------------------
fn main() -> anyhow::Result<()> {
    std::env::set_var("RUST_LIB_BACKTRACE", "0");
    third_level_error().map_err(|x| anyhow::Error::new(x))?;
    Ok(())
}
