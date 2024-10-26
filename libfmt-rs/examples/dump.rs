use std::path::Path;

use anyhow::Result;
use tracing::Level;
use tracing_subscriber::FmtSubscriber;

fn main() -> Result<()> {
    let subscriber = FmtSubscriber::builder()
        .with_max_level(Level::DEBUG)
        .finish();
    tracing::subscriber::set_global_default(subscriber).unwrap();

    // Pass in an example
    let path = Path::new(env!("CARGO_MANIFEST_DIR")).join("examples/dump.rs");
    let formatted = libfmt_rs::format_file(None, path).map_err(|e| anyhow::anyhow!(e))?;
    println!("{}", formatted);

    Ok(())
}
