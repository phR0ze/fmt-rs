use libfmt-rs::Result;
use tracing::Level;
use tracing_subscriber::FmtSubscriber;

fn main() -> Result<()> {
    let subscriber = FmtSubscriber::builder()
        .with_max_level(Level::DEBUG)
        .finish();
    tracing::subscriber::set_global_default(subscriber).unwrap();

    // Pass in an example
    let path = "examples/dump.rs";
    let formatted = libfmt-rs::format_file(path)?;
    println!("{}", formatted);

    Ok(())
}
