use indoc::indoc;
use libfmt::Result;
use tracing::Level;
use tracing_subscriber::FmtSubscriber;

fn main() -> Result<()> {
    let subscriber = FmtSubscriber::builder()
        .with_max_level(Level::TRACE)
        .finish();
    tracing::subscriber::set_global_default(subscriber).unwrap();

    // Pass in an example
    let source = indoc! {r#"
        // Comment
        println!("{}", "1");
    "#};
    libfmt::format_str(source)?;

    Ok(())
}
