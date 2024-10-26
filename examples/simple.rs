use anyhow::Result;
use std::path::Path;

fn main() -> Result<()> {
    // Handy way to ensure I'm getting the right path
    let src = Path::new(env!("CARGO_MANIFEST_DIR")).join("src/main.rs");

    // Read and parse the input file
    println!("Processing: {:?}", src);
    let result = libfmt_rs::format_file(None, &src).map_err(|e| anyhow::Error::new(e))?;

    // Print results to screen
    print!("{}", result);

    Ok(())
}
