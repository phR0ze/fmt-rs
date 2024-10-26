use anyhow::Result;
use libfmt_rs;

fn main() -> Result<()> {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <path>", args[0]);
        std::process::exit(1);
    }

    // Read and parse the input file
    println!("Processing: {}", args[1]);
    let result = libfmt_rs::format_file(None, &args[1]).map_err(|e| anyhow::Error::new(e))?;

    // Print results to screen
    print!("{}", result);

    Ok(())
}
