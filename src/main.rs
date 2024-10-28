mod utils;
use anyhow::{Context, Result};
use libfmt_rs;
use utils::ColorExt;

fn main() -> Result<()> {
    let args: Vec<String> = std::env::args().skip(1).collect();
    if args.is_empty() || args[0] == "-h" || args[0] == "--help" {
        help()
    }

    // Read and parse the input file
    let path = &args[args.len() - 1];
    let src = std::fs::read_to_string(path).with_context(|| "failed to read source file")?;
    let result = libfmt_rs::format_str(None, &src).map_err(|e| anyhow::Error::new(e))?;

    // Print diff of changes
    if args[0] == "-d" || args[0] == "--diff" {
        let diff = similar::TextDiff::from_lines(&src, &result);
        for change in diff.iter_all_changes() {
            match change.tag() {
                similar::ChangeTag::Delete => print!("{}{}", "-".red(), change.red()),
                similar::ChangeTag::Insert => print!("{}{}", "+".green(), change.green()),
                similar::ChangeTag::Equal => print!("{}{}", " ", change),
            }
        }

    // Print results to screen
    } else {
        print!("{}", result);
    }

    Ok(())
}

fn help() {
    println!("Usage: fmt-rs <path>");
    println!("Format the given file using the libfmt-rs library.");
    println!();
    println!("Options:");
    println!("  -h, --help    Display this help message");
    println!("  -d, --diff    Output the diff of the changes");
    std::process::exit(1);
}
