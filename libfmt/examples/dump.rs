use libfmt::Result;
use quote::quote;
use tracing::Level;
use tracing_subscriber::FmtSubscriber;

fn main() -> Result<()> {
    let subscriber = FmtSubscriber::builder()
        .with_max_level(Level::TRACE)
        .finish();
    tracing::subscriber::set_global_default(subscriber).unwrap();

    // Pass in an example
    let tokens = quote! {
        // foo
        println!("{}", "1",);
    };
    let ast: syn::File = syn::parse2(tokens).unwrap();
    libfmt::format_syn_file(&ast)?;

    Ok(())
}
