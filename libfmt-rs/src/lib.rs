pub(crate) mod attrs;
mod convenience;
mod data;
mod engine;
mod expr;
mod f0000_drop_trailing_comma;
mod f0001_developer_comments;
mod f0002_smart_wrapping;
mod f0003_drop_ampersand_trailing_space;
mod generics;
mod item;
mod iter;
mod lifetime;
mod lit;
mod r#macro;
mod model;
mod pat;
mod path;
mod stmt;
mod token;
mod ty;

pub(crate) use engine::Engine;
use model::Config;
use std::path::Path;
use tracing::trace;

// Re-export the public API
pub use model::error::{Error, Result};

/// Format the given source file
pub fn format_file<T: AsRef<Path>>(config: Option<Config>, path: T) -> Result<String> {
    let path = path.as_ref();
    let source = std::fs::read_to_string(path)
        .map_err(|e| Error::new("failed to read source file").wrap_io(e))?;
    format_str(config, &source)
}

/// Format the given source string
pub fn format_str(config: Option<Config>, source: &str) -> Result<String> {
    let mut engine = Engine::new(source, config.unwrap_or_default());
    engine.format()?;
    Ok(engine.print())
}

impl Engine {
    pub(crate) fn format(&mut self) -> Result<()> {
        trace!("Formatting");

        // Pre-process the token stream for comment locational information
        let tokens = f0001_developer_comments::inject(&self.config, &self.src)?;

        // Parse the syntax tree from the token stream
        let ast: syn::File = syn::parse2(tokens)
            .map_err(|e| Error::new("failed to parse token stream into syntax tree").wrap_syn(e))?;
        self.scan_begin_vertical(0);

        // Check for any inner attributes for the file
        self.inner_attrs(&ast.attrs);
        for item in &ast.items {
            self.item(item);
        }
        self.scan_end();

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;
    use tracing_test::traced_test;
}
