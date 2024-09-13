use crate::engine::Engine;
use syn::Lifetime;

impl Engine {
    pub fn lifetime(&mut self, lifetime: &Lifetime) {
        self.scan_string("'");
        self.ident(&lifetime.ident);
    }
}
