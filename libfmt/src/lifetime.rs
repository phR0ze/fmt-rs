use crate::algorithm::Engine;
use syn::Lifetime;

impl Engine {
    pub fn lifetime(&mut self, lifetime: &Lifetime) {
        self.word("'");
        self.ident(&lifetime.ident);
    }
}
