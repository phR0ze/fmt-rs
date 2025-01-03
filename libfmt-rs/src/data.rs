use crate::engine::Engine;
use crate::iter::IterDelimited;
use crate::path::PathKind;
use syn::{Field, Fields, FieldsUnnamed, Variant, VisRestricted, Visibility};

impl Engine {
    pub fn variant(&mut self, variant: &Variant) {
        self.outer_attrs(&variant.attrs);
        self.ident(&variant.ident);
        match &variant.fields {
            Fields::Named(fields) => {
                self.scan_space();
                self.scan_string("{");
                self.scan_begin_vertical(self.config.indent);
                self.scan_break_space();
                for field in fields.named.iter().delimited() {
                    self.scan_field(&field);
                    self.trailing_comma_or_space(field.is_last);
                }
                self.update_break_offset(-self.config.indent);
                self.scan_end();
                self.scan_string("}");
            }
            Fields::Unnamed(fields) => {
                self.scan_begin_vertical(self.config.indent);
                self.fields_unnamed(fields);
                self.scan_end();
            }
            Fields::Unit => {}
        }
        if let Some((_eq_token, discriminant)) = &variant.discriminant {
            self.scan_string(" = ");
            self.expr(discriminant);
        }
    }

    pub fn fields_unnamed(&mut self, fields: &FieldsUnnamed) {
        self.scan_string("(");
        self.scan_break_zero();
        for field in fields.unnamed.iter().delimited() {
            self.scan_field(&field);
            self.trailing_comma(field.is_last);
        }
        self.update_break_offset(-self.config.indent);
        self.scan_string(")");
    }

    /// Scan a field
    pub fn scan_field(&mut self, field: &Field) {
        self.outer_attrs(&field.attrs);
        self.visibility(&field.vis);
        if let Some(ident) = &field.ident {
            self.ident(ident);
            self.scan_string(": ");
        }
        self.ty(&field.ty);
    }

    pub fn visibility(&mut self, vis: &Visibility) {
        match vis {
            Visibility::Public(_) => self.scan_string("pub "),
            Visibility::Restricted(vis) => self.vis_restricted(vis),
            Visibility::Inherited => {}
        }
    }

    fn vis_restricted(&mut self, vis: &VisRestricted) {
        self.scan_string("pub(");
        let omit_in = vis.path.get_ident().map_or(false, |ident| {
            matches!(ident.to_string().as_str(), "self" | "super" | "crate")
        });
        if !omit_in {
            self.scan_string("in ");
        }
        self.scan_path(&vis.path, PathKind::Simple);
        self.scan_string(") ");
    }
}
