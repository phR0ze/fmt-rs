use crate::engine::Engine;
use crate::iter::IterDelimited;
use crate::path::PathKind;
use crate::INDENT;
use syn::{Field, Fields, FieldsUnnamed, Variant, VisRestricted, Visibility};

impl Engine {
    pub fn variant(&mut self, variant: &Variant) {
        self.outer_attrs(&variant.attrs);
        self.ident(&variant.ident);
        match &variant.fields {
            Fields::Named(fields) => {
                self.nbsp();
                self.scan_string("{");
                self.scan_begin_consistent(INDENT);
                self.space();
                for field in fields.named.iter().delimited() {
                    self.field(&field);
                    self.trailing_comma_or_space(field.is_last);
                }
                self.offset(-INDENT);
                self.scan_end();
                self.scan_string("}");
            }
            Fields::Unnamed(fields) => {
                self.scan_begin_consistent(INDENT);
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
        self.zerobreak();
        for field in fields.unnamed.iter().delimited() {
            self.field(&field);
            self.trailing_comma(field.is_last);
        }
        self.offset(-INDENT);
        self.scan_string(")");
    }

    pub fn field(&mut self, field: &Field) {
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
        self.path(&vis.path, PathKind::Simple);
        self.scan_string(") ");
    }
}
