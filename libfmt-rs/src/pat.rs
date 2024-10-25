use crate::engine::Engine;
use crate::iter::IterDelimited;
use crate::path::PathKind;
use proc_macro2::TokenStream;
use syn::{
    FieldPat, Pat, PatIdent, PatOr, PatParen, PatReference, PatRest, PatSlice, PatStruct, PatTuple,
    PatTupleStruct, PatType, PatWild,
};

impl Engine {
    pub fn pat(&mut self, pat: &Pat) {
        match pat {
            #![cfg_attr(all(test, exhaustive), deny(non_exhaustive_omitted_patterns))]
            Pat::Const(pat) => self.expr_const(pat),
            Pat::Ident(pat) => self.pat_ident(pat),
            Pat::Lit(pat) => self.expr_lit(pat),
            Pat::Macro(pat) => self.expr_macro(pat),
            Pat::Or(pat) => self.pat_or(pat),
            Pat::Paren(pat) => self.pat_paren(pat),
            Pat::Path(pat) => self.expr_path(pat),
            Pat::Range(pat) => self.expr_range(pat),
            Pat::Reference(pat) => self.pat_reference(pat),
            Pat::Rest(pat) => self.pat_rest(pat),
            Pat::Slice(pat) => self.pat_slice(pat),
            Pat::Struct(pat) => self.pat_struct(pat),
            Pat::Tuple(pat) => self.pat_tuple(pat),
            Pat::TupleStruct(pat) => self.pat_tuple_struct(pat),
            Pat::Type(pat) => self.pat_type(pat),
            Pat::Verbatim(pat) => self.pat_verbatim(pat),
            Pat::Wild(pat) => self.pat_wild(pat),
            _ => unimplemented!("unknown Pat"),
        }
    }

    fn pat_ident(&mut self, pat: &PatIdent) {
        self.outer_attrs(&pat.attrs);
        if pat.by_ref.is_some() {
            self.scan_string("ref ");
        }
        if pat.mutability.is_some() {
            self.scan_string("mut ");
        }
        self.ident(&pat.ident);
        if let Some((_at_token, subpat)) = &pat.subpat {
            self.scan_string(" @ ");
            self.pat(subpat);
        }
    }

    fn pat_or(&mut self, pat: &PatOr) {
        self.outer_attrs(&pat.attrs);
        let mut vertical_flow = false;
        for case in &pat.cases {
            match case {
                Pat::Lit(_) | Pat::Wild(_) => {}
                _ => {
                    vertical_flow = true;
                    break;
                }
            }
        }
        if vertical_flow {
            self.scan_begin_vertical(0);
        } else {
            self.scan_begin_horizontal(0);
        }
        for case in pat.cases.iter().delimited() {
            if !case.is_first {
                self.scan_break_space();
                self.scan_string("| ");
            }
            self.pat(&case);
        }
        self.scan_end();
    }

    fn pat_paren(&mut self, pat: &PatParen) {
        self.outer_attrs(&pat.attrs);
        self.scan_string("(");
        self.pat(&pat.pat);
        self.scan_string(")");
    }

    fn pat_reference(&mut self, pat: &PatReference) {
        self.outer_attrs(&pat.attrs);
        self.scan_string("&");
        if pat.mutability.is_some() {
            self.scan_string("mut ");
        }
        self.pat(&pat.pat);
    }

    fn pat_rest(&mut self, pat: &PatRest) {
        self.outer_attrs(&pat.attrs);
        self.scan_string("..");
    }

    fn pat_slice(&mut self, pat: &PatSlice) {
        self.outer_attrs(&pat.attrs);
        self.scan_string("[");
        for elem in pat.elems.iter().delimited() {
            self.pat(&elem);
            self.trailing_comma(elem.is_last);
        }
        self.scan_string("]");
    }

    fn pat_struct(&mut self, pat: &PatStruct) {
        self.outer_attrs(&pat.attrs);
        self.scan_begin_vertical(self.config.indent);
        self.scan_path(&pat.path, PathKind::Expr);
        self.scan_string(" {");
        self.scan_break_space_if_nonempty();
        for field in pat.fields.iter().delimited() {
            self.field_pat(&field);
            self.trailing_comma_or_space(field.is_last && pat.rest.is_none());
        }
        if let Some(rest) = &pat.rest {
            self.pat_rest(rest);
            self.scan_break_space();
        }
        self.update_break_offset(-self.config.indent);
        self.scan_end();
        self.scan_string("}");
    }

    fn pat_tuple(&mut self, pat: &PatTuple) {
        self.outer_attrs(&pat.attrs);
        self.scan_string("(");
        self.scan_begin_vertical(self.config.indent);
        self.scan_break_zero();
        for elem in pat.elems.iter().delimited() {
            self.pat(&elem);
            if pat.elems.len() == 1 {
                if pat.elems.trailing_punct() {
                    self.scan_string(",");
                }
                self.scan_break_zero();
            } else {
                self.trailing_comma(elem.is_last);
            }
        }
        self.update_break_offset(-self.config.indent);
        self.scan_end();
        self.scan_string(")");
    }

    fn pat_tuple_struct(&mut self, pat: &PatTupleStruct) {
        self.outer_attrs(&pat.attrs);
        self.scan_path(&pat.path, PathKind::Expr);
        self.scan_string("(");
        self.scan_begin_vertical(self.config.indent);
        self.scan_break_zero();
        for elem in pat.elems.iter().delimited() {
            self.pat(&elem);
            self.trailing_comma(elem.is_last);
        }
        self.update_break_offset(-self.config.indent);
        self.scan_end();
        self.scan_string(")");
    }

    pub fn pat_type(&mut self, pat: &PatType) {
        self.outer_attrs(&pat.attrs);
        self.pat(&pat.pat);
        self.scan_string(": ");
        self.ty(&pat.ty);
    }

    #[cfg(not(feature = "verbatim"))]
    fn pat_verbatim(&mut self, pat: &TokenStream) {
        unimplemented!("Pat::Verbatim `{}`", pat);
    }

    fn pat_wild(&mut self, pat: &PatWild) {
        self.outer_attrs(&pat.attrs);
        self.scan_string("_");
    }

    fn field_pat(&mut self, field_pat: &FieldPat) {
        self.outer_attrs(&field_pat.attrs);
        if field_pat.colon_token.is_some() {
            self.member(&field_pat.member);
            self.scan_string(": ");
        }
        self.pat(&field_pat.pat);
    }
}
