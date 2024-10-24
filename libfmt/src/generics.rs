use crate::engine::Engine;
use crate::iter::IterDelimited;
use crate::path::PathKind;
use proc_macro2::TokenStream;
use std::ptr;
use syn::{
    BoundLifetimes, ConstParam, GenericParam, Generics, LifetimeParam, PredicateLifetime,
    PredicateType, TraitBound, TraitBoundModifier, TypeParam, TypeParamBound, WhereClause,
    WherePredicate,
};

impl Engine {
    pub fn generics(&mut self, generics: &Generics) {
        if generics.params.is_empty() {
            return;
        }

        self.scan_string("<");
        self.scan_begin_vertical(0);
        self.scan_break_zero();

        // Print lifetimes before types and consts, regardless of their
        // order in self.params.
        #[derive(Ord, PartialOrd, Eq, PartialEq)]
        enum Group {
            First,
            Second,
        }
        fn group(param: &GenericParam) -> Group {
            match param {
                GenericParam::Lifetime(_) => Group::First,
                GenericParam::Type(_) | GenericParam::Const(_) => Group::Second,
            }
        }
        let last = generics.params.iter().max_by_key(|param| group(param));
        for current_group in [Group::First, Group::Second] {
            for param in &generics.params {
                if group(param) == current_group {
                    self.generic_param(param);
                    self.trailing_comma(ptr::eq(param, last.unwrap()));
                }
            }
        }

        self.offset(-self.config.indent);
        self.scan_end();
        self.scan_string(">");
    }

    fn generic_param(&mut self, generic_param: &GenericParam) {
        match generic_param {
            GenericParam::Type(type_param) => self.type_param(type_param),
            GenericParam::Lifetime(lifetime_param) => self.lifetime_param(lifetime_param),
            GenericParam::Const(const_param) => self.const_param(const_param),
        }
    }

    pub fn bound_lifetimes(&mut self, bound_lifetimes: &BoundLifetimes) {
        self.scan_string("for<");
        for param in bound_lifetimes.lifetimes.iter().delimited() {
            self.generic_param(&param);
            if !param.is_last {
                self.scan_string(", ");
            }
        }
        self.scan_string("> ");
    }

    fn lifetime_param(&mut self, lifetime_param: &LifetimeParam) {
        self.outer_attrs(&lifetime_param.attrs);
        self.lifetime(&lifetime_param.lifetime);
        for lifetime in lifetime_param.bounds.iter().delimited() {
            if lifetime.is_first {
                self.scan_string(": ");
            } else {
                self.scan_string(" + ");
            }
            self.lifetime(&lifetime);
        }
    }

    fn type_param(&mut self, type_param: &TypeParam) {
        self.outer_attrs(&type_param.attrs);
        self.ident(&type_param.ident);
        self.scan_begin_horizontal(self.config.indent);
        for type_param_bound in type_param.bounds.iter().delimited() {
            if type_param_bound.is_first {
                self.scan_string(": ");
            } else {
                self.scan_break_space();
                self.scan_string("+ ");
            }
            self.type_param_bound(&type_param_bound);
        }
        if let Some(default) = &type_param.default {
            self.scan_break_space();
            self.scan_string("= ");
            self.ty(default);
        }
        self.scan_end();
    }

    pub fn type_param_bound(&mut self, type_param_bound: &TypeParamBound) {
        match type_param_bound {
            #![cfg_attr(all(test, exhaustive), deny(non_exhaustive_omitted_patterns))]
            TypeParamBound::Trait(trait_bound) => {
                let tilde_const = false;
                self.trait_bound(trait_bound, tilde_const);
            }
            TypeParamBound::Lifetime(lifetime) => self.lifetime(lifetime),
            TypeParamBound::Verbatim(bound) => self.type_param_bound_verbatim(bound),
            _ => unimplemented!("unknown TypeParamBound"),
        }
    }

    fn trait_bound(&mut self, trait_bound: &TraitBound, tilde_const: bool) {
        if trait_bound.paren_token.is_some() {
            self.scan_string("(");
        }
        if tilde_const {
            self.scan_string("~const ");
        }
        self.trait_bound_modifier(&trait_bound.modifier);
        if let Some(bound_lifetimes) = &trait_bound.lifetimes {
            self.bound_lifetimes(bound_lifetimes);
        }
        for segment in trait_bound.path.segments.iter().delimited() {
            if !segment.is_first || trait_bound.path.leading_colon.is_some() {
                self.scan_string("::");
            }
            self.path_segment(&segment, PathKind::Type);
        }
        if trait_bound.paren_token.is_some() {
            self.scan_string(")");
        }
    }

    fn trait_bound_modifier(&mut self, trait_bound_modifier: &TraitBoundModifier) {
        match trait_bound_modifier {
            TraitBoundModifier::None => {}
            TraitBoundModifier::Maybe(_question_mark) => self.scan_string("?"),
        }
    }

    #[cfg(not(feature = "verbatim"))]
    fn type_param_bound_verbatim(&mut self, bound: &TokenStream) {
        unimplemented!("TypeParamBound::Verbatim `{}`", bound);
    }

    #[cfg(feature = "verbatim")]
    fn type_param_bound_verbatim(&mut self, tokens: &TokenStream) {
        use syn::parse::{Parse, ParseStream, Result};
        use syn::{parenthesized, token, Ident, Lifetime, Token};

        enum TypeParamBoundVerbatim {
            Ellipsis,
            PreciseCapture(Vec<Capture>),
            TildeConst(TraitBound),
        }

        enum Capture {
            Lifetime(Lifetime),
            Type(Ident),
        }

        impl Parse for TypeParamBoundVerbatim {
            fn parse(input: ParseStream) -> Result<Self> {
                let content;
                let (paren_token, content) = if input.peek(token::Paren) {
                    (Some(parenthesized!(content in input)), &content)
                } else {
                    (None, input)
                };
                let lookahead = content.lookahead1();
                if lookahead.peek(Token![use]) {
                    input.parse::<Token![use]>()?;
                    input.parse::<Token![<]>()?;
                    let mut captures = Vec::new();
                    loop {
                        let lookahead = input.lookahead1();
                        captures.push(if lookahead.peek(Lifetime) {
                            input.parse().map(Capture::Lifetime)?
                        } else if lookahead.peek(Ident) {
                            input.parse().map(Capture::Type)?
                        } else if lookahead.peek(Token![>]) {
                            break;
                        } else {
                            return Err(lookahead.error());
                        });
                        let lookahead = input.lookahead1();
                        if lookahead.peek(Token![,]) {
                            input.parse::<Token![,]>()?;
                        } else if lookahead.peek(Token![>]) {
                            break;
                        } else {
                            return Err(lookahead.error());
                        }
                    }
                    input.parse::<Token![>]>()?;
                    Ok(TypeParamBoundVerbatim::PreciseCapture(captures))
                } else if lookahead.peek(Token![~]) {
                    content.parse::<Token![~]>()?;
                    content.parse::<Token![const]>()?;
                    let mut bound: TraitBound = content.parse()?;
                    bound.paren_token = paren_token;
                    Ok(TypeParamBoundVerbatim::TildeConst(bound))
                } else if lookahead.peek(Token![...]) {
                    content.parse::<Token![...]>()?;
                    Ok(TypeParamBoundVerbatim::Ellipsis)
                } else {
                    Err(lookahead.error())
                }
            }
        }

        let bound: TypeParamBoundVerbatim = match syn::parse2(tokens.clone()) {
            Ok(bound) => bound,
            Err(_) => unimplemented!("TypeParamBound::Verbatim `{}`", tokens),
        };

        match bound {
            TypeParamBoundVerbatim::Ellipsis => {
                self.scan_string("...");
            }
            TypeParamBoundVerbatim::PreciseCapture(captures) => {
                self.scan_string("use<");
                for capture in captures.iter().delimited() {
                    match *capture {
                        Capture::Lifetime(lifetime) => self.lifetime(lifetime),
                        Capture::Type(ident) => self.ident(ident),
                    }
                    if !capture.is_last {
                        self.scan_string(", ");
                    }
                }
                self.scan_string(">");
            }
            TypeParamBoundVerbatim::TildeConst(trait_bound) => {
                let tilde_const = true;
                self.trait_bound(&trait_bound, tilde_const);
            }
        }
    }

    fn const_param(&mut self, const_param: &ConstParam) {
        self.outer_attrs(&const_param.attrs);
        self.scan_string("const ");
        self.ident(&const_param.ident);
        self.scan_string(": ");
        self.ty(&const_param.ty);
        if let Some(default) = &const_param.default {
            self.scan_string(" = ");
            self.expr(default);
        }
    }

    pub fn where_clause_for_body(&mut self, where_clause: &Option<WhereClause>) {
        let hardbreaks = true;
        let semi = false;
        self.where_clause_impl(where_clause, hardbreaks, semi);
    }

    pub fn where_clause_semi(&mut self, where_clause: &Option<WhereClause>) {
        let hardbreaks = true;
        let semi = true;
        self.where_clause_impl(where_clause, hardbreaks, semi);
    }

    pub fn where_clause_oneline(&mut self, where_clause: &Option<WhereClause>) {
        let hardbreaks = false;
        let semi = false;
        self.where_clause_impl(where_clause, hardbreaks, semi);
    }

    pub fn where_clause_oneline_semi(&mut self, where_clause: &Option<WhereClause>) {
        let hardbreaks = false;
        let semi = true;
        self.where_clause_impl(where_clause, hardbreaks, semi);
    }

    fn where_clause_impl(
        &mut self,
        where_clause: &Option<WhereClause>,
        hardbreaks: bool,
        semi: bool,
    ) {
        let where_clause = match where_clause {
            Some(where_clause) if !where_clause.predicates.is_empty() => where_clause,
            _ => {
                if semi {
                    self.scan_string(";");
                } else {
                    self.scan_space();
                }
                return;
            }
        };
        if hardbreaks {
            self.scan_break_newline();
            self.offset(-self.config.indent);
            self.scan_string("where");
            self.scan_break_newline();
            for predicate in where_clause.predicates.iter().delimited() {
                self.where_predicate(&predicate);
                if predicate.is_last && semi {
                    self.scan_string(";");
                } else {
                    self.scan_string(",");
                    self.scan_break_newline();
                }
            }
            if !semi {
                self.offset(-self.config.indent);
            }
        } else {
            self.scan_break_space();
            self.offset(-self.config.indent);
            self.scan_string("where");
            self.scan_break_space();
            for predicate in where_clause.predicates.iter().delimited() {
                self.where_predicate(&predicate);
                if predicate.is_last && semi {
                    self.scan_string(";");
                } else {
                    self.trailing_comma_or_space(predicate.is_last);
                }
            }
            if !semi {
                self.offset(-self.config.indent);
            }
        }
    }

    fn where_predicate(&mut self, predicate: &WherePredicate) {
        match predicate {
            #![cfg_attr(all(test, exhaustive), deny(non_exhaustive_omitted_patterns))]
            WherePredicate::Type(predicate) => self.predicate_type(predicate),
            WherePredicate::Lifetime(predicate) => self.predicate_lifetime(predicate),
            _ => unimplemented!("unknown WherePredicate"),
        }
    }

    fn predicate_type(&mut self, predicate: &PredicateType) {
        if let Some(bound_lifetimes) = &predicate.lifetimes {
            self.bound_lifetimes(bound_lifetimes);
        }
        self.ty(&predicate.bounded_ty);
        self.scan_string(":");
        if predicate.bounds.len() == 1 {
            self.scan_begin_horizontal(0);
        } else {
            self.scan_begin_horizontal(self.config.indent);
        }
        for type_param_bound in predicate.bounds.iter().delimited() {
            if type_param_bound.is_first {
                self.scan_space();
            } else {
                self.scan_break_space();
                self.scan_string("+ ");
            }
            self.type_param_bound(&type_param_bound);
        }
        self.scan_end();
    }

    fn predicate_lifetime(&mut self, predicate: &PredicateLifetime) {
        self.lifetime(&predicate.lifetime);
        self.scan_string(":");
        self.scan_begin_horizontal(self.config.indent);
        for lifetime in predicate.bounds.iter().delimited() {
            if lifetime.is_first {
                self.scan_space();
            } else {
                self.scan_break_space();
                self.scan_string("+ ");
            }
            self.lifetime(&lifetime);
        }
        self.scan_end();
    }
}
