use crate::engine::Engine;
use crate::iter::IterDelimited;
use crate::path::PathKind;
use proc_macro2::TokenStream;
use syn::{
    Abi, BareFnArg, BareVariadic, ReturnType, Type, TypeArray, TypeBareFn, TypeGroup,
    TypeImplTrait, TypeInfer, TypeMacro, TypeNever, TypeParen, TypePath, TypePtr, TypeReference,
    TypeSlice, TypeTraitObject, TypeTuple,
};

impl Engine {
    /// Type
    pub fn ty(&mut self, ty: &Type) {
        match ty {
            #![cfg_attr(all(test, exhaustive), deny(non_exhaustive_omitted_patterns))]
            Type::Array(ty) => self.type_array(ty),
            Type::BareFn(ty) => self.type_bare_fn(ty),
            Type::Group(ty) => self.type_group(ty),
            Type::ImplTrait(ty) => self.type_impl_trait(ty),
            Type::Infer(ty) => self.type_infer(ty),
            Type::Macro(ty) => self.type_macro(ty),
            Type::Never(ty) => self.type_never(ty),
            Type::Paren(ty) => self.type_paren(ty),
            Type::Path(ty) => self.type_path(ty),
            Type::Ptr(ty) => self.type_ptr(ty),
            Type::Reference(ty) => self.type_reference(ty),
            Type::Slice(ty) => self.type_slice(ty),
            Type::TraitObject(ty) => self.type_trait_object(ty),
            Type::Tuple(ty) => self.type_tuple(ty),
            Type::Verbatim(ty) => self.type_verbatim(ty),
            _ => unimplemented!("unknown Type"),
        }
    }

    fn type_array(&mut self, ty: &TypeArray) {
        self.scan_string("[");
        self.ty(&ty.elem);
        self.scan_string("; ");
        self.expr(&ty.len);
        self.scan_string("]");
    }

    fn type_bare_fn(&mut self, ty: &TypeBareFn) {
        if let Some(bound_lifetimes) = &ty.lifetimes {
            self.bound_lifetimes(bound_lifetimes);
        }
        if ty.unsafety.is_some() {
            self.scan_string("unsafe ");
        }
        if let Some(abi) = &ty.abi {
            self.abi(abi);
        }
        self.scan_string("fn(");
        self.scan_begin_vertical(self.config.indent);
        self.scan_break_zero();
        for bare_fn_arg in ty.inputs.iter().delimited() {
            self.bare_fn_arg(&bare_fn_arg);
            self.trailing_comma(bare_fn_arg.is_last && ty.variadic.is_none());
        }
        if let Some(variadic) = &ty.variadic {
            self.bare_variadic(variadic);
            self.scan_break_zero();
        }
        self.update_break_offset(-self.config.indent);
        self.scan_end();
        self.scan_string(")");
        self.return_type(&ty.output);
    }

    fn type_group(&mut self, ty: &TypeGroup) {
        self.ty(&ty.elem);
    }

    fn type_impl_trait(&mut self, ty: &TypeImplTrait) {
        self.scan_string("impl ");
        for type_param_bound in ty.bounds.iter().delimited() {
            if !type_param_bound.is_first {
                self.scan_string(" + ");
            }
            self.type_param_bound(&type_param_bound);
        }
    }

    fn type_infer(&mut self, ty: &TypeInfer) {
        let _ = ty;
        self.scan_string("_");
    }

    fn type_macro(&mut self, ty: &TypeMacro) {
        let semicolon = false;
        self.scan_mac(&ty.mac, None, semicolon);
    }

    fn type_never(&mut self, ty: &TypeNever) {
        let _ = ty;
        self.scan_string("!");
    }

    fn type_paren(&mut self, ty: &TypeParen) {
        self.scan_string("(");
        self.ty(&ty.elem);
        self.scan_string(")");
    }

    fn type_path(&mut self, ty: &TypePath) {
        self.qpath(&ty.qself, &ty.path, PathKind::Type);
    }

    fn type_ptr(&mut self, ty: &TypePtr) {
        self.scan_string("*");
        if ty.mutability.is_some() {
            self.scan_string("mut ");
        } else {
            self.scan_string("const ");
        }
        self.ty(&ty.elem);
    }

    fn type_reference(&mut self, ty: &TypeReference) {
        self.scan_string("&");
        if let Some(lifetime) = &ty.lifetime {
            self.lifetime(lifetime);
            self.scan_space();
        }
        if ty.mutability.is_some() {
            self.scan_string("mut ");
        }
        self.ty(&ty.elem);
    }

    fn type_slice(&mut self, ty: &TypeSlice) {
        self.scan_string("[");
        self.ty(&ty.elem);
        self.scan_string("]");
    }

    fn type_trait_object(&mut self, ty: &TypeTraitObject) {
        self.scan_string("dyn ");
        for type_param_bound in ty.bounds.iter().delimited() {
            if !type_param_bound.is_first {
                self.scan_string(" + ");
            }
            self.type_param_bound(&type_param_bound);
        }
    }

    fn type_tuple(&mut self, ty: &TypeTuple) {
        self.scan_string("(");
        self.scan_begin_vertical(self.config.indent);
        self.scan_break_zero();
        for elem in ty.elems.iter().delimited() {
            self.ty(&elem);
            if ty.elems.len() == 1 {
                self.scan_string(",");
                self.scan_break_zero();
            } else {
                self.trailing_comma(elem.is_last);
            }
        }
        self.update_break_offset(-self.config.indent);
        self.scan_end();
        self.scan_string(")");
    }

    fn type_verbatim(&mut self, ty: &TokenStream) {
        unimplemented!("Type::Verbatim `{}`", ty);
    }

    pub fn return_type(&mut self, ty: &ReturnType) {
        match ty {
            ReturnType::Default => {}
            ReturnType::Type(_arrow, ty) => {
                self.scan_string(" -> ");
                self.ty(ty);
            }
        }
    }

    fn bare_fn_arg(&mut self, bare_fn_arg: &BareFnArg) {
        self.outer_attrs(&bare_fn_arg.attrs);
        if let Some((name, _colon)) = &bare_fn_arg.name {
            self.ident(name);
            self.scan_string(": ");
        }
        self.ty(&bare_fn_arg.ty);
    }

    fn bare_variadic(&mut self, variadic: &BareVariadic) {
        self.outer_attrs(&variadic.attrs);
        if let Some((name, _colon)) = &variadic.name {
            self.ident(name);
            self.scan_string(": ");
        }
        self.scan_string("...");
    }

    pub fn abi(&mut self, abi: &Abi) {
        self.scan_string("extern ");
        if let Some(name) = &abi.name {
            self.lit_str(name);
            self.scan_space();
        }
    }
}
