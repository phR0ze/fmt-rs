use crate::iter::IterDelimited;
use crate::path::PathKind;
use crate::{engine::Engine, model::BreakToken};
use proc_macro2::TokenStream;
use syn::{
    Fields, FnArg, ForeignItem, ForeignItemFn, ForeignItemMacro, ForeignItemStatic,
    ForeignItemType, ImplItem, ImplItemConst, ImplItemFn, ImplItemMacro, ImplItemType, Item,
    ItemConst, ItemEnum, ItemExternCrate, ItemFn, ItemForeignMod, ItemImpl, ItemMacro, ItemMod,
    ItemStatic, ItemStruct, ItemTrait, ItemTraitAlias, ItemType, ItemUnion, ItemUse, Receiver,
    Signature, StaticMutability, TraitItem, TraitItemConst, TraitItemFn, TraitItemMacro,
    TraitItemType, Type, UseGlob, UseGroup, UseName, UsePath, UseRename, UseTree, Variadic,
};

impl Engine {
    pub fn item(&mut self, item: &Item) {
        match item {
            #![cfg_attr(all(test, exhaustive), deny(non_exhaustive_omitted_patterns))]
            Item::Const(item) => self.item_const(item),
            Item::Enum(item) => self.item_enum(item),
            Item::ExternCrate(item) => self.item_extern_crate(item),
            Item::Fn(item) => self.item_fn(item),
            Item::ForeignMod(item) => self.item_foreign_mod(item),
            Item::Impl(item) => self.item_impl(item),
            Item::Macro(item) => self.item_macro(item),
            Item::Mod(item) => self.item_mod(item),
            Item::Static(item) => self.item_static(item),
            Item::Struct(item) => self.item_struct(item),
            Item::Trait(item) => self.item_trait(item),
            Item::TraitAlias(item) => self.item_trait_alias(item),
            Item::Type(item) => self.item_type(item),
            Item::Union(item) => self.item_union(item),
            Item::Use(item) => self.item_use(item),
            Item::Verbatim(item) => self.item_verbatim(item),
            _ => unimplemented!("unknown Item"),
        }
    }

    fn item_const(&mut self, item: &ItemConst) {
        self.outer_attrs(&item.attrs);
        self.scan_begin_vertical(0);
        self.visibility(&item.vis);
        self.scan_string("const ");
        self.ident(&item.ident);
        self.generics(&item.generics);
        self.scan_string(": ");
        self.ty(&item.ty);
        self.scan_string(" = ");
        self.scan_break_never();
        self.expr(&item.expr);
        self.scan_string(";");
        self.scan_end();
        self.scan_trailing_comment(&item.attrs);
        self.scan_break_newline();
    }

    fn item_enum(&mut self, item: &ItemEnum) {
        self.outer_attrs(&item.attrs);
        self.scan_begin_vertical(self.config.indent);
        self.visibility(&item.vis);
        self.scan_string("enum ");
        self.ident(&item.ident);
        self.generics(&item.generics);
        self.where_clause_for_body(&item.generics.where_clause);
        self.scan_string("{");
        self.scan_trailing_comment(&item.attrs);
        self.scan_break_newline_if_nonempty();
        for variant in &item.variants {
            self.variant(variant);
            self.scan_string(",");
            self.scan_trailing_comment(&variant.attrs);
            self.scan_break_newline();
        }
        self.update_break_offset(-self.config.indent);
        self.scan_end();
        self.scan_string("}");
        self.scan_break_newline();
    }

    fn item_extern_crate(&mut self, item: &ItemExternCrate) {
        self.outer_attrs(&item.attrs);
        self.visibility(&item.vis);
        self.scan_string("extern crate ");
        self.ident(&item.ident);
        if let Some((_as_token, rename)) = &item.rename {
            self.scan_string(" as ");
            self.ident(rename);
        }
        self.scan_string(";");
        self.scan_break_newline();
    }

    fn item_fn(&mut self, item: &ItemFn) {
        self.outer_attrs(&item.attrs);
        self.smart_wrap_begin_default();

        // Feature F0002: Smart wrapping
        self.reset_wrap_tracker();

        self.visibility(&item.vis);
        self.signature(&item.sig);
        self.where_clause_for_body(&item.sig.generics.where_clause);
        self.scan_string("{");

        self.scan_trailing_comment(&item.attrs);
        self.scan_break_newline_if_nonempty();
        self.inner_attrs(&item.attrs);
        for stmt in &item.block.stmts {
            self.stmt(stmt);
        }
        self.update_break_offset(-self.config.indent);
        self.scan_end();
        self.scan_string("}");
        self.scan_break_newline();
    }

    fn item_foreign_mod(&mut self, item: &ItemForeignMod) {
        self.outer_attrs(&item.attrs);
        self.scan_begin_vertical(self.config.indent);
        if item.unsafety.is_some() {
            self.scan_string("unsafe ");
        }
        self.abi(&item.abi);
        self.scan_string("{");
        self.scan_break_newline_if_nonempty();
        self.inner_attrs(&item.attrs);
        for foreign_item in &item.items {
            self.foreign_item(foreign_item);
        }
        self.update_break_offset(-self.config.indent);
        self.scan_end();
        self.scan_string("}");
        self.scan_break_newline();
    }

    fn item_impl(&mut self, item: &ItemImpl) {
        self.outer_attrs(&item.attrs);
        self.scan_begin_vertical(self.config.indent);
        self.scan_begin_horizontal(-self.config.indent);
        self.scan_begin_vertical(self.config.indent);
        if item.defaultness.is_some() {
            self.scan_string("default ");
        }
        if item.unsafety.is_some() {
            self.scan_string("unsafe ");
        }
        self.scan_string("impl");
        self.generics(&item.generics);
        self.scan_end();
        self.scan_space();
        if let Some((negative_polarity, path, _for_token)) = &item.trait_ {
            if negative_polarity.is_some() {
                self.scan_string("!");
            }
            self.scan_path(path, PathKind::Type);
            self.scan_break_space();
            self.scan_string("for ");
        }
        self.ty(&item.self_ty);
        self.scan_end();
        self.where_clause_for_body(&item.generics.where_clause);
        self.scan_string("{");
        self.scan_trailing_comment(&item.attrs);
        self.scan_break_newline_if_nonempty();
        self.inner_attrs(&item.attrs);
        for impl_item in &item.items {
            self.impl_item(impl_item);
        }
        self.update_break_offset(-self.config.indent);
        self.scan_end();
        self.scan_string("}");
        self.scan_break_newline();
    }

    fn item_macro(&mut self, item: &ItemMacro) {
        self.outer_attrs(&item.attrs);
        let semicolon = true;
        self.scan_mac(&item.mac, item.ident.as_ref(), semicolon);
        self.scan_trailing_comment(&item.attrs);
        self.scan_break_newline();
    }

    fn item_mod(&mut self, item: &ItemMod) {
        self.outer_attrs(&item.attrs);
        self.scan_begin_vertical(self.config.indent);
        self.visibility(&item.vis);
        if item.unsafety.is_some() {
            self.scan_string("unsafe ");
        }
        self.scan_string("mod ");
        self.ident(&item.ident);
        if let Some((_brace, items)) = &item.content {
            self.scan_string(" {");
            self.scan_trailing_comment(&item.attrs);
            self.scan_break_newline_if_nonempty();
            self.inner_attrs(&item.attrs);
            for item in items {
                self.item(item);
            }
            self.update_break_offset(-self.config.indent);
            self.scan_end();
            self.scan_string("}");
        } else {
            self.scan_string(";");
            self.scan_trailing_comment(&item.attrs);
            self.scan_end();
        }
        self.scan_break_newline();
    }

    fn item_static(&mut self, item: &ItemStatic) {
        self.outer_attrs(&item.attrs);
        self.smart_wrap_begin_zero();
        self.visibility(&item.vis);
        self.scan_string("static ");
        self.static_mutability(&item.mutability);
        self.ident(&item.ident);
        self.scan_string(": ");
        self.ty(&item.ty);
        self.scan_string(" = ");
        self.scan_break_never();
        self.expr(&item.expr);
        self.scan_string(";");
        self.scan_end();
        self.scan_trailing_comment(&item.attrs);
        self.scan_break_newline();
    }

    fn item_struct(&mut self, item: &ItemStruct) {
        self.outer_attrs(&item.attrs);
        self.scan_begin_vertical(self.config.indent);
        self.visibility(&item.vis);
        self.scan_string("struct ");
        self.ident(&item.ident);
        self.generics(&item.generics);
        match &item.fields {
            // With fields
            Fields::Named(fields) => {
                self.where_clause_for_body(&item.generics.where_clause);
                self.scan_string("{");
                self.scan_trailing_comment(&item.attrs);
                self.scan_break_newline_if_nonempty();

                for field in fields.named.iter() {
                    self.scan_field(field);
                    self.scan_string(",");
                    self.scan_trailing_comment(&field.attrs);
                    self.scan_break_newline();
                }

                self.update_break_offset(-self.config.indent);
                self.scan_end();
                self.scan_string("}");
            }
            Fields::Unnamed(fields) => {
                self.fields_unnamed(fields);
                self.where_clause_semi(&item.generics.where_clause);
                self.scan_end();
            }

            // Unit only e.g. 'struct Unit;'
            Fields::Unit => {
                self.where_clause_semi(&item.generics.where_clause);
                self.scan_end();
                self.scan_trailing_comment(&item.attrs);
            }
        }
        self.scan_break_newline();
    }

    fn item_trait(&mut self, item: &ItemTrait) {
        self.outer_attrs(&item.attrs);
        self.scan_begin_vertical(self.config.indent);
        self.visibility(&item.vis);
        if item.unsafety.is_some() {
            self.scan_string("unsafe ");
        }
        if item.auto_token.is_some() {
            self.scan_string("auto ");
        }
        self.scan_string("trait ");
        self.ident(&item.ident);
        self.generics(&item.generics);
        for supertrait in item.supertraits.iter().delimited() {
            if supertrait.is_first {
                self.scan_string(": ");
            } else {
                self.scan_string(" + ");
            }
            self.type_param_bound(&supertrait);
        }
        self.where_clause_for_body(&item.generics.where_clause);
        self.scan_string("{");
        self.scan_trailing_comment(&item.attrs);
        self.scan_break_newline_if_nonempty();
        self.inner_attrs(&item.attrs);
        for trait_item in &item.items {
            self.trait_item(trait_item);
        }
        self.update_break_offset(-self.config.indent);
        self.scan_end();
        self.scan_string("}");
        self.scan_break_newline();
    }

    fn item_trait_alias(&mut self, item: &ItemTraitAlias) {
        self.outer_attrs(&item.attrs);
        self.scan_begin_vertical(self.config.indent);
        self.visibility(&item.vis);
        self.scan_string("trait ");
        self.ident(&item.ident);
        self.generics(&item.generics);
        self.scan_string(" = ");
        self.scan_break_never();
        for bound in item.bounds.iter().delimited() {
            if !bound.is_first {
                self.scan_break_space();
                self.scan_string("+ ");
            }
            self.type_param_bound(&bound);
        }
        self.where_clause_semi(&item.generics.where_clause);
        self.scan_end();
        self.scan_break_newline();
    }

    fn item_type(&mut self, item: &ItemType) {
        self.outer_attrs(&item.attrs);
        self.scan_begin_vertical(self.config.indent);
        self.visibility(&item.vis);
        self.scan_string("type ");
        self.ident(&item.ident);
        self.generics(&item.generics);
        self.where_clause_oneline(&item.generics.where_clause);
        self.scan_string("= ");
        self.scan_break_never();
        self.scan_begin_horizontal(-self.config.indent);
        self.ty(&item.ty);
        self.scan_end();
        self.scan_string(";");
        self.scan_end();
        self.scan_break_newline();
    }

    fn item_union(&mut self, item: &ItemUnion) {
        self.outer_attrs(&item.attrs);
        self.scan_begin_vertical(self.config.indent);
        self.visibility(&item.vis);
        self.scan_string("union ");
        self.ident(&item.ident);
        self.generics(&item.generics);
        self.where_clause_for_body(&item.generics.where_clause);
        self.scan_string("{");
        self.scan_break_newline_if_nonempty();
        for field in &item.fields.named {
            self.scan_field(field);
            self.scan_string(",");
            self.scan_break_newline();
        }
        self.update_break_offset(-self.config.indent);
        self.scan_end();
        self.scan_string("}");
        self.scan_break_newline();
    }

    fn item_use(&mut self, item: &ItemUse) {
        self.outer_attrs(&item.attrs);
        self.visibility(&item.vis);
        self.scan_string("use ");
        if item.leading_colon.is_some() {
            self.scan_string("::");
        }
        self.use_tree(&item.tree);
        self.scan_string(";");
        self.scan_break_newline();
    }

    #[cfg(not(feature = "verbatim"))]
    fn item_verbatim(&mut self, item: &TokenStream) {
        if !item.is_empty() {
            unimplemented!("Item::Verbatim `{}`", item);
        }
        self.scan_break_newline();
    }

    fn use_tree(&mut self, use_tree: &UseTree) {
        match use_tree {
            UseTree::Path(use_path) => self.use_path(use_path),
            UseTree::Name(use_name) => self.use_name(use_name),
            UseTree::Rename(use_rename) => self.use_rename(use_rename),
            UseTree::Glob(use_glob) => self.use_glob(use_glob),
            UseTree::Group(use_group) => self.use_group(use_group),
        }
    }

    fn use_path(&mut self, use_path: &UsePath) {
        self.ident(&use_path.ident);
        self.scan_string("::");
        self.use_tree(&use_path.tree);
    }

    fn use_name(&mut self, use_name: &UseName) {
        self.ident(&use_name.ident);
    }

    fn use_rename(&mut self, use_rename: &UseRename) {
        self.ident(&use_rename.ident);
        self.scan_string(" as ");
        self.ident(&use_rename.rename);
    }

    fn use_glob(&mut self, use_glob: &UseGlob) {
        let _ = use_glob;
        self.scan_string("*");
    }

    fn use_group(&mut self, use_group: &UseGroup) {
        if use_group.items.is_empty() {
            self.scan_string("{}");
        } else if use_group.items.len() == 1
            && match &use_group.items[0] {
                UseTree::Rename(use_rename) => use_rename.ident != "self",
                _ => true,
            }
        {
            self.use_tree(&use_group.items[0]);
        } else {
            self.scan_begin_vertical(self.config.indent);
            self.scan_string("{");
            self.scan_break_zero();
            self.scan_begin_horizontal(0);
            for use_tree in use_group.items.iter().delimited() {
                self.use_tree(&use_tree);
                if !use_tree.is_last {
                    self.scan_string(",");
                    let mut use_tree = *use_tree;
                    while let UseTree::Path(use_path) = use_tree {
                        use_tree = &use_path.tree;
                    }
                    if let UseTree::Group(_) = use_tree {
                        self.scan_break_newline();
                    } else {
                        self.scan_break_space();
                    }
                }
            }
            self.scan_end();
            self.trailing_comma(true);
            self.update_break_offset(-self.config.indent);
            self.scan_string("}");
            self.scan_end();
        }
    }

    fn foreign_item(&mut self, foreign_item: &ForeignItem) {
        match foreign_item {
            #![cfg_attr(all(test, exhaustive), deny(non_exhaustive_omitted_patterns))]
            ForeignItem::Fn(item) => self.foreign_item_fn(item),
            ForeignItem::Static(item) => self.foreign_item_static(item),
            ForeignItem::Type(item) => self.foreign_item_type(item),
            ForeignItem::Macro(item) => self.foreign_item_macro(item),
            ForeignItem::Verbatim(item) => self.foreign_item_verbatim(item),
            _ => unimplemented!("unknown ForeignItem"),
        }
    }

    fn foreign_item_fn(&mut self, foreign_item: &ForeignItemFn) {
        self.outer_attrs(&foreign_item.attrs);
        self.scan_begin_vertical(self.config.indent);
        self.visibility(&foreign_item.vis);
        self.signature(&foreign_item.sig);
        self.where_clause_semi(&foreign_item.sig.generics.where_clause);
        self.scan_end();
        self.scan_break_newline();
    }

    fn foreign_item_static(&mut self, foreign_item: &ForeignItemStatic) {
        self.outer_attrs(&foreign_item.attrs);
        self.scan_begin_vertical(0);
        self.visibility(&foreign_item.vis);
        self.scan_string("static ");
        self.static_mutability(&foreign_item.mutability);
        self.ident(&foreign_item.ident);
        self.scan_string(": ");
        self.ty(&foreign_item.ty);
        self.scan_string(";");
        self.scan_end();
        self.scan_break_newline();
    }

    fn foreign_item_type(&mut self, foreign_item: &ForeignItemType) {
        self.outer_attrs(&foreign_item.attrs);
        self.scan_begin_vertical(0);
        self.visibility(&foreign_item.vis);
        self.scan_string("type ");
        self.ident(&foreign_item.ident);
        self.generics(&foreign_item.generics);
        self.scan_string(";");
        self.scan_end();
        self.scan_break_newline();
    }

    fn foreign_item_macro(&mut self, foreign_item: &ForeignItemMacro) {
        self.outer_attrs(&foreign_item.attrs);
        let semicolon = true;
        self.scan_mac(&foreign_item.mac, None, semicolon);
        self.scan_break_newline();
    }

    #[cfg(not(feature = "verbatim"))]
    fn foreign_item_verbatim(&mut self, foreign_item: &TokenStream) {
        if !foreign_item.is_empty() {
            unimplemented!("ForeignItem::Verbatim `{}`", foreign_item);
        }
        self.scan_break_newline();
    }

    fn trait_item(&mut self, trait_item: &TraitItem) {
        match trait_item {
            #![cfg_attr(all(test, exhaustive), deny(non_exhaustive_omitted_patterns))]
            TraitItem::Const(item) => self.trait_item_const(item),
            TraitItem::Fn(item) => self.trait_item_fn(item),
            TraitItem::Type(item) => self.trait_item_type(item),
            TraitItem::Macro(item) => self.trait_item_macro(item),
            TraitItem::Verbatim(item) => self.trait_item_verbatim(item),
            _ => unimplemented!("unknown TraitItem"),
        }
    }

    fn trait_item_const(&mut self, trait_item: &TraitItemConst) {
        self.outer_attrs(&trait_item.attrs);
        self.scan_begin_vertical(0);
        self.scan_string("const ");
        self.ident(&trait_item.ident);
        self.generics(&trait_item.generics);
        self.scan_string(": ");
        self.ty(&trait_item.ty);
        if let Some((_eq_token, default)) = &trait_item.default {
            self.scan_string(" = ");
            self.scan_break_never();
            self.expr(default);
        }
        self.scan_string(";");
        self.scan_end();
        self.scan_trailing_comment(&trait_item.attrs);
        self.scan_break_newline();
    }

    fn trait_item_fn(&mut self, trait_item: &TraitItemFn) {
        self.outer_attrs(&trait_item.attrs);
        self.scan_begin_vertical(self.config.indent);
        self.signature(&trait_item.sig);
        if let Some(block) = &trait_item.default {
            self.where_clause_for_body(&trait_item.sig.generics.where_clause);
            self.scan_string("{");
            self.scan_trailing_comment(&trait_item.attrs);
            self.scan_break_newline_if_nonempty();
            self.inner_attrs(&trait_item.attrs);
            for stmt in &block.stmts {
                self.stmt(stmt);
            }
            self.update_break_offset(-self.config.indent);
            self.scan_end();
            self.scan_string("}");
        } else {
            self.where_clause_semi(&trait_item.sig.generics.where_clause);
            self.scan_end();
            self.scan_trailing_comment(&trait_item.attrs);
        }
        self.scan_break_newline();
    }

    fn trait_item_type(&mut self, trait_item: &TraitItemType) {
        self.outer_attrs(&trait_item.attrs);
        self.scan_begin_vertical(self.config.indent);
        self.scan_string("type ");
        self.ident(&trait_item.ident);
        self.generics(&trait_item.generics);
        for bound in trait_item.bounds.iter().delimited() {
            if bound.is_first {
                self.scan_string(": ");
            } else {
                self.scan_break_space();
                self.scan_string("+ ");
            }
            self.type_param_bound(&bound);
        }
        if let Some((_eq_token, default)) = &trait_item.default {
            self.scan_string(" = ");
            self.scan_break_never();
            self.scan_begin_horizontal(-self.config.indent);
            self.ty(default);
            self.scan_end();
        }
        self.where_clause_oneline_semi(&trait_item.generics.where_clause);
        self.scan_end();
        self.scan_trailing_comment(&trait_item.attrs);
        self.scan_break_newline();
    }

    fn trait_item_macro(&mut self, trait_item: &TraitItemMacro) {
        self.outer_attrs(&trait_item.attrs);
        let semicolon = true;
        self.scan_mac(&trait_item.mac, None, semicolon);
        self.scan_break_newline();
    }

    #[cfg(not(feature = "verbatim"))]
    fn trait_item_verbatim(&mut self, trait_item: &TokenStream) {
        if !trait_item.is_empty() {
            unimplemented!("TraitItem::Verbatim `{}`", trait_item);
        }
        self.scan_break_newline();
    }

    fn impl_item(&mut self, impl_item: &ImplItem) {
        match impl_item {
            #![cfg_attr(all(test, exhaustive), deny(non_exhaustive_omitted_patterns))]
            ImplItem::Const(item) => self.impl_item_const(item),
            ImplItem::Fn(item) => self.impl_item_fn(item),
            ImplItem::Type(item) => self.impl_item_type(item),
            ImplItem::Macro(item) => self.impl_item_macro(item),
            ImplItem::Verbatim(item) => self.impl_item_verbatim(item),
            _ => unimplemented!("unknown ImplItem"),
        }
    }

    fn impl_item_const(&mut self, impl_item: &ImplItemConst) {
        self.outer_attrs(&impl_item.attrs);
        self.scan_begin_vertical(0);
        self.visibility(&impl_item.vis);
        if impl_item.defaultness.is_some() {
            self.scan_string("default ");
        }
        self.scan_string("const ");
        self.ident(&impl_item.ident);
        self.generics(&impl_item.generics);
        self.scan_string(": ");
        self.ty(&impl_item.ty);
        self.scan_string(" = ");
        self.scan_break_never();
        self.expr(&impl_item.expr);
        self.scan_string(";");
        self.scan_end();
        self.scan_break_newline();
    }

    fn impl_item_fn(&mut self, impl_item: &ImplItemFn) {
        self.outer_attrs(&impl_item.attrs);
        self.scan_begin_vertical(self.config.indent);
        self.visibility(&impl_item.vis);
        if impl_item.defaultness.is_some() {
            self.scan_string("default ");
        }
        self.signature(&impl_item.sig);
        self.where_clause_for_body(&impl_item.sig.generics.where_clause);

        // Want this on the next line if the prior line was a smart wrap
        self.scan_string("{");

        self.scan_break_newline_if_nonempty();
        self.inner_attrs(&impl_item.attrs);
        for stmt in &impl_item.block.stmts {
            self.stmt(stmt);
        }
        self.update_break_offset(-self.config.indent);
        self.scan_end();
        self.scan_string("}");
        self.scan_break_newline();
    }

    fn impl_item_type(&mut self, impl_item: &ImplItemType) {
        self.outer_attrs(&impl_item.attrs);
        self.scan_begin_vertical(self.config.indent);
        self.visibility(&impl_item.vis);
        if impl_item.defaultness.is_some() {
            self.scan_string("default ");
        }
        self.scan_string("type ");
        self.ident(&impl_item.ident);
        self.generics(&impl_item.generics);
        self.scan_string(" = ");
        self.scan_break_never();
        self.scan_begin_horizontal(-self.config.indent);
        self.ty(&impl_item.ty);
        self.scan_end();
        self.where_clause_oneline_semi(&impl_item.generics.where_clause);
        self.scan_end();
        self.scan_break_newline();
    }

    fn impl_item_macro(&mut self, impl_item: &ImplItemMacro) {
        self.outer_attrs(&impl_item.attrs);
        let semicolon = true;
        self.scan_mac(&impl_item.mac, None, semicolon);
        self.scan_break_newline();
    }

    #[cfg(not(feature = "verbatim"))]
    fn impl_item_verbatim(&mut self, impl_item: &TokenStream) {
        if !impl_item.is_empty() {
            unimplemented!("ImplItem::Verbatim `{}`", impl_item);
        }
        self.scan_break_newline();
    }

    fn signature(&mut self, signature: &Signature) {
        if signature.constness.is_some() {
            self.scan_string("const ");
        }
        if signature.asyncness.is_some() {
            self.scan_string("async ");
        }
        if signature.unsafety.is_some() {
            self.scan_string("unsafe ");
        }
        if let Some(abi) = &signature.abi {
            self.abi(abi);
        }
        self.scan_string("fn ");
        self.ident(&signature.ident);
        self.generics(&signature.generics);
        self.scan_string("(");
        self.scan_break_never();

        // Signature params
        self.smart_wrap_begin_zero();
        self.scan_break_zero();
        for input in signature.inputs.iter().delimited() {
            self.fn_arg(&input);
            let is_last = input.is_last && signature.variadic.is_none();
            self.trailing_comma(is_last);
        }
        if let Some(variadic) = &signature.variadic {
            self.variadic(variadic);
            self.scan_break_zero();
        }
        self.update_break_offset(-self.config.indent);
        self.scan_end();

        self.scan_string(")");
        self.scan_begin_vertical(-self.config.indent);
        self.return_type(&signature.output);
        self.scan_end();
    }

    fn fn_arg(&mut self, fn_arg: &FnArg) {
        match fn_arg {
            FnArg::Receiver(receiver) => self.fn_arg_receiver(receiver),
            FnArg::Typed(pat_type) => self.pat_type(pat_type),
        }
    }

    fn fn_arg_receiver(&mut self, receiver: &Receiver) {
        self.outer_attrs(&receiver.attrs);
        if let Some((_ampersand, lifetime)) = &receiver.reference {
            self.scan_string("&");
            if let Some(lifetime) = lifetime {
                self.lifetime(lifetime);
                self.scan_space();
            }
        }
        if receiver.mutability.is_some() {
            self.scan_string("mut ");
        }
        self.scan_string("self");
        if receiver.colon_token.is_some() {
            self.scan_string(": ");
            self.ty(&receiver.ty);
        } else {
            let consistent = match (&receiver.reference, &receiver.mutability, &*receiver.ty) {
                (Some(_), mutability, Type::Reference(ty)) => {
                    mutability.is_some() == ty.mutability.is_some()
                        && match &*ty.elem {
                            Type::Path(ty) => ty.qself.is_none() && ty.path.is_ident("Self"),
                            _ => false,
                        }
                }
                (None, _, Type::Path(ty)) => ty.qself.is_none() && ty.path.is_ident("Self"),
                _ => false,
            };
            if !consistent {
                self.scan_string(": ");
                self.ty(&receiver.ty);
            }
        }
    }

    fn variadic(&mut self, variadic: &Variadic) {
        self.outer_attrs(&variadic.attrs);
        if let Some((pat, _colon)) = &variadic.pat {
            self.pat(pat);
            self.scan_string(": ");
        }
        self.scan_string("...");
    }

    fn static_mutability(&mut self, mutability: &StaticMutability) {
        match mutability {
            #![cfg_attr(all(test, exhaustive), deny(non_exhaustive_omitted_patterns))]
            StaticMutability::Mut(_) => self.scan_string("mut "),
            StaticMutability::None => {}
            _ => unimplemented!("unknown StaticMutability"),
        }
    }
}
