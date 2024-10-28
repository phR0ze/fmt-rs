use crate::path::PathKind;
use crate::Engine;
use proc_macro2::{Delimiter, Group, TokenStream, TokenTree};
use syn::{AttrStyle, Attribute, Expr, Lit, MacroDelimiter, Meta, MetaList, MetaNameValue};

impl Engine {
    /// Scan outer attributes like `#[doc = "value"]`
    pub(crate) fn outer_attrs(&mut self, attrs: &[Attribute]) {
        for attr in attrs {
            if let AttrStyle::Outer = attr.style {
                // Skip trailing comments as they are attached to the previous item
                // and as such have been handled already.
                if is_trailing_comment(attr) {
                    continue;
                }
                self.attr(attr);
            }
        }
    }

    /// Scan inner attributes like `#![doc = "value"]`
    pub(crate) fn inner_attrs(&mut self, attrs: &[Attribute]) {
        for attr in attrs {
            if let AttrStyle::Inner(_) = attr.style {
                self.attr(attr);
            }
        }
    }

    /// Iterate over the attributes and scan the first trailing comment
    pub(crate) fn scan_trailing_comment(&mut self, attrs: &[Attribute]) -> bool {
        for attr in attrs {
            if let Some(mut comment) = value_of_attribute("comment_line_trailing", attr) {
                self.scan_space();
                trim_trailing_spaces(&mut comment);
                self.scan_string("//");
                self.scan_string(comment);
                return true;
            }
        }
        false
    }

    /// Scan the attribute
    fn attr(&mut self, attr: &Attribute) {
        if let Some(mut doc) = value_of_attribute("doc", attr) {
            // Single line doc comment
            if !doc.contains('\n')
                && match attr.style {
                    AttrStyle::Outer => !doc.starts_with('/'),
                    AttrStyle::Inner(_) => true,
                }
            {
                trim_trailing_spaces(&mut doc);

                // Queue up the doc comment for printing with its correct styled prefix
                self.scan_string(match attr.style {
                    AttrStyle::Outer => "///",
                    AttrStyle::Inner(_) => "//!",
                });
                self.scan_string(doc);
                self.scan_break_newline(); // Add a newline after the doc comment
                return;
            } else if can_be_block_comment(&doc)
                && match attr.style {
                    AttrStyle::Outer => !doc.starts_with(&['*', '/'][..]),
                    AttrStyle::Inner(_) => true,
                }
            {
                trim_interior_trailing_spaces(&mut doc);
                self.scan_string(match attr.style {
                    AttrStyle::Outer => "/**",
                    AttrStyle::Inner(_) => "/*!",
                });
                self.scan_string(doc);
                self.scan_string("*/");
                self.scan_break_newline();
                return;
            }
        } else if let Some(_) = value_of_attribute("comment_empty", attr) {
            self.scan_break_newline();
            return;
        } else if let Some(mut comment) = value_of_attribute("comment_line", attr) {
            trim_trailing_spaces(&mut comment);
            self.scan_string("//");
            self.scan_string(comment);
            self.scan_break_newline();
            return;
        } else if let Some(mut comment) = value_of_attribute("comment_block", attr) {
            trim_interior_trailing_spaces(&mut comment);
            self.scan_string("/*");
            self.scan_string(comment);
            self.scan_string("*/");
            self.scan_break_newline();
            return;
        }

        self.scan_string(match attr.style {
            AttrStyle::Outer => "#",
            AttrStyle::Inner(_) => "#!",
        });
        self.scan_string("[");
        self.meta(&attr.meta);
        self.scan_string("]");
        self.scan_break_space();
    }

    fn meta(&mut self, meta: &Meta) {
        match meta {
            Meta::Path(path) => self.scan_path(path, PathKind::Simple),
            Meta::List(meta) => self.meta_list(meta),
            Meta::NameValue(meta) => self.meta_name_value(meta),
        }
    }

    fn meta_list(&mut self, meta: &MetaList) {
        self.scan_path(&meta.path, PathKind::Simple);
        let delimiter = match meta.delimiter {
            MacroDelimiter::Paren(_) => Delimiter::Parenthesis,
            MacroDelimiter::Brace(_) => Delimiter::Brace,
            MacroDelimiter::Bracket(_) => Delimiter::Bracket,
        };
        let group = Group::new(delimiter, meta.tokens.clone());
        self.attr_tokens(TokenStream::from(TokenTree::Group(group)));
    }

    fn meta_name_value(&mut self, meta: &MetaNameValue) {
        self.scan_path(&meta.path, PathKind::Simple);
        self.scan_string(" = ");
        self.expr(&meta.value);
    }

    fn attr_tokens(&mut self, tokens: TokenStream) {
        let mut stack = Vec::new();
        stack.push((tokens.into_iter().peekable(), Delimiter::None));
        let mut space = Self::scan_space as fn(&mut Self);

        #[derive(PartialEq)]
        enum State {
            Word,
            Punct,
            TrailingComma,
        }

        use State::*;
        let mut state = Word;

        while let Some((tokens, delimiter)) = stack.last_mut() {
            match tokens.next() {
                Some(TokenTree::Ident(ident)) => {
                    if let Word = state {
                        space(self);
                    }
                    self.ident(&ident);
                    state = Word;
                }
                Some(TokenTree::Punct(punct)) => {
                    let ch = punct.as_char();
                    if let (Word, '=') = (state, ch) {
                        self.scan_space();
                    }
                    if ch == ',' && tokens.peek().is_none() {
                        self.trailing_comma(true);
                        state = TrailingComma;
                    } else {
                        self.token_punct(ch);
                        if ch == '=' {
                            self.scan_space();
                        } else if ch == ',' {
                            space(self);
                        }
                        state = Punct;
                    }
                }
                Some(TokenTree::Literal(literal)) => {
                    if let Word = state {
                        space(self);
                    }
                    self.token_literal(&literal);
                    state = Word;
                }
                Some(TokenTree::Group(group)) => {
                    let delimiter = group.delimiter();
                    let stream = group.stream();
                    match delimiter {
                        Delimiter::Parenthesis => {
                            self.scan_string("(");
                            self.scan_begin_vertical(self.config.indent);
                            self.scan_break_zero();
                            state = Punct;
                        }
                        Delimiter::Brace => {
                            self.scan_string("{");
                            state = Punct;
                        }
                        Delimiter::Bracket => {
                            self.scan_string("[");
                            state = Punct;
                        }
                        Delimiter::None => {}
                    }
                    stack.push((stream.into_iter().peekable(), delimiter));
                    space = Self::scan_break_space;
                }
                None => {
                    match delimiter {
                        Delimiter::Parenthesis => {
                            if state != TrailingComma {
                                self.scan_break_zero();
                            }
                            self.update_break_offset(-self.config.indent);
                            self.scan_end();
                            self.scan_string(")");
                            state = Punct;
                        }
                        Delimiter::Brace => {
                            self.scan_string("}");
                            state = Punct;
                        }
                        Delimiter::Bracket => {
                            self.scan_string("]");
                            state = Punct;
                        }
                        Delimiter::None => {}
                    }
                    stack.pop();
                    if stack.is_empty() {
                        space = Self::scan_space;
                    }
                }
            }
        }
    }
}

/// Detect if the given attributes have a trailing comment
/// Feature F0001: Developer comments
pub(crate) fn have_trailing_comment(attrs: &[Attribute]) -> bool {
    for attr in attrs {
        if let AttrStyle::Outer = attr.style {
            if is_trailing_comment(attr) {
                return true;
            }
        }
    }
    false
}

/// Detect if the given attribute is a trailing comment
/// Feature F0001: Developer comments
fn is_trailing_comment(attr: &Attribute) -> bool {
    value_of_attribute("comment_line_trailing", attr).is_some()
}

/// Get the value of the attribute e.g. `#[doc = "value"]`
fn value_of_attribute(requested: &str, attr: &Attribute) -> Option<String> {
    let value = match &attr.meta {
        Meta::NameValue(meta) if meta.path.is_ident(requested) => &meta.value,
        _ => return None,
    };
    let lit = match value {
        Expr::Lit(expr) if expr.attrs.is_empty() => &expr.lit,
        _ => return None,
    };
    match lit {
        Lit::Str(string) => Some(string.value()),
        _ => None,
    }
}

/// Debugging function to print the attributes
#[allow(dead_code)]
#[cfg(test)]
fn debug_print_attrs(attrs: &[Attribute]) {
    for attr in attrs {
        debug_print_attr(attr);
    }
}

/// Debugging function to print the attributes
#[allow(dead_code)]
#[cfg(test)]
fn debug_print_attr(attr: &Attribute) {
    let style = match attr.style {
        AttrStyle::Outer => "Outer",
        _ => "Inner",
    };
    if let Meta::NameValue(meta) = &attr.meta {
        if let Expr::Lit(expr) = &meta.value {
            if let Lit::Str(lit) = &expr.lit {
                println!(
                    "{}: {}, {}",
                    style,
                    meta.path.get_ident().unwrap(),
                    lit.value()
                );
            }
        }
    };
}

pub fn has_outer(attrs: &[Attribute]) -> bool {
    for attr in attrs {
        if let AttrStyle::Outer = attr.style {
            return true;
        }
    }
    false
}

pub fn has_inner(attrs: &[Attribute]) -> bool {
    for attr in attrs {
        if let AttrStyle::Inner(_) = attr.style {
            return true;
        }
    }
    false
}

fn trim_trailing_spaces(doc: &mut String) {
    doc.truncate(doc.trim_end_matches(' ').len());
}

fn trim_interior_trailing_spaces(doc: &mut String) {
    if !doc.contains(" \n") {
        return;
    }
    let mut trimmed = String::with_capacity(doc.len());
    let mut lines = doc.split('\n').peekable();
    while let Some(line) = lines.next() {
        if lines.peek().is_some() {
            trimmed.push_str(line.trim_end_matches(' '));
            trimmed.push('\n');
        } else {
            trimmed.push_str(line);
        }
    }
    *doc = trimmed;
}

fn can_be_block_comment(value: &str) -> bool {
    let mut depth = 0usize;
    let bytes = value.as_bytes();
    let mut i = 0usize;
    let upper = bytes.len() - 1;

    while i < upper {
        if bytes[i] == b'/' && bytes[i + 1] == b'*' {
            depth += 1;
            i += 2;
        } else if bytes[i] == b'*' && bytes[i + 1] == b'/' {
            if depth == 0 {
                return false;
            }
            depth -= 1;
            i += 2;
        } else {
            i += 1;
        }
    }

    depth == 0
}
