use crate::attrs;
use crate::engine::Engine;
use crate::iter::IterDelimited;
use crate::model::BreakToken;
use crate::path::PathKind;
use crate::stmt;
use proc_macro2::TokenStream;
use syn::punctuated::Punctuated;
use syn::{
    token, Arm, Attribute, BinOp, Block, Expr, ExprArray, ExprAssign, ExprAsync, ExprAwait,
    ExprBinary, ExprBlock, ExprBreak, ExprCall, ExprCast, ExprClosure, ExprConst, ExprContinue,
    ExprField, ExprForLoop, ExprGroup, ExprIf, ExprIndex, ExprInfer, ExprLet, ExprLit, ExprLoop,
    ExprMacro, ExprMatch, ExprMethodCall, ExprParen, ExprPath, ExprRange, ExprReference,
    ExprRepeat, ExprReturn, ExprStruct, ExprTry, ExprTryBlock, ExprTuple, ExprUnary, ExprUnsafe,
    ExprWhile, ExprYield, FieldValue, Index, Label, Member, RangeLimits, ReturnType, Stmt, Token,
    UnOp,
};

impl Engine {
    pub fn expr(&mut self, expr: &Expr) {
        let beginning_of_line = false;
        match expr {
            #![cfg_attr(all(test, exhaustive), deny(non_exhaustive_omitted_patterns))]
            Expr::Array(expr) => self.expr_array(expr),
            Expr::Assign(expr) => self.expr_assign(expr),
            Expr::Async(expr) => self.expr_async(expr),
            Expr::Await(expr) => self.expr_await(expr, beginning_of_line),
            Expr::Binary(expr) => self.expr_binary(expr),
            Expr::Block(expr) => self.expr_block(expr),
            Expr::Break(expr) => self.expr_break(expr),
            Expr::Call(expr) => self.expr_call(expr, beginning_of_line),
            Expr::Cast(expr) => self.expr_cast(expr),
            Expr::Closure(expr) => self.expr_closure(expr),
            Expr::Const(expr) => self.expr_const(expr),
            Expr::Continue(expr) => self.expr_continue(expr),
            Expr::Field(expr) => self.expr_field(expr, beginning_of_line),
            Expr::ForLoop(expr) => self.expr_for_loop(expr),
            Expr::Group(expr) => self.expr_group(expr),
            Expr::If(expr) => self.expr_if(expr),
            Expr::Index(expr) => self.expr_index(expr, beginning_of_line),
            Expr::Infer(expr) => self.expr_infer(expr),
            Expr::Let(expr) => self.expr_let(expr),
            Expr::Lit(expr) => self.expr_lit(expr),
            Expr::Loop(expr) => self.expr_loop(expr),
            Expr::Macro(expr) => self.expr_macro(expr),
            Expr::Match(expr) => self.expr_match(expr),
            Expr::MethodCall(expr) => self.expr_method_call(expr, beginning_of_line),
            Expr::Paren(expr) => self.expr_paren(expr),
            Expr::Path(expr) => self.expr_path(expr),
            Expr::Range(expr) => self.expr_range(expr),
            Expr::Reference(expr) => self.expr_reference(expr),
            Expr::Repeat(expr) => self.expr_repeat(expr),
            Expr::Return(expr) => self.expr_return(expr),
            Expr::Struct(expr) => self.expr_struct(expr),
            Expr::Try(expr) => self.expr_try(expr, beginning_of_line),
            Expr::TryBlock(expr) => self.expr_try_block(expr),
            Expr::Tuple(expr) => self.expr_tuple(expr),
            Expr::Unary(expr) => self.expr_unary(expr),
            Expr::Unsafe(expr) => self.expr_unsafe(expr),
            Expr::Verbatim(expr) => self.expr_verbatim(expr),
            Expr::While(expr) => self.expr_while(expr),
            Expr::Yield(expr) => self.expr_yield(expr),
            _ => unimplemented!("unknown Expr"),
        }
    }

    pub fn expr_beginning_of_line(&mut self, expr: &Expr, beginning_of_line: bool) {
        match expr {
            Expr::Await(expr) => self.expr_await(expr, beginning_of_line),
            Expr::Field(expr) => self.expr_field(expr, beginning_of_line),
            Expr::Index(expr) => self.expr_index(expr, beginning_of_line),
            Expr::MethodCall(expr) => self.expr_method_call(expr, beginning_of_line),
            Expr::Try(expr) => self.expr_try(expr, beginning_of_line),
            _ => self.expr(expr),
        }
    }

    fn subexpr(&mut self, expr: &Expr, beginning_of_line: bool) {
        match expr {
            Expr::Await(expr) => self.subexpr_await(expr, beginning_of_line),
            Expr::Call(expr) => self.subexpr_call(expr),
            Expr::Field(expr) => self.subexpr_field(expr, beginning_of_line),
            Expr::Index(expr) => self.subexpr_index(expr, beginning_of_line),
            Expr::MethodCall(expr) => {
                let unindent_call_args = false;
                self.subexpr_method_call(expr, beginning_of_line, unindent_call_args);
            }
            Expr::Try(expr) => self.subexpr_try(expr, beginning_of_line),
            _ => {
                self.scan_begin_consistent(-self.config.indent);
                self.expr(expr);
                self.scan_end();
            }
        }
    }

    fn wrap_exterior_struct(&mut self, expr: &Expr) {
        let needs_paren = contains_exterior_struct_lit(expr);
        if needs_paren {
            self.scan_string("(");
        }
        self.scan_begin_consistent(0);
        self.expr(expr);
        if needs_paren {
            self.scan_string(")");
        }
        if needs_newline_if_wrap(expr) {
            self.scan_space();
        } else {
            self.nbsp();
        }
        self.scan_end();
    }

    fn expr_array(&mut self, expr: &ExprArray) {
        self.outer_attrs(&expr.attrs);
        self.scan_string("[");
        self.scan_begin_consistent(self.config.indent);
        self.zerobreak();
        for element in expr.elems.iter().delimited() {
            self.expr(&element);
            self.trailing_comma(element.is_last);
        }
        self.offset(-self.config.indent);
        self.scan_end();
        self.scan_string("]");
    }

    fn expr_assign(&mut self, expr: &ExprAssign) {
        self.outer_attrs(&expr.attrs);
        self.scan_begin_inconsistent(0);
        self.expr(&expr.left);
        self.scan_string(" = ");
        self.neverbreak();
        self.expr(&expr.right);
        self.scan_end();
    }

    fn expr_async(&mut self, expr: &ExprAsync) {
        self.outer_attrs(&expr.attrs);
        self.scan_string("async ");
        if expr.capture.is_some() {
            self.scan_string("move ");
        }
        self.scan_begin_consistent(self.config.indent);
        self.small_block(&expr.block, &expr.attrs);
        self.scan_end();
    }

    fn expr_await(&mut self, expr: &ExprAwait, beginning_of_line: bool) {
        self.outer_attrs(&expr.attrs);
        self.scan_begin_consistent(self.config.indent);
        self.subexpr_await(expr, beginning_of_line);
        self.scan_end();
    }

    fn subexpr_await(&mut self, expr: &ExprAwait, beginning_of_line: bool) {
        self.subexpr(&expr.base, beginning_of_line);
        self.zerobreak_unless_short_ident(beginning_of_line, &expr.base);
        self.scan_string(".await");
    }

    fn expr_binary(&mut self, expr: &ExprBinary) {
        self.outer_attrs(&expr.attrs);
        self.scan_begin_inconsistent(self.config.indent);
        self.scan_begin_inconsistent(-self.config.indent);
        self.expr(&expr.left);
        self.scan_end();
        self.scan_space();
        self.binary_operator(&expr.op);
        self.nbsp();
        self.expr(&expr.right);
        self.scan_end();
    }

    pub fn expr_block(&mut self, expr: &ExprBlock) {
        self.outer_attrs(&expr.attrs);
        if let Some(label) = &expr.label {
            self.label(label);
        }
        self.scan_begin_consistent(self.config.indent);
        self.small_block(&expr.block, &expr.attrs);
        self.scan_end();
    }

    fn expr_break(&mut self, expr: &ExprBreak) {
        self.outer_attrs(&expr.attrs);
        self.scan_string("break");
        if let Some(lifetime) = &expr.label {
            self.nbsp();
            self.lifetime(lifetime);
        }
        if let Some(value) = &expr.expr {
            self.nbsp();
            self.expr(value);
        }
    }

    fn expr_call(&mut self, expr: &ExprCall, beginning_of_line: bool) {
        self.outer_attrs(&expr.attrs);
        self.expr_beginning_of_line(&expr.func, beginning_of_line);
        self.scan_string("(");
        self.call_args(&expr.args);
        self.scan_string(")");
    }

    fn subexpr_call(&mut self, expr: &ExprCall) {
        let beginning_of_line = false;
        self.subexpr(&expr.func, beginning_of_line);
        self.scan_string("(");
        self.call_args(&expr.args);
        self.scan_string(")");
    }

    fn expr_cast(&mut self, expr: &ExprCast) {
        self.outer_attrs(&expr.attrs);
        self.scan_begin_inconsistent(self.config.indent);
        self.scan_begin_inconsistent(-self.config.indent);
        self.expr(&expr.expr);
        self.scan_end();
        self.scan_space();
        self.scan_string("as ");
        self.ty(&expr.ty);
        self.scan_end();
    }

    fn expr_closure(&mut self, expr: &ExprClosure) {
        self.outer_attrs(&expr.attrs);
        self.scan_begin_inconsistent(0);
        if let Some(bound_lifetimes) = &expr.lifetimes {
            self.bound_lifetimes(bound_lifetimes);
        }
        if expr.constness.is_some() {
            self.scan_string("const ");
        }
        if expr.movability.is_some() {
            self.scan_string("static ");
        }
        if expr.asyncness.is_some() {
            self.scan_string("async ");
        }
        if expr.capture.is_some() {
            self.scan_string("move ");
        }
        self.scan_begin_consistent(self.config.indent);
        self.scan_string("|");
        for pat in expr.inputs.iter().delimited() {
            if pat.is_first {
                self.zerobreak();
            }
            self.pat(&pat);
            if !pat.is_last {
                self.scan_string(",");
                self.scan_space();
            }
        }
        match &expr.output {
            ReturnType::Default => {
                self.scan_string("|");
                self.scan_space();
                self.offset(-self.config.indent);
                self.scan_end();
                self.neverbreak();
                let wrap_in_brace = match &*expr.body {
                    Expr::Match(ExprMatch { attrs, .. }) | Expr::Call(ExprCall { attrs, .. }) => {
                        attrs::has_outer(attrs)
                    }
                    body => !is_blocklike(body),
                };
                if wrap_in_brace {
                    self.scan_begin_consistent(self.config.indent);
                    let okay_to_brace = parseable_as_stmt(&expr.body);
                    self.scan_break(BreakToken {
                        pre_break: Some(if okay_to_brace { '{' } else { '(' }),
                        ..BreakToken::default()
                    });
                    self.expr(&expr.body);
                    self.scan_break(BreakToken {
                        offset: -self.config.indent,
                        pre_break: (okay_to_brace && stmt::add_semi(&expr.body)).then(|| ';'),
                        post_break: Some(if okay_to_brace { '}' } else { ')' }),
                        ..BreakToken::default()
                    });
                    self.scan_end();
                } else {
                    self.expr(&expr.body);
                }
            }
            ReturnType::Type(_arrow, ty) => {
                if !expr.inputs.is_empty() {
                    self.trailing_comma(true);
                    self.offset(-self.config.indent);
                }
                self.scan_string("|");
                self.scan_end();
                self.scan_string(" -> ");
                self.ty(ty);
                self.nbsp();
                self.neverbreak();
                self.expr(&expr.body);
            }
        }
        self.scan_end();
    }

    pub fn expr_const(&mut self, expr: &ExprConst) {
        self.outer_attrs(&expr.attrs);
        self.scan_string("const ");
        self.scan_begin_consistent(self.config.indent);
        self.small_block(&expr.block, &expr.attrs);
        self.scan_end();
    }

    fn expr_continue(&mut self, expr: &ExprContinue) {
        self.outer_attrs(&expr.attrs);
        self.scan_string("continue");
        if let Some(lifetime) = &expr.label {
            self.nbsp();
            self.lifetime(lifetime);
        }
    }

    fn expr_field(&mut self, expr: &ExprField, beginning_of_line: bool) {
        self.outer_attrs(&expr.attrs);
        self.scan_begin_consistent(self.config.indent);
        self.subexpr_field(expr, beginning_of_line);
        self.scan_end();
    }

    fn subexpr_field(&mut self, expr: &ExprField, beginning_of_line: bool) {
        self.subexpr(&expr.base, beginning_of_line);
        self.zerobreak_unless_short_ident(beginning_of_line, &expr.base);
        self.scan_string(".");
        self.member(&expr.member);
    }

    fn expr_for_loop(&mut self, expr: &ExprForLoop) {
        self.outer_attrs(&expr.attrs);
        self.scan_begin_inconsistent(0);
        if let Some(label) = &expr.label {
            self.label(label);
        }
        self.scan_string("for ");
        self.pat(&expr.pat);
        self.scan_string(" in ");
        self.neverbreak();
        self.wrap_exterior_struct(&expr.expr);
        self.scan_string("{");
        self.neverbreak();
        self.scan_begin_consistent(self.config.indent);
        self.hardbreak_if_nonempty();
        self.inner_attrs(&expr.attrs);
        for stmt in &expr.body.stmts {
            self.stmt(stmt);
        }
        self.offset(-self.config.indent);
        self.scan_end();
        self.scan_string("}");
        self.scan_end();
    }

    fn expr_group(&mut self, expr: &ExprGroup) {
        self.outer_attrs(&expr.attrs);
        self.expr(&expr.expr);
    }

    fn expr_if(&mut self, expr: &ExprIf) {
        self.outer_attrs(&expr.attrs);
        self.scan_begin_consistent(self.config.indent);
        self.scan_string("if ");
        self.scan_begin_consistent(-self.config.indent);
        self.wrap_exterior_struct(&expr.cond);
        self.scan_end();
        if let Some((_else_token, else_branch)) = &expr.else_branch {
            let mut else_branch = &**else_branch;
            self.small_block(&expr.then_branch, &[]);
            loop {
                self.scan_string(" else ");
                match else_branch {
                    Expr::If(expr) => {
                        self.scan_string("if ");
                        self.scan_begin_consistent(-self.config.indent);
                        self.wrap_exterior_struct(&expr.cond);
                        self.scan_end();
                        self.small_block(&expr.then_branch, &[]);
                        if let Some((_else_token, next)) = &expr.else_branch {
                            else_branch = next;
                            continue;
                        }
                    }
                    Expr::Block(expr) => {
                        self.small_block(&expr.block, &[]);
                    }
                    // If not one of the valid expressions to exist in an else
                    // clause, wrap in a block.
                    other => {
                        self.scan_string("{");
                        self.scan_space();
                        self.scan_begin_inconsistent(self.config.indent);
                        self.expr(other);
                        self.scan_end();
                        self.scan_space();
                        self.offset(-self.config.indent);
                        self.scan_string("}");
                    }
                }
                break;
            }
        } else if expr.then_branch.stmts.is_empty() {
            self.scan_string("{}");
        } else {
            self.scan_string("{");
            self.scan_hardbreak();
            for stmt in &expr.then_branch.stmts {
                self.stmt(stmt);
            }
            self.offset(-self.config.indent);
            self.scan_string("}");
        }
        self.scan_end();
    }

    fn expr_index(&mut self, expr: &ExprIndex, beginning_of_line: bool) {
        self.outer_attrs(&expr.attrs);
        self.expr_beginning_of_line(&expr.expr, beginning_of_line);
        self.scan_string("[");
        self.expr(&expr.index);
        self.scan_string("]");
    }

    fn subexpr_index(&mut self, expr: &ExprIndex, beginning_of_line: bool) {
        self.subexpr(&expr.expr, beginning_of_line);
        self.scan_string("[");
        self.expr(&expr.index);
        self.scan_string("]");
    }

    fn expr_infer(&mut self, expr: &ExprInfer) {
        self.outer_attrs(&expr.attrs);
        self.scan_string("_");
    }

    fn expr_let(&mut self, expr: &ExprLet) {
        self.outer_attrs(&expr.attrs);
        self.scan_begin_inconsistent(0);
        self.scan_string("let ");
        self.scan_begin_inconsistent(0);
        self.pat(&expr.pat);
        self.scan_end();
        self.scan_string(" = ");
        self.neverbreak();
        self.scan_begin_inconsistent(0);
        let needs_paren = contains_exterior_struct_lit(&expr.expr);
        if needs_paren {
            self.scan_string("(");
        }
        self.expr(&expr.expr);
        if needs_paren {
            self.scan_string(")");
        }
        self.scan_end();
        self.scan_end();
    }

    pub fn expr_lit(&mut self, expr: &ExprLit) {
        self.outer_attrs(&expr.attrs);
        self.lit(&expr.lit);
    }

    fn expr_loop(&mut self, expr: &ExprLoop) {
        self.outer_attrs(&expr.attrs);
        if let Some(label) = &expr.label {
            self.label(label);
        }
        self.scan_string("loop {");
        self.scan_begin_consistent(self.config.indent);
        self.hardbreak_if_nonempty();
        self.inner_attrs(&expr.attrs);
        for stmt in &expr.body.stmts {
            self.stmt(stmt);
        }
        self.offset(-self.config.indent);
        self.scan_end();
        self.scan_string("}");
    }

    pub fn expr_macro(&mut self, expr: &ExprMacro) {
        self.outer_attrs(&expr.attrs);
        let semicolon = false;
        self.scan_mac(&expr.mac, None, semicolon);
    }

    fn expr_match(&mut self, expr: &ExprMatch) {
        self.outer_attrs(&expr.attrs);
        self.scan_begin_inconsistent(0);
        self.scan_string("match ");
        self.wrap_exterior_struct(&expr.expr);
        self.scan_string("{");
        self.neverbreak();
        self.scan_begin_consistent(self.config.indent);
        self.hardbreak_if_nonempty();
        self.inner_attrs(&expr.attrs);
        for arm in &expr.arms {
            self.arm(arm);
            self.scan_hardbreak();
        }
        self.offset(-self.config.indent);
        self.scan_end();
        self.scan_string("}");
        self.scan_end();
    }

    fn expr_method_call(&mut self, expr: &ExprMethodCall, beginning_of_line: bool) {
        self.outer_attrs(&expr.attrs);
        self.scan_begin_consistent(self.config.indent);
        let unindent_call_args = beginning_of_line && self.is_short_ident(&expr.receiver);
        self.subexpr_method_call(expr, beginning_of_line, unindent_call_args);
        self.scan_end();
    }

    fn subexpr_method_call(
        &mut self,
        expr: &ExprMethodCall,
        beginning_of_line: bool,
        unindent_call_args: bool,
    ) {
        self.subexpr(&expr.receiver, beginning_of_line);
        self.zerobreak_unless_short_ident(beginning_of_line, &expr.receiver);
        self.scan_string(".");
        self.ident(&expr.method);
        if let Some(turbofish) = &expr.turbofish {
            self.angle_bracketed_generic_arguments(turbofish, PathKind::Expr);
        }
        self.scan_begin_consistent(if unindent_call_args {
            -self.config.indent
        } else {
            0
        });
        self.scan_string("(");
        self.call_args(&expr.args);
        self.scan_string(")");
        self.scan_end();
    }

    fn expr_paren(&mut self, expr: &ExprParen) {
        self.outer_attrs(&expr.attrs);
        self.scan_string("(");
        self.expr(&expr.expr);
        self.scan_string(")");
    }

    pub fn expr_path(&mut self, expr: &ExprPath) {
        self.outer_attrs(&expr.attrs);
        self.qpath(&expr.qself, &expr.path, PathKind::Expr);
    }

    pub fn expr_range(&mut self, expr: &ExprRange) {
        self.outer_attrs(&expr.attrs);
        if let Some(start) = &expr.start {
            self.expr(start);
        }
        self.scan_string(match expr.limits {
            RangeLimits::HalfOpen(_) => "..",
            RangeLimits::Closed(_) => "..=",
        });
        if let Some(end) = &expr.end {
            self.expr(end);
        }
    }

    fn expr_reference(&mut self, expr: &ExprReference) {
        self.outer_attrs(&expr.attrs);
        self.scan_string("&");
        if expr.mutability.is_some() {
            self.scan_string("mut ");
        }
        self.expr(&expr.expr);
    }

    fn expr_repeat(&mut self, expr: &ExprRepeat) {
        self.outer_attrs(&expr.attrs);
        self.scan_string("[");
        self.expr(&expr.expr);
        self.scan_string("; ");
        self.expr(&expr.len);
        self.scan_string("]");
    }

    fn expr_return(&mut self, expr: &ExprReturn) {
        self.outer_attrs(&expr.attrs);
        self.scan_string("return");
        if let Some(value) = &expr.expr {
            self.nbsp();
            self.expr(value);
        }
    }

    fn expr_struct(&mut self, expr: &ExprStruct) {
        self.outer_attrs(&expr.attrs);
        self.scan_begin_consistent(self.config.indent);
        self.scan_begin_inconsistent(-self.config.indent);
        self.qpath(&expr.qself, &expr.path, PathKind::Expr);
        self.scan_end();
        self.scan_string(" {");
        self.space_if_nonempty();
        for field_value in expr.fields.iter().delimited() {
            self.field_value(&field_value);
            self.trailing_comma_or_space(field_value.is_last && expr.rest.is_none());
        }
        if let Some(rest) = &expr.rest {
            self.scan_string("..");
            self.expr(rest);
            self.scan_space();
        }
        self.offset(-self.config.indent);
        self.end_with_max_width(34);
        self.scan_string("}");
    }

    fn expr_try(&mut self, expr: &ExprTry, beginning_of_line: bool) {
        self.outer_attrs(&expr.attrs);
        self.expr_beginning_of_line(&expr.expr, beginning_of_line);
        self.scan_string("?");
    }

    fn subexpr_try(&mut self, expr: &ExprTry, beginning_of_line: bool) {
        self.subexpr(&expr.expr, beginning_of_line);
        self.scan_string("?");
    }

    fn expr_try_block(&mut self, expr: &ExprTryBlock) {
        self.outer_attrs(&expr.attrs);
        self.scan_string("try ");
        self.scan_begin_consistent(self.config.indent);
        self.small_block(&expr.block, &expr.attrs);
        self.scan_end();
    }

    fn expr_tuple(&mut self, expr: &ExprTuple) {
        self.outer_attrs(&expr.attrs);
        self.scan_string("(");
        self.scan_begin_consistent(self.config.indent);
        self.zerobreak();
        for elem in expr.elems.iter().delimited() {
            self.expr(&elem);
            if expr.elems.len() == 1 {
                self.scan_string(",");
                self.zerobreak();
            } else {
                self.trailing_comma(elem.is_last);
            }
        }
        self.offset(-self.config.indent);
        self.scan_end();
        self.scan_string(")");
    }

    fn expr_unary(&mut self, expr: &ExprUnary) {
        self.outer_attrs(&expr.attrs);
        self.unary_operator(&expr.op);
        self.expr(&expr.expr);
    }

    fn expr_unsafe(&mut self, expr: &ExprUnsafe) {
        self.outer_attrs(&expr.attrs);
        self.scan_string("unsafe ");
        self.scan_begin_consistent(self.config.indent);
        self.small_block(&expr.block, &expr.attrs);
        self.scan_end();
    }

    #[cfg(not(feature = "verbatim"))]
    fn expr_verbatim(&mut self, expr: &TokenStream) {
        if !expr.is_empty() {
            unimplemented!("Expr::Verbatim `{}`", expr);
        }
    }

    #[cfg(feature = "verbatim")]
    fn expr_verbatim(&mut self, tokens: &TokenStream) {
        use syn::parse::discouraged::Speculative;
        use syn::parse::{Parse, ParseStream, Result};
        use syn::{parenthesized, Ident};

        enum ExprVerbatim {
            Empty,
            Ellipsis,
            Become(Become),
            Builtin(Builtin),
            RawReference(RawReference),
        }

        struct Become {
            attrs: Vec<Attribute>,
            tail_call: Expr,
        }

        struct Builtin {
            attrs: Vec<Attribute>,
            name: Ident,
            args: TokenStream,
        }

        struct RawReference {
            attrs: Vec<Attribute>,
            mutable: bool,
            expr: Expr,
        }

        mod kw {
            syn::custom_keyword!(builtin);
            syn::custom_keyword!(raw);
        }

        impl Parse for ExprVerbatim {
            fn parse(input: ParseStream) -> Result<Self> {
                let ahead = input.fork();
                let attrs = ahead.call(Attribute::parse_outer)?;
                let lookahead = ahead.lookahead1();
                if input.is_empty() {
                    Ok(ExprVerbatim::Empty)
                } else if lookahead.peek(Token![become]) {
                    input.advance_to(&ahead);
                    input.parse::<Token![become]>()?;
                    let tail_call: Expr = input.parse()?;
                    Ok(ExprVerbatim::Become(Become { attrs, tail_call }))
                } else if lookahead.peek(kw::builtin) {
                    input.advance_to(&ahead);
                    input.parse::<kw::builtin>()?;
                    input.parse::<Token![#]>()?;
                    let name: Ident = input.parse()?;
                    let args;
                    parenthesized!(args in input);
                    let args: TokenStream = args.parse()?;
                    Ok(ExprVerbatim::Builtin(Builtin { attrs, name, args }))
                } else if lookahead.peek(Token![&]) {
                    input.advance_to(&ahead);
                    input.parse::<Token![&]>()?;
                    input.parse::<kw::raw>()?;
                    let mutable = input.parse::<Option<Token![mut]>>()?.is_some();
                    if !mutable {
                        input.parse::<Token![const]>()?;
                    }
                    let expr: Expr = input.parse()?;
                    Ok(ExprVerbatim::RawReference(RawReference {
                        attrs,
                        mutable,
                        expr,
                    }))
                } else if lookahead.peek(Token![...]) {
                    input.parse::<Token![...]>()?;
                    Ok(ExprVerbatim::Ellipsis)
                } else {
                    Err(lookahead.error())
                }
            }
        }

        let expr: ExprVerbatim = match syn::parse2(tokens.clone()) {
            Ok(expr) => expr,
            Err(_) => unimplemented!("Expr::Verbatim `{}`", tokens),
        };

        match expr {
            ExprVerbatim::Empty => {}
            ExprVerbatim::Ellipsis => {
                self.scan_string("...");
            }
            ExprVerbatim::Become(expr) => {
                self.outer_attrs(&expr.attrs);
                self.scan_string("become");
                self.nbsp();
                self.expr(&expr.tail_call);
            }
            ExprVerbatim::Builtin(expr) => {
                self.outer_attrs(&expr.attrs);
                self.scan_string("builtin # ");
                self.ident(&expr.name);
                self.scan_string("(");
                if !expr.args.is_empty() {
                    self.scan_begin_consistent(self.config.indent);
                    self.zerobreak();
                    self.scan_begin_inconsistent(0);
                    self.macro_rules_tokens(expr.args, false);
                    self.scan_end();
                    self.zerobreak();
                    self.offset(-self.config.indent);
                    self.scan_end();
                }
                self.scan_string(")");
            }
            ExprVerbatim::RawReference(expr) => {
                self.outer_attrs(&expr.attrs);
                self.scan_string("&raw ");
                self.scan_string(if expr.mutable { "mut " } else { "const " });
                self.expr(&expr.expr);
            }
        }
    }

    fn expr_while(&mut self, expr: &ExprWhile) {
        self.outer_attrs(&expr.attrs);
        if let Some(label) = &expr.label {
            self.label(label);
        }
        self.scan_string("while ");
        self.wrap_exterior_struct(&expr.cond);
        self.scan_string("{");
        self.neverbreak();
        self.scan_begin_consistent(self.config.indent);
        self.hardbreak_if_nonempty();
        self.inner_attrs(&expr.attrs);
        for stmt in &expr.body.stmts {
            self.stmt(stmt);
        }
        self.offset(-self.config.indent);
        self.scan_end();
        self.scan_string("}");
    }

    fn expr_yield(&mut self, expr: &ExprYield) {
        self.outer_attrs(&expr.attrs);
        self.scan_string("yield");
        if let Some(value) = &expr.expr {
            self.nbsp();
            self.expr(value);
        }
    }

    fn label(&mut self, label: &Label) {
        self.lifetime(&label.name);
        self.scan_string(": ");
    }

    fn field_value(&mut self, field_value: &FieldValue) {
        self.outer_attrs(&field_value.attrs);
        self.member(&field_value.member);
        if field_value.colon_token.is_some() {
            self.scan_string(": ");
            self.scan_begin_inconsistent(0);
            self.expr(&field_value.expr);
            self.scan_end();
        }
    }

    fn arm(&mut self, arm: &Arm) {
        self.outer_attrs(&arm.attrs);
        self.scan_begin_inconsistent(0);
        self.pat(&arm.pat);
        if let Some((_if_token, guard)) = &arm.guard {
            self.scan_string(" if ");
            self.expr(guard);
        }
        self.scan_string(" =>");
        let empty_block;
        let mut body = &*arm.body;
        while let Expr::Block(expr) = body {
            if expr.attrs.is_empty() && expr.label.is_none() {
                let mut stmts = expr.block.stmts.iter();
                if let (Some(Stmt::Expr(inner, None)), None) = (stmts.next(), stmts.next()) {
                    body = inner;
                    continue;
                }
            }
            break;
        }
        if let Expr::Tuple(expr) = body {
            if expr.elems.is_empty() && expr.attrs.is_empty() {
                empty_block = Expr::Block(ExprBlock {
                    attrs: Vec::new(),
                    label: None,
                    block: Block {
                        brace_token: token::Brace::default(),
                        stmts: Vec::new(),
                    },
                });
                body = &empty_block;
            }
        }
        if let Expr::Block(body) = body {
            self.nbsp();
            if let Some(label) = &body.label {
                self.label(label);
            }
            self.scan_string("{");
            self.neverbreak();
            self.scan_begin_consistent(self.config.indent);
            self.hardbreak_if_nonempty();
            self.inner_attrs(&body.attrs);
            for stmt in &body.block.stmts {
                self.stmt(stmt);
            }
            self.offset(-self.config.indent);
            self.scan_end();
            self.scan_string("}");
            self.scan_end();
        } else {
            self.nbsp();
            self.neverbreak();
            self.scan_begin_consistent(self.config.indent);
            self.scan_break(BreakToken {
                pre_break: Some('{'),
                ..BreakToken::default()
            });
            self.expr_beginning_of_line(body, true);
            self.scan_break(BreakToken {
                offset: -self.config.indent,
                pre_break: stmt::add_semi(body).then(|| ';'),
                post_break: Some('}'),
                no_break: requires_terminator(body).then(|| ','),
                ..BreakToken::default()
            });
            self.scan_end();
            self.scan_end();
        }
    }

    fn call_args(&mut self, args: &Punctuated<Expr, Token![,]>) {
        let mut iter = args.iter();
        match (iter.next(), iter.next()) {
            (Some(expr), None) if is_blocklike(expr) => {
                self.expr(expr);
            }
            _ => {
                self.scan_begin_consistent(self.config.indent);
                self.zerobreak();
                for arg in args.iter().delimited() {
                    self.expr(&arg);
                    self.trailing_comma(arg.is_last);
                }
                self.offset(-self.config.indent);
                self.scan_end();
            }
        }
    }

    pub fn small_block(&mut self, block: &Block, attrs: &[Attribute]) {
        self.scan_string("{");
        if attrs::has_inner(attrs) || !block.stmts.is_empty() {
            self.scan_space();
            self.inner_attrs(attrs);
            match block.stmts.as_slice() {
                [Stmt::Expr(expr, None)] if stmt::break_after(expr) => {
                    self.scan_begin_inconsistent(0);
                    self.expr_beginning_of_line(expr, true);
                    self.scan_end();
                    self.scan_space();
                }
                _ => {
                    for stmt in &block.stmts {
                        self.stmt(stmt);
                    }
                }
            }
            self.offset(-self.config.indent);
        }
        self.scan_string("}");
    }

    pub fn member(&mut self, member: &Member) {
        match member {
            Member::Named(ident) => self.ident(ident),
            Member::Unnamed(index) => self.index(index),
        }
    }

    fn index(&mut self, member: &Index) {
        self.scan_string(member.index.to_string());
    }

    fn binary_operator(&mut self, op: &BinOp) {
        self.scan_string(
            match op {
                #![cfg_attr(all(test, exhaustive), deny(non_exhaustive_omitted_patterns))]
                BinOp::Add(_) => "+",
                BinOp::Sub(_) => "-",
                BinOp::Mul(_) => "*",
                BinOp::Div(_) => "/",
                BinOp::Rem(_) => "%",
                BinOp::And(_) => "&&",
                BinOp::Or(_) => "||",
                BinOp::BitXor(_) => "^",
                BinOp::BitAnd(_) => "&",
                BinOp::BitOr(_) => "|",
                BinOp::Shl(_) => "<<",
                BinOp::Shr(_) => ">>",
                BinOp::Eq(_) => "==",
                BinOp::Lt(_) => "<",
                BinOp::Le(_) => "<=",
                BinOp::Ne(_) => "!=",
                BinOp::Ge(_) => ">=",
                BinOp::Gt(_) => ">",
                BinOp::AddAssign(_) => "+=",
                BinOp::SubAssign(_) => "-=",
                BinOp::MulAssign(_) => "*=",
                BinOp::DivAssign(_) => "/=",
                BinOp::RemAssign(_) => "%=",
                BinOp::BitXorAssign(_) => "^=",
                BinOp::BitAndAssign(_) => "&=",
                BinOp::BitOrAssign(_) => "|=",
                BinOp::ShlAssign(_) => "<<=",
                BinOp::ShrAssign(_) => ">>=",
                _ => unimplemented!("unknown BinOp"),
            },
        );
    }

    fn unary_operator(&mut self, op: &UnOp) {
        self.scan_string(
            match op {
                #![cfg_attr(all(test, exhaustive), deny(non_exhaustive_omitted_patterns))]
                UnOp::Deref(_) => "*",
                UnOp::Not(_) => "!",
                UnOp::Neg(_) => "-",
                _ => unimplemented!("unknown UnOp"),
            },
        );
    }

    fn zerobreak_unless_short_ident(&mut self, beginning_of_line: bool, expr: &Expr) {
        if beginning_of_line && self.is_short_ident(expr) {
            return;
        }
        self.zerobreak();
    }

    fn is_short_ident(&self, expr: &Expr) -> bool {
        if let Expr::Path(expr) = expr {
            return expr.attrs.is_empty()
                && expr.qself.is_none()
                && expr.path.get_ident().map_or(false, |ident| {
                    ident.to_string().len() as isize <= self.config.indent
                });
        }
        false
    }
}

fn requires_terminator(expr: &Expr) -> bool {
    // see https://github.com/rust-lang/rust/blob/a266f1199/compiler/rustc_ast/src/util/classify.rs#L7-L26
    match expr {
        #![cfg_attr(all(test, exhaustive), deny(non_exhaustive_omitted_patterns))]
        Expr::If(_)
        | Expr::Match(_)
        | Expr::Block(_) | Expr::Unsafe(_) // both under ExprKind::Block in rustc
        | Expr::While(_)
        | Expr::Loop(_)
        | Expr::ForLoop(_)
        | Expr::TryBlock(_)
        | Expr::Const(_) => false,

        Expr::Array(_)
        | Expr::Assign(_)
        | Expr::Async(_)
        | Expr::Await(_)
        | Expr::Binary(_)
        | Expr::Break(_)
        | Expr::Call(_)
        | Expr::Cast(_)
        | Expr::Closure(_)
        | Expr::Continue(_)
        | Expr::Field(_)
        | Expr::Group(_)
        | Expr::Index(_)
        | Expr::Infer(_)
        | Expr::Let(_)
        | Expr::Lit(_)
        | Expr::Macro(_)
        | Expr::MethodCall(_)
        | Expr::Paren(_)
        | Expr::Path(_)
        | Expr::Range(_)
        | Expr::Reference(_)
        | Expr::Repeat(_)
        | Expr::Return(_)
        | Expr::Struct(_)
        | Expr::Try(_)
        | Expr::Tuple(_)
        | Expr::Unary(_)
        | Expr::Verbatim(_)
        | Expr::Yield(_) => true,

        _ => true,
    }
}

// Expressions that syntactically contain an "exterior" struct literal i.e. not
// surrounded by any parens or other delimiters. For example `X { y: 1 }`, `X {
// y: 1 }.method()`, `foo == X { y: 1 }` and `X { y: 1 } == foo` all do, but `(X
// { y: 1 }) == foo` does not.
fn contains_exterior_struct_lit(expr: &Expr) -> bool {
    match expr {
        #![cfg_attr(all(test, exhaustive), deny(non_exhaustive_omitted_patterns))]
        Expr::Struct(_) => true,

        Expr::Assign(ExprAssign { left, right, .. })
        | Expr::Binary(ExprBinary { left, right, .. }) => {
            // X { y: 1 } + X { y: 2 }
            contains_exterior_struct_lit(left) || contains_exterior_struct_lit(right)
        }

        Expr::Await(ExprAwait { base: e, .. })
        | Expr::Cast(ExprCast { expr: e, .. })
        | Expr::Field(ExprField { base: e, .. })
        | Expr::Group(ExprGroup { expr: e, .. })
        | Expr::Index(ExprIndex { expr: e, .. })
        | Expr::MethodCall(ExprMethodCall { receiver: e, .. })
        | Expr::Reference(ExprReference { expr: e, .. })
        | Expr::Unary(ExprUnary { expr: e, .. }) => {
            // &X { y: 1 }, X { y: 1 }.y
            contains_exterior_struct_lit(e)
        }

        Expr::Array(_)
        | Expr::Async(_)
        | Expr::Block(_)
        | Expr::Break(_)
        | Expr::Call(_)
        | Expr::Closure(_)
        | Expr::Const(_)
        | Expr::Continue(_)
        | Expr::ForLoop(_)
        | Expr::If(_)
        | Expr::Infer(_)
        | Expr::Let(_)
        | Expr::Lit(_)
        | Expr::Loop(_)
        | Expr::Macro(_)
        | Expr::Match(_)
        | Expr::Paren(_)
        | Expr::Path(_)
        | Expr::Range(_)
        | Expr::Repeat(_)
        | Expr::Return(_)
        | Expr::Try(_)
        | Expr::TryBlock(_)
        | Expr::Tuple(_)
        | Expr::Unsafe(_)
        | Expr::Verbatim(_)
        | Expr::While(_)
        | Expr::Yield(_) => false,

        _ => false,
    }
}

fn needs_newline_if_wrap(expr: &Expr) -> bool {
    match expr {
        #![cfg_attr(all(test, exhaustive), deny(non_exhaustive_omitted_patterns))]
        Expr::Array(_)
        | Expr::Async(_)
        | Expr::Block(_)
        | Expr::Break(ExprBreak { expr: None, .. })
        | Expr::Closure(_)
        | Expr::Const(_)
        | Expr::Continue(_)
        | Expr::ForLoop(_)
        | Expr::If(_)
        | Expr::Infer(_)
        | Expr::Lit(_)
        | Expr::Loop(_)
        | Expr::Macro(_)
        | Expr::Match(_)
        | Expr::Path(_)
        | Expr::Range(ExprRange { end: None, .. })
        | Expr::Repeat(_)
        | Expr::Return(ExprReturn { expr: None, .. })
        | Expr::Struct(_)
        | Expr::TryBlock(_)
        | Expr::Tuple(_)
        | Expr::Unsafe(_)
        | Expr::Verbatim(_)
        | Expr::While(_)
        | Expr::Yield(ExprYield { expr: None, .. }) => false,

        Expr::Assign(_)
        | Expr::Await(_)
        | Expr::Binary(_)
        | Expr::Cast(_)
        | Expr::Field(_)
        | Expr::Index(_)
        | Expr::MethodCall(_) => true,

        Expr::Break(ExprBreak { expr: Some(e), .. })
        | Expr::Call(ExprCall { func: e, .. })
        | Expr::Group(ExprGroup { expr: e, .. })
        | Expr::Let(ExprLet { expr: e, .. })
        | Expr::Paren(ExprParen { expr: e, .. })
        | Expr::Range(ExprRange { end: Some(e), .. })
        | Expr::Reference(ExprReference { expr: e, .. })
        | Expr::Return(ExprReturn { expr: Some(e), .. })
        | Expr::Try(ExprTry { expr: e, .. })
        | Expr::Unary(ExprUnary { expr: e, .. })
        | Expr::Yield(ExprYield { expr: Some(e), .. }) => needs_newline_if_wrap(e),

        _ => false,
    }
}

fn is_blocklike(expr: &Expr) -> bool {
    match expr {
        #![cfg_attr(all(test, exhaustive), deny(non_exhaustive_omitted_patterns))]
        Expr::Array(ExprArray { attrs, .. })
        | Expr::Async(ExprAsync { attrs, .. })
        | Expr::Block(ExprBlock { attrs, .. })
        | Expr::Closure(ExprClosure { attrs, .. })
        | Expr::Const(ExprConst { attrs, .. })
        | Expr::Struct(ExprStruct { attrs, .. })
        | Expr::TryBlock(ExprTryBlock { attrs, .. })
        | Expr::Tuple(ExprTuple { attrs, .. })
        | Expr::Unsafe(ExprUnsafe { attrs, .. }) => !attrs::has_outer(attrs),

        Expr::Assign(_)
        | Expr::Await(_)
        | Expr::Binary(_)
        | Expr::Break(_)
        | Expr::Call(_)
        | Expr::Cast(_)
        | Expr::Continue(_)
        | Expr::Field(_)
        | Expr::ForLoop(_)
        | Expr::Group(_)
        | Expr::If(_)
        | Expr::Index(_)
        | Expr::Infer(_)
        | Expr::Let(_)
        | Expr::Lit(_)
        | Expr::Loop(_)
        | Expr::Macro(_)
        | Expr::Match(_)
        | Expr::MethodCall(_)
        | Expr::Paren(_)
        | Expr::Path(_)
        | Expr::Range(_)
        | Expr::Reference(_)
        | Expr::Repeat(_)
        | Expr::Return(_)
        | Expr::Try(_)
        | Expr::Unary(_)
        | Expr::Verbatim(_)
        | Expr::While(_)
        | Expr::Yield(_) => false,

        _ => false,
    }
}

// Expressions for which `$expr` and `{ $expr }` mean the same thing.
//
// This is not the case for all expressions. For example `{} | x | x` has some
// bitwise OR operators while `{ {} |x| x }` has a block followed by a closure.
fn parseable_as_stmt(expr: &Expr) -> bool {
    match expr {
        #![cfg_attr(all(test, exhaustive), deny(non_exhaustive_omitted_patterns))]
        Expr::Array(_)
        | Expr::Async(_)
        | Expr::Block(_)
        | Expr::Break(_)
        | Expr::Closure(_)
        | Expr::Const(_)
        | Expr::Continue(_)
        | Expr::ForLoop(_)
        | Expr::If(_)
        | Expr::Infer(_)
        | Expr::Let(_)
        | Expr::Lit(_)
        | Expr::Loop(_)
        | Expr::Macro(_)
        | Expr::Match(_)
        | Expr::Paren(_)
        | Expr::Path(_)
        | Expr::Reference(_)
        | Expr::Repeat(_)
        | Expr::Return(_)
        | Expr::Struct(_)
        | Expr::TryBlock(_)
        | Expr::Tuple(_)
        | Expr::Unary(_)
        | Expr::Unsafe(_)
        | Expr::Verbatim(_)
        | Expr::While(_)
        | Expr::Yield(_) => true,

        Expr::Assign(expr) => parseable_as_stmt(&expr.left),
        Expr::Await(expr) => parseable_as_stmt(&expr.base),
        Expr::Binary(expr) => requires_terminator(&expr.left) && parseable_as_stmt(&expr.left),
        Expr::Call(expr) => requires_terminator(&expr.func) && parseable_as_stmt(&expr.func),
        Expr::Cast(expr) => requires_terminator(&expr.expr) && parseable_as_stmt(&expr.expr),
        Expr::Field(expr) => parseable_as_stmt(&expr.base),
        Expr::Group(expr) => parseable_as_stmt(&expr.expr),
        Expr::Index(expr) => requires_terminator(&expr.expr) && parseable_as_stmt(&expr.expr),
        Expr::MethodCall(expr) => parseable_as_stmt(&expr.receiver),
        Expr::Range(expr) => match &expr.start {
            None => true,
            Some(start) => requires_terminator(start) && parseable_as_stmt(start),
        },
        Expr::Try(expr) => parseable_as_stmt(&expr.expr),

        _ => false,
    }
}
