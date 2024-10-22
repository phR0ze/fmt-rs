use crate::engine::Engine;
use crate::path::PathKind;
use crate::token::Token;
use proc_macro2::{Delimiter, Spacing, TokenStream};
use syn::{Ident, Macro, MacroDelimiter};

impl Engine {
    /// Format a macro invocation or definition.
    pub fn scan_mac(&mut self, mac: &Macro, ident: Option<&Ident>, semicolon: bool) {
        if mac.path.is_ident("macro_rules") {
            if let Some(ident) = ident {
                self.macro_rules(ident, &mac.tokens);
                return;
            }
        }
        #[cfg(feature = "verbatim")]
        if ident.is_none() && self.standard_library_macro(mac, semicolon) {
            return;
        }

        // Scan the macro name and path e.g. `foo` or `foo::bar`
        self.scan_path(&mac.path, PathKind::Simple);

        // Scan the macro ! symbol
        self.scan_string("!");

        // Scan the macro ident if it exists
        if let Some(ident) = ident {
            self.nbsp();
            self.ident(ident);
        }

        // Compute open, close delimiters and break function
        let (open_delimiter, close_delimiter, delimiter_break) = match mac.delimiter {
            MacroDelimiter::Paren(_) => ("(", ")", Self::zerobreak as fn(&mut Self)),
            MacroDelimiter::Brace(_) => (" {", "}", Self::scan_hardbreak as fn(&mut Self)),
            MacroDelimiter::Bracket(_) => ("[", "]", Self::zerobreak as fn(&mut Self)),
        };

        // Scan the open delimiter
        self.scan_string(open_delimiter);

        // Scan the macro body tokens
        if !mac.tokens.is_empty() {
            self.scan_begin_consistent(self.config.indent);
            delimiter_break(self);

            // Scan the macro body
            self.scan_begin_inconsistent(0);
            self.macro_rules_tokens(mac.tokens.clone(), false);
            self.scan_end();

            // libfmt: control brace_style for macro
            // if !INVOKATION_BRACE_STYLE_SAME_LINE {
            delimiter_break(self);
            // }

            // Reset offset back to macro root level
            self.offset(-self.config.indent);
            self.scan_end();
        }

        // Scan the macro close delimiter
        self.scan_string(close_delimiter);

        if semicolon {
            match mac.delimiter {
                MacroDelimiter::Paren(_) | MacroDelimiter::Bracket(_) => self.scan_string(";"),
                MacroDelimiter::Brace(_) => {}
            }
        }
    }

    fn macro_rules(&mut self, name: &Ident, rules: &TokenStream) {
        enum State {
            Start,
            Matcher,
            Equal,
            Greater,
            Expander,
        }

        use State::*;

        self.scan_string("macro_rules! ");
        self.ident(name);
        self.scan_string(" {");
        self.scan_begin_consistent(self.config.indent);
        self.hardbreak_if_nonempty();
        let mut state = State::Start;
        for tt in rules.clone() {
            let token = Token::from(tt);
            match (state, token) {
                (Start, Token::Group(delimiter, stream)) => {
                    self.delimiter_open(delimiter);
                    if !stream.is_empty() {
                        self.scan_begin_consistent(self.config.indent);
                        self.zerobreak();
                        self.scan_begin_inconsistent(0);
                        self.macro_rules_tokens(stream, true);
                        self.scan_end();
                        self.zerobreak();
                        self.offset(-self.config.indent);
                        self.scan_end();
                    }
                    self.delimiter_close(delimiter);
                    state = Matcher;
                }
                (Matcher, Token::Punct('=', Spacing::Joint)) => {
                    self.scan_string(" =");
                    state = Equal;
                }
                (Equal, Token::Punct('>', Spacing::Alone)) => {
                    self.scan_string(">");
                    state = Greater;
                }
                (Greater, Token::Group(_delimiter, stream)) => {
                    self.scan_string(" {");
                    self.neverbreak();
                    if !stream.is_empty() {
                        self.scan_begin_consistent(self.config.indent);
                        self.scan_hardbreak();
                        self.scan_begin_inconsistent(0);
                        self.macro_rules_tokens(stream, false);
                        self.scan_end();
                        self.scan_hardbreak();
                        self.offset(-self.config.indent);
                        self.scan_end();
                    }
                    self.scan_string("}");
                    state = Expander;
                }
                (Expander, Token::Punct(';', Spacing::Alone)) => {
                    self.scan_string(";");
                    self.scan_hardbreak();
                    state = Start;
                }
                _ => unimplemented!("bad macro_rules syntax"),
            }
        }
        match state {
            Start => {}
            Expander => {
                self.scan_string(";");
                self.scan_hardbreak();
            }
            _ => self.scan_hardbreak(),
        }
        self.offset(-self.config.indent);
        self.scan_end();
        self.scan_string("}");
    }

    pub fn macro_rules_tokens(&mut self, stream: TokenStream, matcher: bool) {
        #[derive(PartialEq)]
        enum State {
            Start,
            Dollar,
            DollarIdent,
            DollarIdentColon,
            DollarParen,
            DollarParenSep,
            Pound,
            PoundBang,
            Dot,
            Colon,
            Colon2,
            Ident,
            IdentBang,
            Delim,
            Other,
        }

        use State::*;

        let mut state = Start;
        let mut previous_is_joint = true;
        for tt in stream {
            let token = Token::from(tt);
            let (needs_space, next_state) = match (&state, &token) {
                (Dollar, Token::Ident(_)) => (false, if matcher { DollarIdent } else { Other }),
                (DollarIdent, Token::Punct(':', Spacing::Alone)) => (false, DollarIdentColon),
                (DollarIdentColon, Token::Ident(_)) => (false, Other),
                (DollarParen, Token::Punct('+' | '*' | '?', Spacing::Alone)) => (false, Other),
                (DollarParen, Token::Ident(_) | Token::Literal(_)) => (false, DollarParenSep),
                (DollarParen, Token::Punct(_, Spacing::Joint)) => (false, DollarParen),
                (DollarParen, Token::Punct(_, Spacing::Alone)) => (false, DollarParenSep),
                (DollarParenSep, Token::Punct('+' | '*', _)) => (false, Other),
                (Pound, Token::Punct('!', _)) => (false, PoundBang),
                (Dollar, Token::Group(Delimiter::Parenthesis, _)) => (false, DollarParen),
                (Pound | PoundBang, Token::Group(Delimiter::Bracket, _)) => (false, Other),
                (Ident, Token::Group(Delimiter::Parenthesis | Delimiter::Bracket, _)) => {
                    (false, Delim)
                }
                (Ident, Token::Punct('!', Spacing::Alone)) => (false, IdentBang),
                (IdentBang, Token::Group(Delimiter::Parenthesis | Delimiter::Bracket, _)) => {
                    (false, Other)
                }
                (Colon, Token::Punct(':', _)) => (false, Colon2),
                (_, Token::Group(Delimiter::Parenthesis | Delimiter::Bracket, _)) => (true, Delim),
                (_, Token::Group(Delimiter::Brace | Delimiter::None, _)) => (true, Other),
                (_, Token::Ident(ident)) if !is_keyword(ident) => {
                    (state != Dot && state != Colon2, Ident)
                }
                (_, Token::Literal(_)) => (state != Dot, Ident),
                (_, Token::Punct(',' | ';', _)) => (false, Other),
                (_, Token::Punct('.', _)) if !matcher => (state != Ident && state != Delim, Dot),
                (_, Token::Punct(':', Spacing::Joint)) => (state != Ident, Colon),
                (_, Token::Punct('$', _)) => (true, Dollar),
                (_, Token::Punct('#', _)) => (true, Pound),
                (_, _) => (true, Other),
            };

            if !previous_is_joint {
                if needs_space {
                    self.scan_space();
                } else if let Token::Punct('.', _) = token {
                    self.zerobreak();
                }
            }
            previous_is_joint = match token {
                Token::Punct(_, Spacing::Joint) | Token::Punct('$', _) => true,
                _ => false,
            };
            self.single_token(
                token,
                if matcher {
                    |printer, stream| printer.macro_rules_tokens(stream, true)
                } else {
                    |printer, stream| printer.macro_rules_tokens(stream, false)
                },
            );
            state = next_state;
        }
    }
}

fn is_keyword(ident: &Ident) -> bool {
    match ident.to_string().as_str() {
        "as" | "async" | "await" | "box" | "break" | "const" | "continue" | "crate" | "dyn"
        | "else" | "enum" | "extern" | "fn" | "for" | "if" | "impl" | "in" | "let" | "loop"
        | "macro" | "match" | "mod" | "move" | "mut" | "pub" | "ref" | "return" | "static"
        | "struct" | "trait" | "type" | "unsafe" | "use" | "where" | "while" | "yield" => true,
        _ => false,
    }
}

#[cfg(feature = "verbatim")]
mod standard_library {
    use crate::engine::Engine;
    use crate::iter::IterDelimited;
    use crate::path::PathKind;
    use syn::ext::IdentExt;
    use syn::parse::{Parse, ParseStream, Parser, Result};
    use syn::{
        parenthesized, token, Attribute, Expr, ExprAssign, ExprPath, Ident, Lit, Macro, Pat, Path,
        Token, Type, Visibility,
    };

    enum KnownMacro {
        Expr(Expr),
        Exprs(Vec<Expr>),
        Cfg(Cfg),
        Matches(Matches),
        ThreadLocal(Vec<ThreadLocal>),
        VecArray(Vec<Expr>),
        VecRepeat { elem: Expr, n: Expr },
    }

    enum Cfg {
        Eq(Ident, Option<Lit>),
        Call(Ident, Vec<Cfg>),
    }

    struct Matches {
        expression: Expr,
        pattern: Pat,
        guard: Option<Expr>,
    }

    struct ThreadLocal {
        attrs: Vec<Attribute>,
        vis: Visibility,
        name: Ident,
        ty: Type,
        init: Expr,
    }

    struct FormatArgs {
        format_string: Expr,
        args: Vec<Expr>,
    }

    impl Parse for FormatArgs {
        fn parse(input: ParseStream) -> Result<Self> {
            let format_string: Expr = input.parse()?;

            let mut args = Vec::new();
            while !input.is_empty() {
                input.parse::<Token![,]>()?;
                if input.is_empty() {
                    break;
                }
                let arg = if input.peek(Ident::peek_any)
                    && input.peek2(Token![=])
                    && !input.peek2(Token![==])
                {
                    let key = input.call(Ident::parse_any)?;
                    let eq_token: Token![=] = input.parse()?;
                    let value: Expr = input.parse()?;
                    Expr::Assign(ExprAssign {
                        attrs: Vec::new(),
                        left: Box::new(Expr::Path(ExprPath {
                            attrs: Vec::new(),
                            qself: None,
                            path: Path::from(key),
                        })),
                        eq_token,
                        right: Box::new(value),
                    })
                } else {
                    input.parse()?
                };
                args.push(arg);
            }

            Ok(FormatArgs {
                format_string,
                args,
            })
        }
    }

    impl KnownMacro {
        fn parse_expr(input: ParseStream) -> Result<Self> {
            let expr: Expr = input.parse()?;
            Ok(KnownMacro::Expr(expr))
        }

        fn parse_expr_comma(input: ParseStream) -> Result<Self> {
            let expr: Expr = input.parse()?;
            input.parse::<Option<Token![,]>>()?;
            Ok(KnownMacro::Exprs(vec![expr]))
        }

        fn parse_exprs(input: ParseStream) -> Result<Self> {
            let exprs = input.parse_terminated(Expr::parse, Token![,])?;
            Ok(KnownMacro::Exprs(Vec::from_iter(exprs)))
        }

        fn parse_assert(input: ParseStream) -> Result<Self> {
            let mut exprs = Vec::new();
            let cond: Expr = input.parse()?;
            exprs.push(cond);
            if input.parse::<Option<Token![,]>>()?.is_some() && !input.is_empty() {
                let format_args: FormatArgs = input.parse()?;
                exprs.push(format_args.format_string);
                exprs.extend(format_args.args);
            }
            Ok(KnownMacro::Exprs(exprs))
        }

        fn parse_assert_cmp(input: ParseStream) -> Result<Self> {
            let mut exprs = Vec::new();
            let left: Expr = input.parse()?;
            exprs.push(left);
            input.parse::<Token![,]>()?;
            let right: Expr = input.parse()?;
            exprs.push(right);
            if input.parse::<Option<Token![,]>>()?.is_some() && !input.is_empty() {
                let format_args: FormatArgs = input.parse()?;
                exprs.push(format_args.format_string);
                exprs.extend(format_args.args);
            }
            Ok(KnownMacro::Exprs(exprs))
        }

        fn parse_cfg(input: ParseStream) -> Result<Self> {
            fn parse_single(input: ParseStream) -> Result<Cfg> {
                let ident: Ident = input.parse()?;
                if input.peek(token::Paren) && (ident == "all" || ident == "any") {
                    let content;
                    parenthesized!(content in input);
                    let list = content.call(parse_multiple)?;
                    Ok(Cfg::Call(ident, list))
                } else if input.peek(token::Paren) && ident == "not" {
                    let content;
                    parenthesized!(content in input);
                    let cfg = content.call(parse_single)?;
                    content.parse::<Option<Token![,]>>()?;
                    Ok(Cfg::Call(ident, vec![cfg]))
                } else if input.peek(Token![=]) {
                    input.parse::<Token![=]>()?;
                    let string: Lit = input.parse()?;
                    Ok(Cfg::Eq(ident, Some(string)))
                } else {
                    Ok(Cfg::Eq(ident, None))
                }
            }

            fn parse_multiple(input: ParseStream) -> Result<Vec<Cfg>> {
                let mut vec = Vec::new();
                while !input.is_empty() {
                    let cfg = input.call(parse_single)?;
                    vec.push(cfg);
                    if input.is_empty() {
                        break;
                    }
                    input.parse::<Token![,]>()?;
                }
                Ok(vec)
            }

            let cfg = input.call(parse_single)?;
            input.parse::<Option<Token![,]>>()?;
            Ok(KnownMacro::Cfg(cfg))
        }

        fn parse_env(input: ParseStream) -> Result<Self> {
            let mut exprs = Vec::new();
            let name: Expr = input.parse()?;
            exprs.push(name);
            if input.parse::<Option<Token![,]>>()?.is_some() && !input.is_empty() {
                let error_msg: Expr = input.parse()?;
                exprs.push(error_msg);
                input.parse::<Option<Token![,]>>()?;
            }
            Ok(KnownMacro::Exprs(exprs))
        }

        fn parse_format_args(input: ParseStream) -> Result<Self> {
            let format_args: FormatArgs = input.parse()?;
            let mut exprs = format_args.args;
            exprs.insert(0, format_args.format_string);
            Ok(KnownMacro::Exprs(exprs))
        }

        fn parse_matches(input: ParseStream) -> Result<Self> {
            let expression: Expr = input.parse()?;
            input.parse::<Token![,]>()?;
            let pattern = input.call(Pat::parse_multi_with_leading_vert)?;
            let guard = if input.parse::<Option<Token![if]>>()?.is_some() {
                Some(input.parse()?)
            } else {
                None
            };
            input.parse::<Option<Token![,]>>()?;
            Ok(KnownMacro::Matches(Matches {
                expression,
                pattern,
                guard,
            }))
        }

        fn parse_thread_local(input: ParseStream) -> Result<Self> {
            let mut items = Vec::new();
            while !input.is_empty() {
                let attrs = input.call(Attribute::parse_outer)?;
                let vis: Visibility = input.parse()?;
                input.parse::<Token![static]>()?;
                let name: Ident = input.parse()?;
                input.parse::<Token![:]>()?;
                let ty: Type = input.parse()?;
                input.parse::<Token![=]>()?;
                let init: Expr = input.parse()?;
                if input.is_empty() {
                    break;
                }
                input.parse::<Token![;]>()?;
                items.push(ThreadLocal {
                    attrs,
                    vis,
                    name,
                    ty,
                    init,
                });
            }
            Ok(KnownMacro::ThreadLocal(items))
        }

        fn parse_vec(input: ParseStream) -> Result<Self> {
            if input.is_empty() {
                return Ok(KnownMacro::VecArray(Vec::new()));
            }
            let first: Expr = input.parse()?;
            if input.parse::<Option<Token![;]>>()?.is_some() {
                let len: Expr = input.parse()?;
                Ok(KnownMacro::VecRepeat {
                    elem: first,
                    n: len,
                })
            } else {
                let mut vec = vec![first];
                while !input.is_empty() {
                    input.parse::<Token![,]>()?;
                    if input.is_empty() {
                        break;
                    }
                    let next: Expr = input.parse()?;
                    vec.push(next);
                }
                Ok(KnownMacro::VecArray(vec))
            }
        }

        fn parse_write(input: ParseStream) -> Result<Self> {
            let mut exprs = Vec::new();
            let dst: Expr = input.parse()?;
            exprs.push(dst);
            input.parse::<Token![,]>()?;
            let format_args: FormatArgs = input.parse()?;
            exprs.push(format_args.format_string);
            exprs.extend(format_args.args);
            Ok(KnownMacro::Exprs(exprs))
        }

        fn parse_writeln(input: ParseStream) -> Result<Self> {
            let mut exprs = Vec::new();
            let dst: Expr = input.parse()?;
            exprs.push(dst);
            if input.parse::<Option<Token![,]>>()?.is_some() && !input.is_empty() {
                let format_args: FormatArgs = input.parse()?;
                exprs.push(format_args.format_string);
                exprs.extend(format_args.args);
            }
            Ok(KnownMacro::Exprs(exprs))
        }
    }

    impl Engine {
        pub fn standard_library_macro(&mut self, mac: &Macro, mut semicolon: bool) -> bool {
            let name = mac.path.segments.last().unwrap().ident.to_string();
            let parser = match name.as_str() {
                "addr_of" | "addr_of_mut" => KnownMacro::parse_expr,
                "assert" | "debug_assert" => KnownMacro::parse_assert,
                "assert_eq" | "assert_ne" | "debug_assert_eq" | "debug_assert_ne" => {
                    KnownMacro::parse_assert_cmp
                }
                "cfg" => KnownMacro::parse_cfg,
                "compile_error" | "include" | "include_bytes" | "include_str" | "option_env" => {
                    KnownMacro::parse_expr_comma
                }
                "concat" | "concat_bytes" | "dbg" => KnownMacro::parse_exprs,
                "const_format_args" | "eprint" | "eprintln" | "format" | "format_args"
                | "format_args_nl" | "panic" | "print" | "println" | "todo" | "unimplemented"
                | "unreachable" => KnownMacro::parse_format_args,
                "env" => KnownMacro::parse_env,
                "matches" => KnownMacro::parse_matches,
                "thread_local" => KnownMacro::parse_thread_local,
                "vec" => KnownMacro::parse_vec,
                "write" => KnownMacro::parse_write,
                "writeln" => KnownMacro::parse_writeln,
                _ => return false,
            };

            let known_macro = match parser.parse2(mac.tokens.clone()) {
                Ok(known_macro) => known_macro,
                Err(_) => return false,
            };

            self.path(&mac.path, PathKind::Simple);
            self.scan_string("!");

            match &known_macro {
                KnownMacro::Expr(expr) => {
                    self.scan_string("(");
                    self.scan_begin_consistent(self.config.indent);
                    self.zerobreak();
                    self.expr(expr);
                    self.zerobreak();
                    self.offset(-self.config.indent);
                    self.scan_end();
                    self.scan_string(")");
                }
                KnownMacro::Exprs(exprs) => {
                    self.scan_string("(");
                    self.scan_begin_consistent(self.config.indent);
                    self.zerobreak();
                    for elem in exprs.iter().delimited() {
                        self.expr(&elem);
                        self.trailing_comma(elem.is_last);
                    }
                    self.offset(-self.config.indent);
                    self.scan_end();
                    self.scan_string(")");
                }
                KnownMacro::Cfg(cfg) => {
                    self.scan_string("(");
                    self.cfg(cfg);
                    self.scan_string(")");
                }
                KnownMacro::Matches(matches) => {
                    self.scan_string("(");
                    self.scan_begin_consistent(self.config.indent);
                    self.zerobreak();
                    self.expr(&matches.expression);
                    self.scan_string(",");
                    self.space();
                    self.pat(&matches.pattern);
                    if let Some(guard) = &matches.guard {
                        self.space();
                        self.scan_string("if ");
                        self.expr(guard);
                    }
                    self.zerobreak();
                    self.offset(-self.config.indent);
                    self.scan_end();
                    self.scan_string(")");
                }
                KnownMacro::ThreadLocal(items) => {
                    self.scan_string(" {");
                    self.scan_begin_consistent(self.config.indent);
                    self.hardbreak_if_nonempty();
                    for item in items {
                        self.outer_attrs(&item.attrs);
                        self.scan_begin_consistent(0);
                        self.visibility(&item.vis);
                        self.scan_string("static ");
                        self.ident(&item.name);
                        self.scan_string(": ");
                        self.ty(&item.ty);
                        self.scan_string(" = ");
                        self.neverbreak();
                        self.expr(&item.init);
                        self.scan_string(";");
                        self.scan_end();
                        self.hardbreak();
                    }
                    self.offset(-self.config.indent);
                    self.scan_end();
                    self.scan_string("}");
                    semicolon = false;
                }
                KnownMacro::VecArray(vec) => {
                    self.scan_string("[");
                    self.scan_begin_consistent(self.config.indent);
                    self.zerobreak();
                    for elem in vec.iter().delimited() {
                        self.expr(&elem);
                        self.trailing_comma(elem.is_last);
                    }
                    self.offset(-self.config.indent);
                    self.scan_end();
                    self.scan_string("]");
                }
                KnownMacro::VecRepeat { elem, n } => {
                    self.scan_string("[");
                    self.scan_begin_consistent(self.config.indent);
                    self.zerobreak();
                    self.expr(elem);
                    self.scan_string(";");
                    self.space();
                    self.expr(n);
                    self.zerobreak();
                    self.offset(-self.config.indent);
                    self.scan_end();
                    self.scan_string("]");
                }
            }

            if semicolon {
                self.scan_string(";");
            }

            true
        }

        fn cfg(&mut self, cfg: &Cfg) {
            match cfg {
                Cfg::Eq(ident, value) => {
                    self.ident(ident);
                    if let Some(value) = value {
                        self.scan_string(" = ");
                        self.lit(value);
                    }
                }
                Cfg::Call(ident, args) => {
                    self.ident(ident);
                    self.scan_string("(");
                    self.scan_begin_consistent(self.config.indent);
                    self.zerobreak();
                    for arg in args.iter().delimited() {
                        self.cfg(&arg);
                        self.trailing_comma(arg.is_last);
                    }
                    self.offset(-self.config.indent);
                    self.scan_end();
                    self.scan_string(")");
                }
            }
        }
    }
}
