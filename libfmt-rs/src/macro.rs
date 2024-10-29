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

        // Scan the macro name and path e.g. `foo` or `foo::bar`
        self.scan_path(&mac.path, PathKind::Simple);

        // Scan the macro ! symbol
        self.scan_string("!");

        // Scan the macro ident if it exists
        if let Some(ident) = ident {
            self.scan_space();
            self.ident(ident);
        }

        // Compute open, close delimiters and break function
        let (open_delimiter, close_delimiter, delimiter_break) = match mac.delimiter {
            MacroDelimiter::Paren(_) => ("(", ")", Self::scan_break_zero as fn(&mut Self)),
            MacroDelimiter::Brace(_) => (" {", "}", Self::scan_break_newline as fn(&mut Self)),
            MacroDelimiter::Bracket(_) => ("[", "]", Self::scan_break_zero as fn(&mut Self)),
        };

        // Scan the open delimiter
        self.scan_string(open_delimiter);

        // Scan the macro body tokens
        if !mac.tokens.is_empty() {
            self.smart_wrap_begin_default();

            delimiter_break(self);
            self.smart_wrap_no_begin_zero();
            self.macro_rules_tokens(mac.tokens.clone(), false);
            self.smart_wrap_no_end();
            delimiter_break(self);

            // Reset offset back to macro root level
            self.update_break_offset(-self.config.indent);
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
        self.scan_begin_vertical(self.config.indent);
        self.scan_break_newline_if_nonempty();
        let mut state = State::Start;
        for tt in rules.clone() {
            let token = Token::from(tt);
            match (state, token) {
                (Start, Token::Group(delimiter, stream)) => {
                    self.delimiter_open(delimiter);
                    if !stream.is_empty() {
                        self.scan_begin_vertical(self.config.indent);
                        self.scan_break_zero();
                        self.scan_begin_horizontal(0);
                        self.macro_rules_tokens(stream, true);
                        self.scan_end();
                        self.scan_break_zero();
                        self.update_break_offset(-self.config.indent);
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
                    self.scan_break_never();
                    if !stream.is_empty() {
                        self.scan_begin_vertical(self.config.indent);
                        self.scan_break_newline();
                        self.scan_begin_horizontal(0);
                        self.macro_rules_tokens(stream, false);
                        self.scan_end();
                        self.scan_break_newline();
                        self.update_break_offset(-self.config.indent);
                        self.scan_end();
                    }
                    self.scan_string("}");
                    state = Expander;
                }
                (Expander, Token::Punct(';', Spacing::Alone)) => {
                    self.scan_string(";");
                    self.scan_break_newline();
                    state = Start;
                }
                _ => unimplemented!("bad macro_rules syntax"),
            }
        }
        match state {
            Start => {}
            Expander => {
                self.scan_string(";");
                self.scan_break_newline();
            }
            _ => self.scan_break_newline(),
        }
        self.update_break_offset(-self.config.indent);
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
            Ampersand,
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
                    // By checking the Ampersand state here we can avoid a trailing space
                    // Feature F0003: Drop ampersand tailing space
                    (state != Dot && state != Colon2 && state != Ampersand, Ident)
                }
                (_, Token::Literal(_)) => (state != Dot, Ident),
                (_, Token::Punct(',' | ';', _)) => (false, Other),
                (_, Token::Punct('.', _)) if !matcher => (state != Ident && state != Delim, Dot),
                (_, Token::Punct(':', Spacing::Joint)) => (state != Ident, Colon),
                (_, Token::Punct('$', _)) => (true, Dollar),
                (_, Token::Punct('#', _)) => (true, Pound),

                // Keeping the preceeding space as per original functionality but now tracking the
                // Ampersand state so that we can use it to determine if the next value which is an
                // Ident needs its own preceeding space; which it does not.
                // Feature F0003: Drop ampersand tailing space
                (_, Token::Punct('&', _)) => (true, Ampersand),

                (_, _) => (true, Other),
            };
            if !previous_is_joint {
                if needs_space {
                    self.scan_break_space();
                } else if let Token::Punct('.', _) = token {
                    self.scan_break_zero();
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
