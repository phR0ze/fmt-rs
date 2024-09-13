use crate::engine::Engine;
use crate::INDENT;
use syn::{BinOp, Expr, Stmt};

impl Engine {
    pub fn stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Local(local) => {
                self.outer_attrs(&local.attrs);
                self.scan_begin_iconsistent(0);
                self.scan_string("let ");
                self.pat(&local.pat);
                if let Some(local_init) = &local.init {
                    self.scan_string(" = ");
                    self.neverbreak();
                    self.expr(&local_init.expr);
                    if let Some((_else, diverge)) = &local_init.diverge {
                        self.space();
                        self.scan_string("else ");
                        self.scan_end();
                        self.neverbreak();
                        if let Expr::Block(expr) = diverge.as_ref() {
                            self.scan_begin_consistent(INDENT);
                            self.small_block(&expr.block, &[]);
                            self.scan_end();
                        } else {
                            self.scan_string("{");
                            self.space();
                            self.scan_begin_iconsistent(INDENT);
                            self.expr(diverge);
                            self.scan_end();
                            self.space();
                            self.offset(-INDENT);
                            self.scan_string("}");
                        }
                    } else {
                        self.scan_end();
                    }
                } else {
                    self.scan_end();
                }
                self.scan_string(";");
                self.hardbreak();
            }
            Stmt::Item(item) => self.item(item),
            Stmt::Expr(expr, None) => {
                if break_after(expr) {
                    self.scan_begin_iconsistent(0);
                    self.expr_beginning_of_line(expr, true);
                    if add_semi(expr) {
                        self.scan_string(";");
                    }
                    self.scan_end();
                    self.hardbreak();
                } else {
                    self.expr_beginning_of_line(expr, true);
                }
            }
            Stmt::Expr(expr, Some(_semi)) => {
                if let Expr::Verbatim(tokens) = expr {
                    if tokens.is_empty() {
                        return;
                    }
                }
                self.scan_begin_iconsistent(0);
                self.expr_beginning_of_line(expr, true);
                if !remove_semi(expr) {
                    self.scan_string(";");
                }
                self.scan_end();
                self.hardbreak();
            }
            Stmt::Macro(stmt) => {
                self.outer_attrs(&stmt.attrs);
                let semicolon = true;
                self.mac(&stmt.mac, None, semicolon);
                self.hardbreak();
            }
        }
    }
}

pub fn add_semi(expr: &Expr) -> bool {
    match expr {
        #![cfg_attr(all(test, exhaustive), deny(non_exhaustive_omitted_patterns))]
        Expr::Assign(_) | Expr::Break(_) | Expr::Continue(_) | Expr::Return(_) | Expr::Yield(_) => {
            true
        }
        Expr::Binary(expr) =>
        {
            match expr.op {
                #![cfg_attr(all(test, exhaustive), deny(non_exhaustive_omitted_patterns))]
                BinOp::AddAssign(_)
                | BinOp::SubAssign(_)
                | BinOp::MulAssign(_)
                | BinOp::DivAssign(_)
                | BinOp::RemAssign(_)
                | BinOp::BitXorAssign(_)
                | BinOp::BitAndAssign(_)
                | BinOp::BitOrAssign(_)
                | BinOp::ShlAssign(_)
                | BinOp::ShrAssign(_) => true,
                BinOp::Add(_)
                | BinOp::Sub(_)
                | BinOp::Mul(_)
                | BinOp::Div(_)
                | BinOp::Rem(_)
                | BinOp::And(_)
                | BinOp::Or(_)
                | BinOp::BitXor(_)
                | BinOp::BitAnd(_)
                | BinOp::BitOr(_)
                | BinOp::Shl(_)
                | BinOp::Shr(_)
                | BinOp::Eq(_)
                | BinOp::Lt(_)
                | BinOp::Le(_)
                | BinOp::Ne(_)
                | BinOp::Ge(_)
                | BinOp::Gt(_) => false,
                _ => unimplemented!("unknown BinOp"),
            }
        }
        Expr::Group(group) => add_semi(&group.expr),

        Expr::Array(_)
        | Expr::Async(_)
        | Expr::Await(_)
        | Expr::Block(_)
        | Expr::Call(_)
        | Expr::Cast(_)
        | Expr::Closure(_)
        | Expr::Const(_)
        | Expr::Field(_)
        | Expr::ForLoop(_)
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
        | Expr::Struct(_)
        | Expr::Try(_)
        | Expr::TryBlock(_)
        | Expr::Tuple(_)
        | Expr::Unary(_)
        | Expr::Unsafe(_)
        | Expr::Verbatim(_)
        | Expr::While(_) => false,

        _ => false,
    }
}

pub fn break_after(expr: &Expr) -> bool {
    if let Expr::Group(group) = expr {
        if let Expr::Verbatim(verbatim) = group.expr.as_ref() {
            return !verbatim.is_empty();
        }
    }
    true
}

fn remove_semi(expr: &Expr) -> bool {
    match expr {
        #![cfg_attr(all(test, exhaustive), deny(non_exhaustive_omitted_patterns))]
        Expr::ForLoop(_) | Expr::While(_) => true,
        Expr::Group(group) => remove_semi(&group.expr),
        Expr::If(expr) => match &expr.else_branch {
            Some((_else_token, else_branch)) => remove_semi(else_branch),
            None => true,
        },

        Expr::Array(_)
        | Expr::Assign(_)
        | Expr::Async(_)
        | Expr::Await(_)
        | Expr::Binary(_)
        | Expr::Block(_)
        | Expr::Break(_)
        | Expr::Call(_)
        | Expr::Cast(_)
        | Expr::Closure(_)
        | Expr::Continue(_)
        | Expr::Const(_)
        | Expr::Field(_)
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
        | Expr::Struct(_)
        | Expr::Try(_)
        | Expr::TryBlock(_)
        | Expr::Tuple(_)
        | Expr::Unary(_)
        | Expr::Unsafe(_)
        | Expr::Verbatim(_)
        | Expr::Yield(_) => false,

        _ => false,
    }
}