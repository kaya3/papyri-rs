use std::rc::Rc;

use crate::errors::{Diagnostics, SyntaxError, Reported};
use crate::utils::{StringPool, NameID};
use crate::utils::sourcefile::{SourceFile, SourceRange};
use super::ast::*;
use super::token::{TokenKind, Token, Keyword, VerbatimKind};

/// Holds the mutable state of the parser.
pub(super) struct Parser<'a> {
    pub(super) src: Rc<SourceFile>,
    pub(super) diagnostics: &'a mut Diagnostics,
    pub(super) string_pool: &'a mut StringPool,
    pub(super) tokens: Vec<Token>,
}

/// Parses a Papyri source file into an abstract syntax tree.
pub fn parse(src: Rc<SourceFile>, diagnostics: &mut Diagnostics, string_pool: &mut StringPool) -> Vec<AST> {
    let mut q = Parser::new(src, diagnostics, string_pool);
    q.parse_root()
}

impl <'a> Parser<'a> {
    fn parse_root(&mut self) -> Vec<AST> {
        let (nodes, _) = self.parse_nodes_until(|_self, _tok| false);
        nodes
    }
    
    pub(super) fn tok_str(&self, tok: Token) -> &str {
        self.src.get_span(tok.range)
    }
    
    pub(super) fn tok_name_id(&mut self, tok: Token) -> NameID {
        // must use self.src.get_span instead of self.tok_str here, to satisfy the borrow checker
        let s = self.src.get_span(tok.range);
        let s = match tok.kind {
            TokenKind::Name => s,
            TokenKind::FuncName | TokenKind::VarName => &s[1..],
            _ => self.ice_at(&format!("token {} is not Name, FuncName or VarName", tok.kind), tok.range),
        };
        self.string_pool.insert(s)
    }
    
    pub(super) fn tok_lowercase_name_id(&mut self, tok: Token) -> NameID {
        if tok.kind != TokenKind::Name {
            self.ice_at(&format!("token {} is not Name", tok.kind), tok.range);
        }
        let s = self.src.get_span(tok.range);
        self.string_pool.insert_lowercase(s)
    }
    
    pub(super) fn parse_nodes_until(&mut self, closer: impl Fn(&Parser, Token) -> bool) -> (Vec<AST>, Option<Token>) {
        self.skip_whitespace();
        
        let mut nodes = Vec::new();
        let end = loop {
            let Some(tok) = self.poll() else {
                break None;
            };
            if closer(self, tok) {
                break Some(tok);
            } else if let Ok(child) = self.parse_node(tok) {
                nodes.push(child);
            }
        };
        
        while matches!(nodes.last(), Some(node) if node.is_whitespace()) {
            nodes.pop();
        }
        (nodes, end)
    }
    
    pub(super) fn parse_separated_until<T>(&mut self, open: Token, parse: impl Fn(&mut Self) -> Reported<T>, sep_kind: TokenKind, close_kind: TokenKind) -> Reported<(Vec<T>, Token)> {
        let mut children = Vec::new();
        let mut expect_end = false;
        let close = loop {
            self.skip_whitespace();
            let close = if expect_end { self.expect_poll_kind(close_kind).ok() } else { self.poll_if_kind(close_kind) };
            if let Some(close) = close {
                break close;
            }
            
            let child = parse(self)
                .map_err(|_| {
                    self.err_unmatched(open);
                    self.report(SyntaxError::TokenExpectedWasEOF(close_kind), self.src.eof_range())
                })?;
            children.push(child);
            
            self.skip_whitespace();
            expect_end = sep_kind != TokenKind::Whitespace && self.poll_if_kind(sep_kind).is_none();
        };
        
        Ok((children, close))
    }
    
    pub(super) fn parse_expr_or_ellipsis(&mut self) -> Reported<Expr> {
        self.skip_whitespace();
        if let Some(ellipsis) = self.poll_if_kind(TokenKind::Ellipsis) {
            Ok(self.parse_ellipsis_group(ellipsis))
        } else {
            self.parse_expr()
        }
    }
    
    pub(super) fn parse_expr(&mut self) -> Reported<Expr> {
        self.skip_whitespace();
        let tok = self.expect_poll()?;
        self.parse_expr_tok(tok)
    }
    
    pub(super) fn parse_expr_tok(&mut self, tok: Token) -> Reported<Expr> {
        match tok.kind {
            TokenKind::Dot => Ok(Expr::Unit(tok.range)),
            TokenKind::Boolean(b) => Ok(Expr::Bool(b, tok.range)),
            TokenKind::Name => Ok(Expr::BareString(tok.range)),
            
            TokenKind::Number => {
                self.parse_number(tok)
                    .map(|i| Expr::Int(i, tok.range))
            },
            
            TokenKind::Verbatim(..) => Ok(Expr::Verbatim(tok.range)),
            TokenKind::VarName => self.parse_name(tok).map(Expr::Name),
            
            TokenKind::FuncName => {
                self.parse_func_call(tok)
                    .map(Box::new)
                    .map(Expr::FuncCall)
            },
            
            TokenKind::Keyword(k) => match k {
                Keyword::Export => {
                    Err(self.report(SyntaxError::ExportNotAllowed, tok.range))
                },
                Keyword::Fn => {
                    self.parse_func_def(tok, true)
                        .map(Box::new)
                        .map(Expr::FuncDef)
                },
                Keyword::Implicit |
                Keyword::Let => {
                    self.parse_let_in(tok, false)
                        .map(Box::new)
                        .map(Expr::LetIn)
                },
                Keyword::Match => {
                    self.parse_match(tok)
                        .map(Box::new)
                        .map(Expr::Match)
                },
            },
            
            TokenKind::LBrace => Ok(self.parse_group(tok)),
            TokenKind::LSqb => self.parse_list(tok),
            TokenKind::LAngle if self.has_next(|t| matches!(t.kind, TokenKind::Name | TokenKind::VarName | TokenKind::ExclamationMark)) => {
                self.parse_tag(tok)
            },
            TokenKind::Quote(open_kind, _) => {
                let (parts, range) = self.parse_template_parts(tok, open_kind)?;
                Ok(Expr::Template(parts.into_boxed_slice(), range))
            },
            
            _ => {
                Err(self.report(SyntaxError::ExpectedExpr, tok.range))
            }
        }
    }
    
    fn parse_node(&mut self, tok: Token) -> Reported<AST> {
        match tok.kind {
            TokenKind::Keyword(Keyword::Export) => {
                self.parse_export(tok)
                    .map(Box::new)
                    .map(AST::Export)
            },
            TokenKind::Keyword(Keyword::Fn) => {
                self.parse_func_def(tok, false)
                    .map(Box::new)
                    .map(AST::FuncDef)
            },
            TokenKind::Verbatim(k) => Ok(AST::CodeFence(tok.range, k == VerbatimKind::Multiline)),
            
            TokenKind::FuncName |
            TokenKind::Keyword(..) |
            TokenKind::VarName |
            TokenKind::LBrace |
            TokenKind::LSqb => {
                self.parse_expr_tok(tok)
                    .map(AST::Expr)
            },
            
            TokenKind::LAngle if self.has_next(|t| matches!(t.kind, TokenKind::Name | TokenKind::VarName | TokenKind::ExclamationMark)) => {
                self.parse_tag(tok)
                    .map(AST::Expr)
            },
            
            TokenKind::CloseTag |
            TokenKind::RBrace |
            TokenKind::RSqb => {
                Err(self.err_unmatched(tok))
            },
            
            TokenKind::Comment => self.ice_at("comment should not occur here", tok.range),
            
            TokenKind::Whitespace => Ok(AST::Whitespace(tok.range)),
            TokenKind::Newline => Ok(AST::ParagraphBreak(tok.range)),
            TokenKind::Escape => {
                let c = self.unescape_char(tok);
                Ok(AST::Char(c, tok.range))
            },
            TokenKind::Entity => {
                let c = self.decode_entity(tok);
                Ok(AST::Char(c, tok.range))
            },
            _ => Ok(self.parse_text(tok)),
        }
    }
    
    fn parse_number(&mut self, token: Token) -> Reported<i64> {
        self.tok_str(token)
            .parse::<i64>()
            .map_err(|e| self.report(SyntaxError::TokenInvalidNumber(e), token.range))
    }
    
    pub(super) fn parse_name(&mut self, token: Token) -> Reported<Name> {
        let mut name = Name::Simple(SimpleName {
            name_id: self.tok_name_id(token),
            range: token.range,
        });
        while let Some(tok) = self.poll_if(|_self, t| matches!(t.kind, TokenKind::DoubleColon | TokenKind::QuestionMarkDoubleColon)) {
            let is_coalescing = tok.kind == TokenKind::QuestionMarkDoubleColon;
            if let Some(index_tok) = self.poll_if_kind(TokenKind::Number) {
                let index = self.parse_number(index_tok)?;
                let range = name.range().to_end(index_tok.range.end);
                name = Name::Index(Box::new(IndexName {
                    subject: name,
                    is_coalescing,
                    index,
                    range,
                }));
            } else {
                let attr_name = self.expect_poll_kind(TokenKind::Name)?;
                let range = name.range().to_end(attr_name.range.end);
                name = Name::Attr(Box::new(AttrName {
                    subject: name,
                    is_coalescing,
                    attr_name_id: self.tok_name_id(attr_name),
                    range,
                }));
            }
        }
        Ok(name)
    }
    
    fn seq_range(open: Token, children: &[AST], close: Option<Token>) -> SourceRange {
        let end = match (children.last(), close) {
            (_, Some(close)) => close.range.end,
            (Some(child), _) => child.range().end,
            _ => open.range.end,
        };
        open.range.to_end(end)
    }
    
    pub(super) fn parse_ellipsis_group(&mut self, open: Token) -> Expr {
        let mut children = Vec::new();
        while let Some(tok) = self.poll_if(|_self, tok| !matches!(tok.kind, TokenKind::CloseTag | TokenKind::RBrace)) {
            if let Ok(child) = self.parse_node(tok) {
                children.push(child);
            }
        }
        self.group_of(open, children, None)
    }
    
    fn parse_group(&mut self, open: Token) -> Expr {
        let (children, close) = self.parse_nodes_until(|_self, tok| tok.kind == TokenKind::RBrace);
        if close.is_none() {
            self.err_unmatched(open);
        }
        self.group_of(open, children, close)
    }
    
    fn group_of(&mut self, open: Token, children: Vec<AST>, close: Option<Token>) -> Expr {
        let range = Parser::seq_range(open, &children, close);
        if children.is_empty() {
            Expr::Unit(range)
        } else {
            Expr::Group(children.into_boxed_slice(), range)
        }
    }
    
    fn parse_list(&mut self, open: Token) -> Reported<Expr> {
        let (children, close) = self.parse_separated_until(
            open,
            |_self| {
                let arg = _self.parse_arg()?;
                if arg.spread_kind == SpreadKind::Named {
                    _self.report(SyntaxError::SpreadNamedNotAllowed, arg.range);
                } else if !arg.is_positional() {
                    _self.report(SyntaxError::ArgNamedNotAllowed, arg.range);
                }
                Ok((arg.value, arg.spread_kind == SpreadKind::Positional))
            },
            TokenKind::Comma,
            TokenKind::RSqb,
        )?;
        
        Ok(Expr::List(
            children.into_boxed_slice(),
            open.range.to_end(close.range.end),
        ))
    }
    
    fn parse_export(&mut self, at: Token) -> Reported<Export> {
        self.skip_whitespace();
        if let Some(fn_tok) = self.poll_if_kind(TokenKind::Keyword(Keyword::Fn)) {
            // @export @fn ...
            let f = self.parse_func_def(fn_tok, false)?;
            let range = at.range.to_end(f.range.end);
            Ok(Export::FuncDef(range, f))
        } else if let Some(let_tok) = self.poll_if_kind(TokenKind::Keyword(Keyword::Let)) {
            // @export @let ...
            let let_in = self.parse_let_in(let_tok, true)?;
            let range = at.range.to_end(let_in.range.end);
            Ok(Export::LetIn(range, let_in))
        } else {
            // @export(...).
            let vars = self.parse_some_named_args(at)?;
            self.skip_whitespace();
            let dot = self.expect_poll_kind(TokenKind::Dot)?;
            let range = at.range.to_end(dot.range.end);
            Ok(Export::Names(range, vars))
        }
    }
    
    fn parse_let_in(&mut self, at: Token, is_export: bool) -> Reported<LetIn> {
        let vars = self.parse_some_named_args(at)?;
        let child = self.parse_expr_or_ellipsis()?;
        
        if child.is_literal() && !is_export {
            self.report(SyntaxError::LetInLiteral, child.range());
        }
        
        Ok(LetIn {
            range: at.range.to_end(child.range().end),
            is_implicit: at.kind == TokenKind::Keyword(Keyword::Implicit),
            vars,
            child,
        })
    }
    
    fn parse_some_named_args(&mut self, at: Token) -> Reported<Box<[(NameID, Expr)]>> {
        let args = self.parse_args()?;
        if args.is_empty() {
            return Err(self.report(SyntaxError::DeclMissingArgs, at.range))
        }
        
        Ok(args.into_iter()
            .filter_map(|arg| {
                if arg.is_spread() {
                    let e = if arg.is_positional() { SyntaxError::SpreadPositionalNotAllowed } else { SyntaxError::SpreadNamedNotAllowed };
                    self.report(e, arg.range);
                    None
                } else if arg.is_positional() {
                    self.report(SyntaxError::DeclPositionalArg, arg.range);
                    None
                } else {
                    Some((arg.name_id, arg.value))
                }
            })
            .collect())
    }
}
