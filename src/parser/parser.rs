use std::rc::Rc;

use super::ast::*;
use crate::errors::{Diagnostics, ice_at, SyntaxError};
use crate::utils::{SourceFile, StringPool};
use super::queue::Parser;
use super::token::{TokenKind, Token};

pub fn parse(src: Rc<SourceFile>, diagnostics: &mut Diagnostics, string_pool: &mut StringPool) -> Vec<AST> {
    let mut q = Parser::new(src, diagnostics, string_pool);
    q.parse_root()
}

impl <'a> Parser<'a> {
    fn parse_root(&mut self) -> Vec<AST> {
        let (nodes, _) = self.parse_nodes_until(|_tok| false);
        nodes
    }
    
    pub fn parse_nodes_until(&mut self, closer: impl Fn(&Token) -> bool) -> (Vec<AST>, Option<Token>) {
        self.skip_whitespace();
        
        let mut nodes = Vec::new();
        let end = loop {
            let Some(tok) = self.poll() else {
                break None;
            };
            if closer(&tok) {
                break Some(tok);
            } else if let Some(child) = self.parse_node(tok) {
                nodes.push(child);
            }
        };
        
        while matches!(nodes.last(), Some(node) if node.is_whitespace()) {
            nodes.pop();
        }
        (nodes, end)
    }
    
    pub fn parse_separated_until<T>(&mut self, open: &Token, parse: impl Fn(&mut Self) -> Option<T>, sep_kind: TokenKind, close_kind: TokenKind) -> Option<(Vec<T>, Token)> {
        let mut children = Vec::new();
        let mut expect_end = false;
        let close = loop {
            self.skip_whitespace();
            let close = if expect_end { self.expect_poll_kind(close_kind) } else { self.poll_if_kind(close_kind) };
            if let Some(close) = close {
                break close;
            }
            
            if let Some(child) = parse(self) {
                children.push(child);
            } else {
                self.diagnostics.err_unmatched(open);
                self.diagnostics.syntax_error(SyntaxError::TokenExpectedWasEOF(close_kind), &SourceFile::eof_range(open.range.src.clone()));
                return None;
            }
            self.skip_whitespace();
            expect_end = sep_kind != TokenKind::Whitespace && self.poll_if_kind(sep_kind).is_none();
        };
        
        Some((children, close))
    }
    
    pub fn parse_value(&mut self) -> Option<AST> {
        self.skip_whitespace();
        let tok = self.expect_poll()?;
        
        match tok.kind {
            TokenKind::Dot |
            TokenKind::Boolean |
            TokenKind::Name |
            TokenKind::Number => Some(AST::LiteralValue(tok)),
            
            TokenKind::Verbatim => Some(AST::Verbatim(tok)),
            TokenKind::VarName => Some(AST::VarName(self.parse_var_name(tok))),
            TokenKind::FuncName => self.parse_func(tok),
            TokenKind::Ellipsis => Some(self.parse_ellipsis_group(tok)),
            TokenKind::LBrace => Some(self.parse_group(tok)),
            TokenKind::LSqb => self.parse_list(tok),
            TokenKind::LAngle if self.has_next(|t| matches!(t.kind, TokenKind::Name | TokenKind::VarName)) => self.parse_tag(tok),
            TokenKind::Quote(..) => self.parse_template(tok),
            
            _ => {
                self.diagnostics.syntax_error(SyntaxError::ExpectedValue, &tok.range);
                None
            }
        }
    }
    
    fn parse_node(&mut self, tok: Token) -> Option<AST> {
        match tok.kind {
            TokenKind::Verbatim => Some(AST::Verbatim(tok)),
            TokenKind::VarName => Some(AST::VarName(self.parse_var_name(tok))),
            TokenKind::FuncName => self.parse_func(tok),
            
            TokenKind::LBrace => Some(self.parse_group(tok)),
            TokenKind::LSqb => self.parse_list(tok),
            TokenKind::LAngle if self.has_next(|t| matches!(t.kind, TokenKind::Name | TokenKind::VarName)) => self.parse_tag(tok),
            
            TokenKind::CloseTag |
            TokenKind::RBrace |
            TokenKind::RSqb => {
                self.diagnostics.err_unmatched(&tok);
                None
            },
            
            TokenKind::Undetermined |
            TokenKind::Comment |
            TokenKind::RAngleComment => {
                ice_at("token kind should not occur here", &tok.range)
            },
            
            TokenKind::Whitespace => Some(AST::Whitespace(tok.range)),
            TokenKind::Newline => Some(AST::ParagraphBreak(tok.range)),
            TokenKind::Escape => Some(AST::Escape(tok.range)),
            TokenKind::Entity => Some(AST::Entity(tok.range)),
            _ => Some(self.parse_text(tok)),
        }
    }
    
    pub fn parse_var_name(&mut self, token: Token) -> VarName {
        let name_id = self.string_pool.insert(token.get_var_name());
        VarName {name_id, range: token.range}
    }
    
    fn parse_func(&mut self, at: Token) -> Option<AST> {
        match at.as_str() {
            "@fn" => self.parse_func_def(at)
                .map(Box::new)
                .map(AST::FuncDef),
            "@match" => self.parse_match(at)
                .map(Box::new)
                .map(AST::Match),
            _ => self.parse_func_call(at)
                .map(Box::new)
                .map(AST::FuncCall),
        }
    }
    
    fn parse_ellipsis_group(&mut self, open: Token) -> AST {
        let mut children = Vec::new();
        while let Some(tok) = self.poll_if(|tok| !matches!(tok.kind, TokenKind::CloseTag | TokenKind::RSqb | TokenKind::RBrace)) {
            if let Some(child) = self.parse_node(tok) {
                children.push(child);
            }
        }
        
        let range = AST::seq_range(open, &children, None);
        AST::Group(children.into_boxed_slice(), range)
    }
    
    fn parse_group(&mut self, open: Token) -> AST {
        let (children, close) = self.parse_nodes_until(|tok| tok.kind == TokenKind::RBrace);
        if close.is_none() {
            self.diagnostics.err_unmatched(&open);
        }
        
        let range = AST::seq_range(open, &children, close);
        AST::Group(children.into_boxed_slice(), range)
    }
    
    fn parse_list(&mut self, open: Token) -> Option<AST> {
        let (children, close) = self.parse_separated_until(
            &open,
            |_self| {
                let arg = _self.parse_arg()?;
                if arg.spread_kind == SpreadKind::Named {
                    _self.diagnostics.syntax_error(SyntaxError::SpreadNamedNotAllowed, &arg.range);
                } else if !arg.is_positional() {
                    _self.diagnostics.syntax_error(SyntaxError::ArgNamedNotAllowed, &arg.range);
                }
                Some((arg.value, arg.spread_kind == SpreadKind::Positional))
            },
            TokenKind::Comma,
            TokenKind::RSqb,
        )?;
        
        Some(AST::List(
            children.into_boxed_slice(),
            open.range.to_end(close.range.end),
        ))
    }
}
