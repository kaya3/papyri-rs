use std::rc::Rc;

use crate::errors::{Diagnostics, ice_at, SyntaxError};
use crate::utils::{SourceFile, StringPool, SourceRange};
use super::ast::*;
use super::queue::Parser;
use super::text;
use super::token::{TokenKind, Token};

/// Parses a Papyri source file into an abstract syntax tree.
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
                self.diagnostics.syntax_error(SyntaxError::TokenExpectedWasEOF(close_kind), &SourceRange::eof(open.range.src.clone()));
                return None;
            }
            self.skip_whitespace();
            expect_end = sep_kind != TokenKind::Whitespace && self.poll_if_kind(sep_kind).is_none();
        };
        
        Some((children, close))
    }
    
    pub fn parse_value_or_ellipsis(&mut self) -> Option<AST> {
        self.skip_whitespace();
        if let Some(ellipsis) = self.poll_if_kind(TokenKind::Ellipsis) {
            Some(self.parse_ellipsis_group(ellipsis))
        } else {
            self.parse_value()
        }
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
            TokenKind::VarName => self.parse_name(tok).map(AST::Name),
            TokenKind::FuncName => self.parse_func(tok),
            TokenKind::LBrace => Some(self.parse_group(tok)),
            TokenKind::LSqb => self.parse_list(tok),
            TokenKind::LAngle if self.has_next(|t| matches!(t.kind, TokenKind::Name | TokenKind::VarName)) => self.parse_tag(tok),
            TokenKind::Quote(..) => {
                let (parts, range) = self.parse_template_parts(tok)?;
                Some(AST::Template(parts.into_boxed_slice(), range))
            },
            
            _ => {
                self.diagnostics.syntax_error(SyntaxError::ExpectedValue, &tok.range);
                None
            }
        }
    }
    
    fn parse_node(&mut self, tok: Token) -> Option<AST> {
        match tok.kind {
            TokenKind::Verbatim => Some(AST::Verbatim(tok)),
            TokenKind::VarName => self.parse_name(tok).map(AST::Name),
            TokenKind::FuncName => self.parse_func(tok),
            
            TokenKind::LBrace => Some(self.parse_group(tok)),
            TokenKind::LSqb => self.parse_list(tok),
            TokenKind::LAngle if self.has_next(|t| matches!(t.kind, TokenKind::Name | TokenKind::VarName | TokenKind::ExclamationMark)) => self.parse_tag(tok),
            
            TokenKind::CloseTag |
            TokenKind::RBrace |
            TokenKind::RSqb => {
                self.diagnostics.err_unmatched(&tok);
                None
            },
            
            TokenKind::Comment => {
                ice_at("comment should not occur here", &tok.range);
            },
            
            TokenKind::Whitespace => Some(AST::Whitespace(tok.range)),
            TokenKind::Newline => Some(AST::ParagraphBreak(tok.range)),
            TokenKind::Escape => {
                let s = text::unescape_char(&tok.range, self.diagnostics);
                Some(AST::Text(Rc::from(s), tok.range))
            },
            TokenKind::Entity => {
                let s = text::decode_entity(&tok.range, self.diagnostics);
                Some(AST::Text(Rc::from(s), tok.range))
            },
            _ => Some(self.parse_text(tok)),
        }
    }
    
    pub fn parse_name(&mut self, token: Token) -> Option<Name> {
        let mut name = Name::SimpleName(SimpleName {
            name_id: self.string_pool.insert(token.get_var_name()),
            range: token.range,
        });
        while let Some(tok) = self.poll_if_kind(TokenKind::DoubleColon) {
            let attr_name = self.expect_poll_kind(TokenKind::Name)?;
            let range = name.range().to_end(attr_name.range.end);
            name = Name::AttrName(Box::new(AttrName {
                subject: name,
                is_coalescing: tok.as_str().starts_with("?"),
                attr_name_id: self.string_pool.insert(attr_name.as_str()),
                range,
            }));
        }
        Some(name)
    }
    
    fn parse_func(&mut self, at: Token) -> Option<AST> {
        match at.as_str() {
            "@fn" => self.parse_func_def(at)
                .map(Box::new)
                .map(AST::FuncDef),
            "@implicit" | "@let" => self.parse_let_in(at)
                .map(Box::new)
                .map(AST::LetIn),
            "@match" => self.parse_match(at)
                .map(Box::new)
                .map(AST::Match),
            _ => self.parse_func_call(at)
                .map(Box::new)
                .map(AST::FuncCall),
        }
    }
    
    fn seq_range(open: Token, children: &[AST], close: Option<Token>) -> SourceRange {
        let end = match (children.last(), close) {
            (_, Some(close)) => close.range.end,
            (Some(child), _) => child.range().end,
            _ => open.range.end,
        };
        open.range.to_end(end)
    }
    
    pub fn parse_ellipsis_group(&mut self, open: Token) -> AST {
        let mut children = Vec::new();
        while let Some(tok) = self.poll_if(|tok| !matches!(tok.kind, TokenKind::CloseTag | TokenKind::RBrace)) {
            if let Some(child) = self.parse_node(tok) {
                children.push(child);
            }
        }
        
        let range = Parser::seq_range(open, &children, None);
        AST::Group(children.into_boxed_slice(), range)
    }
    
    fn parse_group(&mut self, open: Token) -> AST {
        let (children, close) = self.parse_nodes_until(|tok| tok.kind == TokenKind::RBrace);
        if close.is_none() {
            self.diagnostics.err_unmatched(&open);
        }
        
        let range = Parser::seq_range(open, &children, close);
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
    
    fn parse_let_in(&mut self, at: Token) -> Option<LetIn> {
        let Some(args) = self.parse_args() else {
            self.diagnostics.syntax_error(SyntaxError::LetInMissingArgs, &at.range);
            return None;
        };
        
        let vars: Box<[_]> = args.into_iter()
            .filter_map(|arg| {
                if arg.is_spread() {
                    self.diagnostics.syntax_error(SyntaxError::SpreadNotAllowed, &arg.range);
                    None
                } else if arg.is_positional() {
                    self.diagnostics.syntax_error(SyntaxError::LetInPositionalArg, &arg.range);
                    None
                } else {
                    Some((arg.name_id, arg.value))
                }
            })
            .collect();
        
        let child = self.parse_value_or_ellipsis()?;
        match &child {
            AST::LiteralValue(tok) |
            AST::Verbatim(tok) => self.diagnostics.syntax_error(SyntaxError::LetInLiteral, &tok.range),
            _ => {},
        }
        
        Some(LetIn {
            range: at.range.to_end(child.range().end),
            is_implicit: at.as_str() == "@implicit",
            vars,
            child,
        })
    }
}
