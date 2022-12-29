use std::rc::Rc;

use crate::errors::{Diagnostics, ice_at, SyntaxError};
use crate::utils::{StringPool, NameID};
use crate::utils::sourcefile::{SourceFile, SourceRange};
use super::ast::*;
use super::token::{TokenKind, Token, Keyword};

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
    
    pub(super) fn tok_str(&self, tok: &Token) -> &str {
        self.src.get_span(tok.range)
    }
    
    pub(super) fn tok_name_id(&mut self, tok: &Token) -> NameID {
        // must use self.src.get_span instead of self.tok_str here, to satisfy the borrow checker
        let s = match tok.kind {
            TokenKind::Name => self.src.get_span(tok.range),
            TokenKind::FuncName | TokenKind::VarName => &self.src.get_span(tok.range)[1..],
            _ => ice_at(&format!("token {} is not Name, FuncName or VarName", tok.kind), tok.range),
        };
        self.string_pool.insert(s)
    }
    
    pub(super) fn tok_lowercase_name_id(&mut self, tok: &Token) -> (NameID, String) {
        let s = match tok.kind {
            TokenKind::Name => self.tok_str(tok).to_ascii_lowercase(),
            _ => ice_at(&format!("token {} is not Name", tok.kind), tok.range),
        };
        (self.string_pool.insert(&s), s)
    }
    
    pub(super) fn parse_nodes_until(&mut self, closer: impl Fn(&Parser, &Token) -> bool) -> (Vec<AST>, Option<Token>) {
        self.skip_whitespace();
        
        let mut nodes = Vec::new();
        let end = loop {
            let Some(tok) = self.poll() else {
                break None;
            };
            if closer(self, &tok) {
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
    
    pub(super) fn parse_separated_until<T>(&mut self, open: &Token, parse: impl Fn(&mut Self) -> Option<T>, sep_kind: TokenKind, close_kind: TokenKind) -> Option<(Vec<T>, Token)> {
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
                self.err_unmatched(open);
                self.syntax_error(SyntaxError::TokenExpectedWasEOF(close_kind), self.src.eof_range());
                return None;
            }
            self.skip_whitespace();
            expect_end = sep_kind != TokenKind::Whitespace && self.poll_if_kind(sep_kind).is_none();
        };
        
        Some((children, close))
    }
    
    pub(super) fn parse_value_or_ellipsis(&mut self) -> Option<AST> {
        self.skip_whitespace();
        if let Some(ellipsis) = self.poll_if_kind(TokenKind::Ellipsis) {
            Some(self.parse_ellipsis_group(ellipsis))
        } else {
            self.parse_value()
        }
    }
    
    pub(super) fn parse_value(&mut self) -> Option<AST> {
        self.skip_whitespace();
        let tok = self.expect_poll()?;
        
        match tok.kind {
            TokenKind::Dot |
            TokenKind::Boolean(..) |
            TokenKind::Name |
            TokenKind::Number => Some(AST::LiteralValue(tok)),
            
            TokenKind::Verbatim(k) => Some(AST::Verbatim(tok.range, k)),
            TokenKind::VarName => self.parse_name(tok).map(AST::Name),
            TokenKind::FuncName => self.parse_func_call(tok),
            TokenKind::Keyword(k) => {
                if k == Keyword::Export {
                    self.syntax_error(SyntaxError::ExportNotAllowed, tok.range);
                }
                self.parse_keyword(tok, k)
            },
            TokenKind::LBrace => Some(self.parse_group(tok)),
            TokenKind::LSqb => self.parse_list(tok),
            TokenKind::LAngle if self.has_next(|t| matches!(t.kind, TokenKind::Name | TokenKind::VarName)) => self.parse_tag(tok),
            TokenKind::Quote(open_kind, _) => {
                let (parts, range) = self.parse_template_parts(tok, open_kind)?;
                Some(AST::Template(parts.into_boxed_slice(), range))
            },
            
            _ => {
                self.syntax_error(SyntaxError::ExpectedValue, tok.range);
                None
            }
        }
    }
    
    fn parse_node(&mut self, tok: Token) -> Option<AST> {
        match tok.kind {
            TokenKind::Verbatim(k) => Some(AST::Verbatim(tok.range, k)),
            TokenKind::VarName => self.parse_name(tok).map(AST::Name),
            TokenKind::FuncName => self.parse_func_call(tok),
            TokenKind::Keyword(k) => self.parse_keyword(tok, k),
            
            TokenKind::LBrace => Some(self.parse_group(tok)),
            TokenKind::LSqb => self.parse_list(tok),
            TokenKind::LAngle if self.has_next(|t| matches!(t.kind, TokenKind::Name | TokenKind::VarName | TokenKind::ExclamationMark)) => self.parse_tag(tok),
            
            TokenKind::CloseTag |
            TokenKind::RBrace |
            TokenKind::RSqb => {
                self.err_unmatched(&tok);
                None
            },
            
            TokenKind::Comment => ice_at("comment should not occur here", tok.range),
            
            TokenKind::Whitespace => Some(AST::Whitespace(tok.range)),
            TokenKind::Newline => Some(AST::ParagraphBreak(tok.range)),
            TokenKind::Escape => {
                let s = self.unescape_char(&tok);
                Some(AST::Text(Rc::from(s), tok.range))
            },
            TokenKind::Entity => {
                let s = self.decode_entity(&tok);
                Some(AST::Text(Rc::from(s), tok.range))
            },
            _ => Some(self.parse_text(tok)),
        }
    }
    
    pub(super) fn parse_name(&mut self, token: Token) -> Option<Name> {
        let mut name = Name::SimpleName(SimpleName {
            name_id: self.tok_name_id(&token),
            range: token.range,
        });
        while let Some(tok) = self.poll_if(|_self, t| matches!(t.kind, TokenKind::DoubleColon | TokenKind::QuestionMarkDoubleColon)) {
            let attr_name = self.expect_poll_kind(TokenKind::Name)?;
            let range = name.range().to_end(attr_name.range.end);
            name = Name::AttrName(Box::new(AttrName {
                subject: name,
                is_coalescing: tok.kind == TokenKind::QuestionMarkDoubleColon,
                attr_name_id: self.tok_name_id(&attr_name),
                range,
            }));
        }
        Some(name)
    }
    
    fn parse_keyword(&mut self, at: Token, keyword: Keyword) -> Option<AST> {
        match keyword {
            Keyword::Export => self.parse_export(at)
                .map(Box::new)
                .map(AST::Export),
            Keyword::Fn => self.parse_func_def(at)
                .map(Box::new)
                .map(AST::FuncDef),
            Keyword::Implicit |
            Keyword::Let => self.parse_let_in(at, false)
                .map(Box::new)
                .map(AST::LetIn),
            Keyword::Match => self.parse_match(at)
                .map(Box::new)
                .map(AST::Match),
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
    
    pub(super) fn parse_ellipsis_group(&mut self, open: Token) -> AST {
        let mut children = Vec::new();
        while let Some(tok) = self.poll_if(|_self, tok| !matches!(tok.kind, TokenKind::CloseTag | TokenKind::RBrace)) {
            if let Some(child) = self.parse_node(tok) {
                children.push(child);
            }
        }
        
        let range = Parser::seq_range(open, &children, None);
        AST::Group(children.into_boxed_slice(), range)
    }
    
    fn parse_group(&mut self, open: Token) -> AST {
        let (children, close) = self.parse_nodes_until(|_self, tok| tok.kind == TokenKind::RBrace);
        if close.is_none() {
            self.err_unmatched(&open);
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
                    _self.syntax_error(SyntaxError::SpreadNamedNotAllowed, arg.range);
                } else if !arg.is_positional() {
                    _self.syntax_error(SyntaxError::ArgNamedNotAllowed, arg.range);
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
    
    fn parse_export(&mut self, at: Token) -> Option<Export> {
        self.skip_whitespace();
        if let Some(fn_tok) = self.poll_if_kind(TokenKind::Keyword(Keyword::Fn)) {
            // @export @fn ...
            let f = self.parse_func_def(fn_tok)?;
            let range = at.range.to_end(f.range.end);
            Some(Export::FuncDef(range, f))
        } else if let Some(let_tok) = self.poll_if_kind(TokenKind::Keyword(Keyword::Let)) {
            // @export @let ...
            let let_in = self.parse_let_in(let_tok, true)?;
            let range = at.range.to_end(let_in.range.end);
            Some(Export::LetIn(range, let_in))
        } else {
            // @export(...).
            let vars = self.parse_some_named_args(&at)?;
            self.skip_whitespace();
            let dot = self.expect_poll_kind(TokenKind::Dot)?;
            let range = at.range.to_end(dot.range.end);
            Some(Export::Names(range, vars))
        }
    }
    
    fn parse_let_in(&mut self, at: Token, is_export: bool) -> Option<LetIn> {
        let vars = self.parse_some_named_args(&at)?;
        let child = self.parse_value_or_ellipsis()?;
        
        match &child {
            AST::LiteralValue(Token {range, ..}) |
            AST::Verbatim(range, ..) if !is_export => self.syntax_error(SyntaxError::LetInLiteral, *range),
            _ => {},
        }
        
        Some(LetIn {
            range: at.range.to_end(child.range().end),
            is_implicit: at.kind == TokenKind::Keyword(Keyword::Implicit),
            vars,
            child,
        })
    }
    
    fn parse_some_named_args(&mut self, at: &Token) -> Option<Box<[(NameID, AST)]>> {
        let args = match self.parse_args() {
            Some(args) if !args.is_empty() => args,
            _ => {
                self.syntax_error(SyntaxError::DeclMissingArgs, at.range);
                return None;
            },
        };
        
        Some(args.into_iter()
            .filter_map(|arg| {
                if arg.is_spread() {
                    let e = if arg.is_positional() { SyntaxError::SpreadPositionalNotAllowed } else { SyntaxError::SpreadNamedNotAllowed };
                    self.syntax_error(e, arg.range);
                    None
                } else if arg.is_positional() {
                    self.syntax_error(SyntaxError::DeclPositionalArg, arg.range);
                    None
                } else {
                    Some((arg.name_id, arg.value))
                }
            })
            .collect())
    }
}
