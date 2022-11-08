use std::rc::Rc;

use crate::utils::{Diagnostics, SourceFile, StringPool};
use super::token::{Token, TokenKind};
use super::tokenizer::tokenize;

impl Diagnostics {
    pub fn err_unexpected_token(&mut self, tok: &Token) {
        self.syntax_error(&format!("unexpected {:?}", tok.kind), &tok.range);
    }
    
    pub fn err_unmatched(&mut self, tok: &Token) {
        self.syntax_error(&format!("unmatched {:?}", tok.kind), &tok.range);
    }
}

pub struct TokenQueue<'a> {
    src: Rc<SourceFile>,
    pub diagnostics: &'a mut Diagnostics,
    pub string_pool: &'a mut StringPool,
    tokens: Vec<Token>,
}

impl <'a> TokenQueue<'a> {
    pub fn new(src: Rc<SourceFile>, diagnostics: &'a mut Diagnostics, string_pool: &'a mut StringPool) -> TokenQueue<'a> {
        let mut tokens = tokenize(src.clone(), true, diagnostics);
        tokens.reverse();
        TokenQueue {src, diagnostics, string_pool, tokens}
    }
    
    pub fn poll(&mut self) -> Option<Token> {
        self.tokens.pop()
    }
    
    pub fn expect_poll(&mut self) -> Option<Token> {
        let t = self.poll();
        if t.is_none() {
            let eof_range = SourceFile::eof_range(self.src.clone());
            self.diagnostics.syntax_error("unexpected end of source", &eof_range);
        }
        t
    }
    
    pub fn expect_poll_kind(&mut self, kind: TokenKind) -> Option<Token> {
        match self.tokens.last() {
            Some(tok) if tok.kind != kind => {
                self.diagnostics.syntax_error(&format!("expected {:?}, was {:?}", kind, tok.kind), &tok.range);
                None
            },
            _ => self.expect_poll(),
        }
    }
    
    pub fn poll_if(&mut self, predicate: impl Fn(&Token) -> bool) -> Option<Token> {
        match self.tokens.last() {
            Some(tok) if predicate(tok) => self.poll(),
            _ => None,
        }
    }
    
    pub fn poll_if_map<T>(&mut self, f: impl Fn(&Token) -> Option<T>) -> Option<(Token, T)> {
        let r = f(self.tokens.last()?)?;
        Some((self.poll().unwrap(), r))
    }
    
    pub fn skip_whitespace(&mut self) {
        self.poll_if(Token::is_whitespace);
    }
    
    pub fn poll_if_kind(&mut self, kind: TokenKind) -> Option<Token> {
        self.poll_if(|tok| tok.kind == kind)
    }
    
    pub fn poll_ignoring_whitespace_if_kind(&mut self, kind: TokenKind) -> Option<Token> {
        if self.tokens.len() < 2 {
            return self.poll_if_kind(kind);
        }
        
        let peek1 = self.tokens.last();
        let peek2 = self.tokens.get(self.tokens.len() - 2);
        match (peek1, peek2) {
            (Some(tok), _) if tok.kind == kind => {
                self.poll()
            },
            (Some(ws), Some(tok)) if ws.is_whitespace() && tok.kind == kind => {
                self.poll();
                self.poll()
            },
            _ => None,
        }
    }
}
