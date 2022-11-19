use std::rc::Rc;

use crate::utils::{Diagnostics, SourceFile, StringPool};
use super::token::{Token, TokenKind};
use super::tokenizer::tokenize;

impl Diagnostics {
    /// Reports a syntax error caused by an unexpected token.
    pub fn err_unexpected_token(&mut self, tok: &Token) {
        self.syntax_error(&format!("unexpected {:?}", tok.kind), &tok.range);
    }
    
    /// Reports a syntax error caused by an unmatched opening token.
    pub fn err_unmatched(&mut self, tok: &Token) {
        self.syntax_error(&format!("unmatched {:?}", tok.kind), &tok.range);
    }
}

/// Holds the mutable state of the parser.
pub struct Parser<'a> {
    src: Rc<SourceFile>,
    pub diagnostics: &'a mut Diagnostics,
    pub string_pool: &'a mut StringPool,
    tokens: Vec<Token>,
}

impl <'a> Parser<'a> {
    /// Creates a new token queue for parsing.
    pub fn new(src: Rc<SourceFile>, diagnostics: &'a mut Diagnostics, string_pool: &'a mut StringPool) -> Parser<'a> {
        let mut tokens = tokenize(src.clone(), true, diagnostics);
        tokens.reverse();
        Parser {src, diagnostics, string_pool, tokens}
    }
    
    /// Removes and returns the next token from the queue, if it exists.
    pub fn poll(&mut self) -> Option<Token> {
        self.tokens.pop()
    }
    
    /// Removes and returns the next token from the queue, if it exists;
    /// otherwise, an "unexpected EOF" error is emitted.
    pub fn expect_poll(&mut self) -> Option<Token> {
        let t = self.poll();
        if t.is_none() {
            let eof_range = SourceFile::eof_range(self.src.clone());
            self.diagnostics.syntax_error("unexpected end of source", &eof_range);
        }
        t
    }
    
    /// Removes and returns the next token from the queue, if it exists and
    /// matches the given kind; otherwise, a syntax error is emitted.
    pub fn expect_poll_kind(&mut self, kind: TokenKind) -> Option<Token> {
        match self.tokens.last() {
            Some(tok) if tok.kind != kind => {
                self.diagnostics.syntax_error(&format!("expected {:?}, was {:?}", kind, tok.kind), &tok.range);
                None
            },
            _ => self.expect_poll(),
        }
    }
    
    /// Removes and returns the next token from the queue, if it exists and
    /// satisfies the given predicate function.
    pub fn poll_if(&mut self, mut predicate: impl FnMut(&Token) -> bool) -> Option<Token> {
        match self.tokens.last() {
            Some(tok) if predicate(tok) => self.poll(),
            _ => None,
        }
    }
    
    /// Indicates whether the next token in the queue exists and satisfies the
    /// given predicate function.
    pub fn has_next(&mut self, mut predicate: impl FnMut(&Token) -> bool) -> bool {
        matches!(self.tokens.last(), Some(tok) if predicate(tok))
    }
    
    /// Removes any whitespace tokens from the front of the queue.
    pub fn skip_whitespace(&mut self) {
        // only need to poll once, since multiple whitespace tokens are never
        // consecutive.
        self.poll_if(Token::is_whitespace);
    }
    
    /// Removes and returns the next token in the queue, if it exists and
    /// matches the given kind.
    pub fn poll_if_kind(&mut self, kind: TokenKind) -> Option<Token> {
        self.poll_if(|tok| tok.kind == kind)
    }
    
    /// Removes up to and returns the next non-whitespace token in the queue,
    /// if it exists and matches the given kind.
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
