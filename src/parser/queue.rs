use std::rc::Rc;

use crate::errors::{Diagnostics, SyntaxError};
use crate::utils::{SourceFile, StringPool, SourceRange};
use super::token::{Token, TokenKind};
use super::tokenizer::tokenize;

impl Diagnostics {
    /// Reports a syntax error caused by an unexpected token.
    pub fn err_unexpected_token(&mut self, tok: &Token) {
        self.syntax_error(SyntaxError::TokenUnexpected(tok.clone()), &tok.range);
    }
    
    /// Reports a syntax error caused by an unmatched opening or closing token.
    pub fn err_unmatched(&mut self, tok: &Token) {
        let e = if tok.kind == TokenKind::CloseTag {
            SyntaxError::TagUnmatchedClose
        } else {
            SyntaxError::TokenUnmatched(tok.clone())
        };
        self.syntax_error(e, &tok.range);
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
            let eof_range = SourceRange::eof(self.src.clone());
            self.diagnostics.syntax_error(SyntaxError::UnexpectedEOF, &eof_range);
        }
        t
    }
    
    /// Removes and returns the next token from the queue, if it exists and
    /// matches the given kind; otherwise, a syntax error is emitted.
    pub fn expect_poll_kind(&mut self, kind: TokenKind) -> Option<Token> {
        match self.tokens.last() {
            Some(tok) if tok.kind != kind => {
                self.diagnostics.syntax_error(SyntaxError::TokenExpectedWas(kind, tok.clone()), &tok.range);
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
}
