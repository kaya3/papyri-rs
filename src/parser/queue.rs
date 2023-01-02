use std::rc::Rc;

use crate::errors::{Diagnostics, SyntaxError};
use crate::utils::StringPool;
use crate::utils::sourcefile::SourceFile;
use super::base::Parser;
use super::token::{Token, TokenKind};
use super::tokenizer::tokenize;

impl <'a> Parser<'a> {
    /// Creates a new token queue for parsing.
    pub(super) fn new(src: Rc<SourceFile>, diagnostics: &'a mut Diagnostics, string_pool: &'a mut StringPool) -> Parser<'a> {
        let mut tokens = tokenize(src.clone(), true, diagnostics);
        tokens.reverse();
        Parser {src, diagnostics, string_pool, tokens}
    }
    
    /// Removes and returns the next token from the queue, if it exists.
    pub(super) fn poll(&mut self) -> Option<Token> {
        self.tokens.pop()
    }
    
    /// Removes and returns the next token from the queue, if it exists;
    /// otherwise, an "unexpected EOF" error is emitted.
    pub(super) fn expect_poll(&mut self) -> Option<Token> {
        let t = self.poll();
        if t.is_none() {
            self.syntax_error(SyntaxError::UnexpectedEOF, self.src.eof_range());
        }
        t
    }
    
    /// Removes and returns the next token from the queue, if it exists and
    /// matches the given kind; otherwise, a syntax error is emitted.
    pub(super) fn expect_poll_kind(&mut self, kind: TokenKind) -> Option<Token> {
        match self.tokens.last() {
            Some(tok) if tok.kind != kind => {
                self.syntax_error(SyntaxError::TokenExpectedWas(kind, tok.kind), tok.range);
                None
            },
            _ => self.expect_poll(),
        }
    }
    
    /// Removes and returns the next token from the queue, if it exists and
    /// satisfies the given predicate function.
    pub(super) fn poll_if(&mut self, mut predicate: impl FnMut(&Parser, Token) -> bool) -> Option<Token> {
        match self.tokens.last() {
            Some(tok) if predicate(self, *tok) => self.poll(),
            _ => None,
        }
    }
    
    /// Indicates whether the next token in the queue exists and satisfies the
    /// given predicate function.
    pub(super) fn has_next(&mut self, mut predicate: impl FnMut(Token) -> bool) -> bool {
        matches!(self.tokens.last(), Some(tok) if predicate(*tok))
    }
    
    /// Removes any whitespace tokens from the front of the queue.
    pub(super) fn skip_whitespace(&mut self) {
        // only need to poll once, since multiple whitespace tokens are never
        // consecutive.
        self.poll_if(|_self, t| t.is_whitespace());
    }
    
    /// Removes and returns the next token in the queue, if it exists and
    /// matches the given kind.
    pub(super) fn poll_if_kind(&mut self, kind: TokenKind) -> Option<Token> {
        self.poll_if(|_self, tok| tok.kind == kind)
    }
}
