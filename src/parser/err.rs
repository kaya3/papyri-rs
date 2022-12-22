use crate::errors::SyntaxError;
use crate::utils::sourcefile::SourceRange;

use super::base::Parser;
use super::token::Token;

impl <'a> Parser<'a> {
    pub(super) fn syntax_error(&mut self, e: SyntaxError, range: SourceRange) {
        self.diagnostics.syntax_error(e, self.src.clone(), range);
    }
    
    /// Reports a syntax error caused by an unexpected token.
    pub(super) fn err_unexpected_token(&mut self, tok: &Token) {
        self.syntax_error(SyntaxError::TokenUnexpected(tok.kind), tok.range);
    }
    
    /// Reports a syntax error caused by an unmatched opening or closing token.
    pub(super) fn err_unmatched(&mut self, tok: &Token) {
        self.syntax_error(SyntaxError::TokenUnmatched(tok.kind), tok.range);
    }
}
