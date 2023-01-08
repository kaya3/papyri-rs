use crate::errors;
use crate::utils::sourcefile::SourceRange;

use super::base::Parser;
use super::token::Token;

impl <'a> Parser<'a> {
    pub(super) fn ice_at(&self, msg: &str, range: SourceRange) -> ! {
        errors::ice_at(msg, self.src.as_ref(), range)
    }
    
    pub(super) fn report<T: Into<errors::PapyriError>>(&mut self, e: T, range: SourceRange) {
        self.diagnostics.report_static(e, self.src.clone(), range);
    }
    
    /// Reports a syntax error caused by an unexpected token.
    pub(super) fn err_unexpected_token(&mut self, tok: Token) {
        self.report(errors::SyntaxError::TokenUnexpected(tok.kind), tok.range);
    }
    
    /// Reports a syntax error caused by an unmatched opening or closing token.
    pub(super) fn err_unmatched(&mut self, tok: Token) {
        self.report(errors::SyntaxError::TokenUnmatched(tok.kind), tok.range);
    }
}
