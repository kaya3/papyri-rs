use crate::utils::sourcefile::SourceRange;
use super::ast::*;
use super::base::Parser;
use super::token::{Token, TokenKind, QuoteKind};

impl <'a> Parser<'a> {
    pub(super) fn parse_template_parts(&mut self, open: Token, open_kind: QuoteKind) -> Option<(Vec<TemplatePart>, SourceRange)> {
        let mut parts: Vec<TemplatePart> = Vec::new();
        let mut brace_stack: Vec<Token> = Vec::new();
        let close = loop {
            let Some(tok) = self.expect_poll() else {
                self.err_unmatched(&open);
                return None;
            };
            match tok.kind {
                TokenKind::Quote(k, _) if k == open_kind => break tok,
                
                TokenKind::Newline |
                TokenKind::Verbatim(..) => {
                    self.err_unexpected_token(&tok);
                },
                
                TokenKind::LBrace => {
                    brace_stack.push(tok);
                    self.skip_whitespace();
                },
                TokenKind::RBrace => {
                    if brace_stack.pop().is_none() {
                        self.err_unexpected_token(&tok);
                    } else if matches!(parts.last(), Some(TemplatePart::Whitespace)) {
                        parts.pop();
                    }
                },
                
                TokenKind::VarName => {
                    if let Some(name) = self.parse_name(tok) {
                        parts.push(TemplatePart::Name(name));
                    }
                },
                TokenKind::Escape => {
                    let s = self.unescape_char(&tok)
                        .into_boxed_str();
                    parts.push(TemplatePart::LiteralStr(s));
                },
                TokenKind::Entity => {
                    let s = self.decode_entity(&tok)
                        .into_boxed_str();
                    parts.push(TemplatePart::LiteralStr(s));
                },
                TokenKind::Whitespace => parts.push(TemplatePart::Whitespace),
                
                _ => match parts.last_mut() {
                    Some(TemplatePart::Literal(range)) if range.end == tok.range.start => {
                        range.end = tok.range.end;
                    },
                    _ => {
                        parts.push(TemplatePart::Literal(tok.range));
                    },
                },
            }
        };
        
        for tok in brace_stack {
            self.err_unmatched(&tok);
        }
        
        Some((
            parts,
            open.range.to_end(close.range.end),
        ))
    }
}
