use crate::errors::ice_at;
use crate::utils::SourceRange;
use super::ast::*;
use super::queue::Parser;
use super::text;
use super::token::{Token, TokenKind};

impl <'a> Parser<'a> {
    pub fn parse_template_parts(&mut self, open: Token) -> Option<(Vec<TemplatePart>, SourceRange)> {
        let TokenKind::Quote(open_kind, _) = open.kind else {
            ice_at("invalid template token", &open.range);
        };
        
        let mut parts: Vec<TemplatePart> = Vec::new();
        let mut brace_stack: Vec<Token> = Vec::new();
        let close = loop {
            let Some(tok) = self.expect_poll() else {
                self.diagnostics.err_unmatched(&open);
                return None;
            };
            match tok.kind {
                TokenKind::Quote(k, _) if k == open_kind => break tok,
                
                TokenKind::Newline |
                TokenKind::Verbatim => {
                    self.diagnostics.err_unexpected_token(&tok);
                },
                
                TokenKind::LBrace => {
                    brace_stack.push(tok);
                    self.skip_whitespace();
                },
                TokenKind::RBrace => {
                    if brace_stack.pop().is_none() {
                        self.diagnostics.err_unexpected_token(&tok);
                    } else if matches!(parts.last(), Some(TemplatePart::Whitespace)) {
                        parts.pop();
                    }
                },
                
                TokenKind::VarName => parts.push(TemplatePart::VarName(self.parse_var_name(tok))),
                TokenKind::Escape => {
                    let s = text::unescape_char(&tok.range, self.diagnostics)
                        .into_boxed_str();
                    parts.push(TemplatePart::LiteralStr(s));
                },
                TokenKind::Entity => {
                    let s = text::decode_entity(&tok.range, self.diagnostics)
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
            self.diagnostics.err_unmatched(&tok);
        }
        
        Some((
            parts,
            open.range.to_end(close.range.end),
        ))
    }
}
