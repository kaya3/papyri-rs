use crate::errors::Reported;
use crate::utils::sourcefile::SourceRange;
use super::ast::*;
use super::base::Parser;
use super::token::{Token, TokenKind, QuoteKind};

impl <'a> Parser<'a> {
    pub(super) fn parse_template_parts(&mut self, open: Token, open_kind: QuoteKind) -> Reported<(Vec<TemplatePart>, SourceRange)> {
        let mut parts: Vec<TemplatePart> = Vec::new();
        let mut brace_stack: Vec<Token> = Vec::new();
        let close = loop {
            let tok = self.expect_poll()
                .map_err(|_| self.err_unmatched(open))?;
            match tok.kind {
                TokenKind::Quote(k, _) if k == open_kind => break tok,
                
                TokenKind::Newline |
                TokenKind::Verbatim(..) => {
                    self.err_unexpected_token(tok);
                },
                
                TokenKind::LBrace => {
                    brace_stack.push(tok);
                    self.skip_whitespace();
                },
                TokenKind::RBrace => {
                    if brace_stack.pop().is_none() {
                        self.err_unexpected_token(tok);
                    } else if matches!(parts.last(), Some(TemplatePart::Whitespace)) {
                        parts.pop();
                    }
                },
                
                TokenKind::VarName => {
                    if let Ok(name) = self.parse_name(tok) {
                        parts.push(TemplatePart::Name(name));
                    }
                },
                TokenKind::Escape => {
                    let c = self.unescape_char(tok);
                    parts.push(TemplatePart::LiteralChar(c));
                },
                TokenKind::Entity => {
                    let c = self.decode_entity(tok);
                    parts.push(TemplatePart::LiteralChar(c));
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
            self.err_unmatched(tok);
        }
        
        Ok((
            parts,
            open.range.to_end(close.range.end),
        ))
    }
}
