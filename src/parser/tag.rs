use std::collections::HashSet;

use crate::utils::{ice_at, NameID, taginfo};

use super::ast::*;
use super::queue::Parser;
use super::token::{Token, TokenKind};

impl <'a> Parser<'a> {
    pub fn parse_tag(&mut self, langle: Token) -> Option<AST> {
        let name_tok = self.expect_poll()?;
        let (name, name_str) = match name_tok.kind {
            TokenKind::Name => {
                let lc_name = name_tok.as_str().to_ascii_lowercase();
                (TagName::Fixed(self.string_pool.insert(&lc_name)), Some(lc_name))
            },
            TokenKind::VarName => {
                let var_name = self.parse_var_name(name_tok);
                (TagName::Variable(var_name), None)
            },
            _ => ice_at("invalid open tag token", &langle.range),
        };
        
        let mut attrs = Vec::new();
        let mut names_used: HashSet<NameID> = HashSet::new();
        while let Some(attr) = self.parse_tag_attribute() {
            if let TagAttrOrSpread::Attr(attr) = &attr {
                if !names_used.insert(attr.name_id) {
                    self.diagnostics.error(&format!("repeated attribute '{}'", self.string_pool.get(attr.name_id)), &attr.range)
                }
            }
            attrs.push(attr);
        }
        
        let Some(rangle) = self.poll_if_kind(TokenKind::RAngle)
            .or_else(|| self.poll_ignoring_whitespace_if_kind(TokenKind::RAngleSlash))
            else {
                self.diagnostics.syntax_error("expected '>' or '/>'", &langle.range);
                return None;
            };
        
        let mut tag = Tag {
            range: langle.range.to_end(rangle.range.end),
            name,
            attrs: attrs.into_boxed_slice(),
            children: Box::from([]),
        };
        
        let self_closing = rangle.kind == TokenKind::RAngleSlash
            || matches!(tag.name, TagName::Fixed(name_id) if taginfo::is_self_closing(name_id));
        
        if !self_closing {
            let (children, Some(close)) = self.parse_nodes_until(|tok| tok.is_close_tag(&name_str)) else {
                self.diagnostics.syntax_error("unmatched opening tag", &tag.range);
                return None;
            };
            tag.range.end = close.range.end;
            tag.children = children.into_boxed_slice();
        }
        Some(AST::Tag(Box::new(tag)))
    }
    
    fn parse_tag_attribute(&mut self) -> Option<TagAttrOrSpread> {
        if let Some(spread) = self.poll_ignoring_whitespace_if_kind(TokenKind::Asterisk) {
            if spread.range.len() != 2 {
                self.diagnostics.syntax_error("positional spread not allowed here", &spread.range);
            }
            return self.parse_value()
                .map(TagAttrOrSpread::Spread);
        }
        
        let name = self.poll_ignoring_whitespace_if_kind(TokenKind::Name)?;
        let mut range = name.range.clone();
        let name_id = self.string_pool.insert(name.as_str());
        let question_mark = self.poll_ignoring_whitespace_if_kind(TokenKind::QuestionMark);
        if self.poll_ignoring_whitespace_if_kind(TokenKind::Equals).is_none() {
            return match question_mark {
                Some(q) => {
                    self.diagnostics.syntax_error("expected '=' after '?'", &q.range);
                    None
                },
                None => Some(TagAttrOrSpread::Attr(TagAttribute {
                    range,
                    name_id,
                    question_mark: false,
                    value: None,
                })),
            };
        }
        
        let value = self.parse_value()?;
        range.end = value.range().end;
        Some(TagAttrOrSpread::Attr(TagAttribute {
            range,
            name_id,
            question_mark: question_mark.is_some(),
            value: Some(value),
        }))
    }
}
