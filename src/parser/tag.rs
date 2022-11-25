use indexmap::IndexSet;

use crate::errors::{ice_at, SyntaxError, RuntimeError};
use crate::utils::{NameID, taginfo};
use super::ast::*;
use super::queue::Parser;
use super::token::{Token, TokenKind};

impl <'a> Parser<'a> {
    pub fn parse_tag(&mut self, langle: Token) -> Option<AST> {
        let name_tok = self.expect_poll()?;
        let (name, name_str) = match name_tok.kind {
            TokenKind::Name => {
                let lc_name = name_tok.as_str().to_ascii_lowercase();
                (TagName::Literal(self.string_pool.insert(&lc_name)), Some(lc_name))
            },
            TokenKind::VarName => {
                let var_name = self.parse_var_name(name_tok);
                (TagName::Variable(var_name), None)
            },
            _ => ice_at("invalid open tag token", &langle.range),
        };
        
        let (attrs, rangle) = self.parse_separated_until(
            &langle,
            Parser::parse_tag_attribute,
            TokenKind::Whitespace,
            TokenKind::RAngle,
        )?;
        
        let mut names_used: IndexSet<NameID> = IndexSet::new();
        for attr in attrs.iter() {
            if let TagAttrOrSpread::Attr(attr) = attr {
                if !names_used.insert(attr.name_id) {
                    let name = self.string_pool.get(attr.name_id).to_string();
                    self.diagnostics.runtime_error(RuntimeError::AttrMultipleValues(name), &attr.range)
                }
            }
        }
        
        let mut tag = Tag {
            range: langle.range.to_end(rangle.range.end),
            name,
            attrs: attrs.into_boxed_slice(),
            children: Box::from([]),
        };
        
        let self_closing = rangle.as_str() == "/>"
            || matches!(tag.name, TagName::Literal(name_id) if taginfo::is_self_closing(name_id));
        
        if !self_closing {
            let (children, Some(close)) = self.parse_nodes_until(|tok| tok.is_close_tag(&name_str)) else {
                self.diagnostics.syntax_error(SyntaxError::TagUnmatchedOpen, &tag.range);
                return None;
            };
            tag.range.end = close.range.end;
            tag.children = children.into_boxed_slice();
        }
        Some(AST::Tag(Box::new(tag)))
    }
    
    fn parse_tag_attribute(&mut self) -> Option<TagAttrOrSpread> {
        self.skip_whitespace();
        if let Some(spread) = self.poll_if_kind(TokenKind::Asterisk) {
            if spread.range.len() != 2 {
                self.diagnostics.syntax_error(SyntaxError::SpreadPositionalNotAllowed, &spread.range);
            }
            return self.parse_value()
                .map(TagAttrOrSpread::Spread);
        }
        
        self.skip_whitespace();
        let name = self.poll_if_kind(TokenKind::Name)?;
        let mut range = name.range.clone();
        let name_id = self.string_pool.insert(name.as_str());
        
        self.skip_whitespace();
        let question_mark = self.poll_if_kind(TokenKind::QuestionMark);
        
        self.skip_whitespace();
        if self.poll_if_kind(TokenKind::Equals).is_none() {
            return match question_mark {
                Some(q) => {
                    self.diagnostics.syntax_error(SyntaxError::TokenExpected(TokenKind::Equals), &q.range);
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
