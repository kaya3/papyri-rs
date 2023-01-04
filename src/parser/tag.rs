use crate::errors::SyntaxError;
use crate::utils::{taginfo, str_ids, NameIDSet, NameID};
use super::ast::*;
use super::base::Parser;
use super::token::{Token, TokenKind};

impl <'a> Parser<'a> {
    pub(super) fn parse_tag(&mut self, langle: Token) -> Option<Expr> {
        let name_tok = self.expect_poll()?;
        let (name, name_id) = match name_tok.kind {
            TokenKind::Name => {
                let name_id = self.tok_lowercase_name_id(name_tok);
                (TagName::Literal(name_id), name_id)
            },
            TokenKind::VarName => {
                let var_name = self.parse_name(name_tok)?;
                (TagName::Name(var_name), str_ids::ANONYMOUS)
            },
            TokenKind::ExclamationMark => {
                let doctype = self.expect_poll()?;
                if !self.tok_str(doctype).eq_ignore_ascii_case("DOCTYPE") {
                    self.syntax_error(SyntaxError::TokenExpectedDoctype, doctype.range);
                    return None;
                }
                (TagName::Literal(str_ids::_DOCTYPE), str_ids::_DOCTYPE)
            },
            _ => self.ice_at("invalid open tag token", langle.range),
        };
        
        let (attrs, rangle) = self.parse_separated_until(
            langle,
            Parser::parse_tag_attribute,
            TokenKind::Whitespace,
            TokenKind::RAngle,
        )?;
        
        let mut names_used = NameIDSet::default();
        for attr in attrs.iter() {
            if let TagAttrOrSpread::Attr(attr) = attr {
                if !names_used.insert(attr.name_id) {
                    let name = self.string_pool.get(attr.name_id);
                    self.syntax_error(SyntaxError::TagDuplicateAttr(name), attr.range)
                }
            }
        }
        
        let mut tag = Tag {
            range: langle.range.to_end(rangle.range.end),
            name,
            attrs: attrs.into_boxed_slice(),
            children: Box::from([]),
        };
        
        let self_closing = self.tok_str(rangle) == "/>"
            || (!name_id.is_anonymous() && taginfo::is_self_closing(name_id));
        
        if !self_closing {
            let (children, Some(close)) = self.parse_nodes_until(|_self, tok| _self.is_close_tag(tok, name_id)) else {
                self.syntax_error(SyntaxError::TagUnmatchedOpen, tag.range);
                return None;
            };
            tag.range.end = close.range.end;
            tag.children = children.into_boxed_slice();
        }
        Some(Expr::Tag(Box::new(tag)))
    }
    
    fn parse_tag_attribute(&mut self) -> Option<TagAttrOrSpread> {
        self.skip_whitespace();
        let (spread_kind, _) = self.poll_if_spread(false, true);
        if spread_kind != SpreadKind::NoSpread {
            return self.parse_expr()
                .map(TagAttrOrSpread::Spread);
        }
        
        self.skip_whitespace();
        let name_tok = self.poll_if_kind(TokenKind::Name)?;
        let name_id = self.tok_name_id(name_tok);
        
        self.skip_whitespace();
        let question_mark = self.poll_if_kind(TokenKind::QuestionMark);
        
        self.skip_whitespace();
        if self.poll_if_kind(TokenKind::Equals).is_none() {
            return match question_mark {
                Some(q) => {
                    self.syntax_error(SyntaxError::TokenExpected(TokenKind::Equals), q.range);
                    None
                },
                None => Some(TagAttrOrSpread::Attr(TagAttribute {
                    range: name_tok.range,
                    name_id,
                    question_mark: false,
                    value: None,
                })),
            };
        }
        
        let value = self.parse_expr()?;
        let range = name_tok.range.to_end(value.range().end);
        Some(TagAttrOrSpread::Attr(TagAttribute {
            range,
            name_id,
            question_mark: question_mark.is_some(),
            value: Some(value),
        }))
    }
    
    /// Indicates whether this token is a closing tag for the given tag name.
    /// If no tag name is given, only `</>` matches.
    pub(super) fn is_close_tag(&self, tok: Token, name_id: NameID) -> bool {
        tok.kind == TokenKind::CloseTag && {
            let s = self.tok_str(tok);
            s == "</>" || (!name_id.is_anonymous() && self.string_pool.get(name_id).eq_ignore_ascii_case(&s[2..s.len() - 1]))
        }
    }
}
