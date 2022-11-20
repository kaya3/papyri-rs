use indexmap::IndexMap;

use crate::utils::{taginfo, ice_at, ice};
use super::ast::*;
use super::queue::Parser;
use super::token::{Token, TokenKind};
use super::text;

impl <'a> Parser<'a> {
    pub fn parse_match(&mut self, at: Token) -> Option<Match> {
        let Some(value) = self.parse_value() else {
            self.diagnostics.syntax_error("expected argument", &at.range);
            return None;
        };
        
        self.skip_whitespace();
        let open = self.expect_poll_kind(TokenKind::LBrace)?;
        let (branches, Some(close)) = self.parse_comma_separated_until(
            &open,
            |_self| _self.parse_match_branch(),
            TokenKind::RBrace,
        ) else {
            return None;
        };
        
        Some(Match {
            range: at.range.to_end(close.range.end),
            value,
            branches: branches.into_boxed_slice(),
        })
    }
    
    fn parse_match_branch(&mut self) -> Option<MatchBranch> {
        let pattern = self.parse_match_pattern(SpreadKind::NoSpread)?;
        self.skip_whitespace();
        self.expect_poll_kind(TokenKind::Arrow)?;
        let then = self.parse_value()?;
        Some(MatchBranch {pattern, then})
    }
    
    fn parse_match_pattern(&mut self, allow_spread: SpreadKind) -> Option<MatchPattern> {
        self.skip_whitespace();
        let tok = self.expect_poll()?;
        match tok.kind {
            _ if tok.as_str() == "_" => {
                let p = MatchPattern::Ignore(tok.range);
                self.parse_optional_typed_pattern(p)
            },
            TokenKind::VarName => {
                let p = MatchPattern::VarName(self.parse_var_name(tok));
                self.parse_optional_typed_pattern(p)
            },
            
            TokenKind::Dot |
            TokenKind::Boolean |
            TokenKind::Number |
            TokenKind::Verbatim => Some(MatchPattern::Literal(tok)),
            
            TokenKind::LAngle => self.parse_tag_pattern(tok),
            TokenKind::LSqb => self.parse_list_pattern(tok),
            TokenKind::Quote(..) => self.parse_template_pattern(tok),
            TokenKind::Asterisk => self.parse_spread_match_pattern(tok, allow_spread),
            
            TokenKind::Name => {
                self.diagnostics.syntax_error("pattern cannot be bare name; use $ for variable name, or backticks for string literal", &tok.range);
                None
            },
            _ => {
                self.diagnostics.err_unexpected_token(&tok);
                None
            },
        }
    }
    
    fn parse_list_pattern(&mut self, lsqb: Token) -> Option<MatchPattern> {
        let (children, Some(rsqb)) = self.parse_comma_separated_until(
            &lsqb,
            |_self| _self.parse_match_pattern(SpreadKind::Positional),
            TokenKind::RSqb,
        ) else {
            return None;
        };
        
        let mut child_patterns = Vec::new();
        let mut spread_index = None;
        
        for (i, child) in children.into_iter().enumerate() {
            if child.is_spread() {
                if spread_index.is_some() {
                    self.diagnostics.syntax_error("cannot have multiple spreads", child.range())
                }
                spread_index = Some(i);
            }
            child_patterns.push(child);
        }
        let child_patterns = child_patterns.into_boxed_slice();
        
        let range = lsqb.range.to_end(rsqb.range.end);
        Some(match spread_index {
            Some(spread_index) => MatchPattern::SpreadList(range, child_patterns, spread_index),
            None => MatchPattern::ExactList(range, child_patterns),
        })
    }
    
    fn parse_tag_pattern(&mut self, langle: Token) -> Option<MatchPattern> {
        let name_tok = self.expect_poll()?;
        let (name, name_str) = match name_tok.kind {
            TokenKind::Name => {
                if name_tok.as_str() == "_" {
                    (MatchPattern::Ignore(name_tok.range), None)
                } else {
                    let name_str = name_tok.as_str().to_ascii_lowercase();
                    let name_id = self.string_pool.insert(&name_str);
                    (MatchPattern::LiteralName(name_tok.range, name_id), Some(name_str))
                }
            },
            TokenKind::VarName => {
                (MatchPattern::VarName(self.parse_var_name(name_tok)), None)
            },
            _ => {
                self.diagnostics.err_unexpected_token(&name_tok);
                return None;
            },
        };
        
        let mut attrs = IndexMap::new();
        let mut spread = None;
        let rangle = loop {
            self.skip_whitespace();
            let tok = self.expect_poll()?;
            
            match tok.kind {
                TokenKind::RAngle | TokenKind::RAngleSlash => break tok,
                TokenKind::Name => {
                    if spread.is_some() {
                        self.diagnostics.syntax_error("named attribute pattern cannot occur after spread pattern", &tok.range);
                    }
                    let attr_name_id = self.string_pool.insert(tok.as_str());
                    
                    self.skip_whitespace();
                    self.expect_poll_kind(TokenKind::Equals)?;
                    self.skip_whitespace();
                    let attr_pattern = self.parse_match_pattern(SpreadKind::NoSpread)?;
                    if attrs.insert(attr_name_id, attr_pattern).is_some() {
                        self.diagnostics.syntax_error(&format!("repeated attribute '{}'", tok.as_str()), &tok.range);
                    }
                },
                TokenKind::Asterisk => {
                    if spread.is_some() {
                        self.diagnostics.syntax_error("cannot have multiple spreads", &tok.range);
                    }
                    spread = self.parse_spread_match_pattern(tok, SpreadKind::Named);
                    if spread.is_none() { return None; }
                },
                _ => {
                    self.diagnostics.err_unexpected_token(&tok);
                    return None;
                },
            }
        };
        
        let self_closing = rangle.kind == TokenKind::RAngleSlash
            || matches!(name, MatchPattern::LiteralName(_, name_id) if taginfo::is_self_closing(name_id));
        
        self.skip_whitespace();
        let (content, close_tag) = if self_closing {
            (MatchPattern::EmptyHTML(rangle.range.clone()), rangle)
        } else if let Some(close_tag) = self.poll_if_kind(TokenKind::CloseTag) {
            (MatchPattern::EmptyHTML(close_tag.range.clone()), close_tag)
        } else {
            let content_pattern = self.parse_match_pattern(SpreadKind::NoSpread)?;
            self.skip_whitespace();
            let close_tag = self.expect_poll_kind(TokenKind::CloseTag)?;
            (content_pattern, close_tag)
        };
        
        if close_tag.kind == TokenKind::CloseTag && !close_tag.is_close_tag(&name_str) {
            self.diagnostics.syntax_error("non-matching close tag; write </> for unnamed tag", &close_tag.range);
        }
        
        Some(MatchPattern::Tag(
            langle.range.to_end(close_tag.range.end),
            Box::new(TagMatchPattern {name, attrs, spread, content}),
        ))
    }
    
    fn parse_template_pattern(&mut self, open: Token) -> Option<MatchPattern> {
        let AST::Template(parts, range) = self.parse_template(open)? else {
            ice("parse did not return template");
        };
        
        let mut regex_str = "^(?:".to_string();
        let mut vars = Vec::new();
        for part in parts.into_vec() {
            match part {
                TemplatePart::Literal(range) => {
                    regex_str += &regex::escape(range.as_str());
                },
                TemplatePart::Escape(range) => {
                    let c = text::unescape_char(&range, self.diagnostics).to_string();
                    regex_str += &regex::escape(&c);
                },
                TemplatePart::Entity(range) => {
                    let c = text::decode_entity(&range, self.diagnostics);
                    regex_str += &regex::escape(&c);
                },
                TemplatePart::VarName(var) => {
                    regex_str += "(.+)";
                    vars.push(var);
                },
                TemplatePart::Whitespace => {
                    regex_str += r"\s+";
                },
            }
        }
        regex_str += ")$";
        
        match regex::Regex::new(&regex_str) {
            Ok(r) => Some(MatchPattern::Regex(range, r, vars.into_boxed_slice())),
            Err(e) => ice_at(&e.to_string(), &range),
        }
    }
    
    fn parse_spread_match_pattern(&mut self, asterisk: Token, allow_spread: SpreadKind) -> Option<MatchPattern> {
        self.skip_whitespace();
        let name = self.poll_if(|t| t.as_str() == "_")
            .or_else(|| self.expect_poll_kind(TokenKind::VarName))?;
        let range = asterisk.range.to_end(name.range.end);
        
        let p = match (asterisk.range.len(), allow_spread) {
            (1, SpreadKind::Positional) | (2, SpreadKind::Named) => if name.as_str() == "_" {
                MatchPattern::SpreadIgnore(range)
            } else {
                MatchPattern::SpreadVarName(self.parse_var_name(name))
            },
            (len, _) => {
                let msg = if len == 1 { "positional spread not allowed here" } else { "named spread not allowed here" };
                self.diagnostics.syntax_error(msg, &asterisk.range);
                return None;
            },
        };
        self.parse_optional_typed_pattern(p)
    }
    
    fn parse_optional_typed_pattern(&mut self, pattern: MatchPattern) -> Option<MatchPattern> {
        self.skip_whitespace();
        if self.poll_if_kind(TokenKind::Colon).is_none() {
            return Some(pattern);
        }
        
        self.skip_whitespace();
        if let Some(var) = self.poll_if_kind(TokenKind::VarName) {
            let range = pattern.range().to_end(var.range.end);
            Some(MatchPattern::TypeOf(
                range,
                Box::new(pattern),
                self.parse_var_name(var),
            ))
        } else {
            let type_ = self.parse_type()?;
            let range = pattern.range().to_end(type_.range().end);
            Some(MatchPattern::Typed(
                range,
                Box::new(pattern),
                type_,
            ))
        }
    }
}
