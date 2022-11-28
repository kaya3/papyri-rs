use indexmap::IndexMap;

use crate::errors::{ice_at, SyntaxError};
use crate::utils::{taginfo, SourceRange};
use super::ast::*;
use super::queue::Parser;
use super::token::{Token, TokenKind};

impl <'a> Parser<'a> {
    pub fn parse_match(&mut self, at: Token) -> Option<Match> {
        let value = self.parse_value()?;
        
        self.skip_whitespace();
        let open = self.expect_poll_kind(TokenKind::LBrace)?;
        let (branches, close) = self.parse_separated_until(
            &open,
            Parser::parse_match_branch,
            TokenKind::Comma,
            TokenKind::RBrace,
        )?;
        
        Some(Match {
            range: at.range.to_end(close.range.end),
            value,
            branches: branches.into_boxed_slice(),
        })
    }
    
    fn parse_match_branch(&mut self) -> Option<MatchBranch> {
        let pattern = self.parse_match_pattern()?;
        self.skip_whitespace();
        self.expect_poll_kind(TokenKind::Arrow)?;
        let then = self.parse_value()?;
        Some(MatchBranch {pattern, then})
    }
    
    fn parse_match_pattern(&mut self) -> Option<MatchPattern> {
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
            TokenKind::LPar => self.parse_dict_pattern(tok),
            TokenKind::Quote(..) => self.parse_template_pattern(tok),
            
            TokenKind::Name => {
                self.diagnostics.syntax_error(SyntaxError::PatternBareName, &tok.range);
                None
            },
            TokenKind::Asterisk => {
                self.diagnostics.syntax_error(SyntaxError::SpreadNotAllowed, &tok.range);
                None
            },
            _ => {
                self.diagnostics.err_unexpected_token(&tok);
                None
            },
        }
    }
    
    fn parse_positional_match_pattern(&mut self) -> Option<PositionalMatchPattern> {
        self.skip_whitespace();
        let asterisk = self.poll_if_kind(TokenKind::Asterisk);
        let pattern = self.parse_match_pattern()?;
        Some(match asterisk {
            Some(asterisk) => {
                if asterisk.range.len() != 1 {
                    self.diagnostics.syntax_error(SyntaxError::SpreadNamedNotAllowed, &asterisk.range);
                }
                PositionalMatchPattern::Spread(pattern)
            },
            None => PositionalMatchPattern::One(pattern),
        })
    }
    
    fn parse_named_match_pattern(&mut self, allow_lone_name: bool) -> Option<NamedMatchPattern> {
        self.skip_whitespace();
        let tok = self.expect_poll()?;
        match tok.kind {
            TokenKind::Name => {
                if tok.as_str().starts_with("_") {
                    self.diagnostics.syntax_error(SyntaxError::PatternNamedUnderscore, &tok.range);
                }
                self.skip_whitespace();
                let equals = if allow_lone_name { self.poll_if_kind(TokenKind::Equals) } else { self.expect_poll_kind(TokenKind::Equals) };
                let name_id = self.string_pool.insert(tok.as_str());
                let pattern = if equals.is_some() { self.parse_match_pattern()? } else { MatchPattern::LiteralNone(tok.range) };
                Some(NamedMatchPattern::One(name_id, pattern))
            },
            TokenKind::Asterisk => {
                if tok.range.len() != 2 {
                    self.diagnostics.syntax_error(SyntaxError::SpreadPositionalNotAllowed, &tok.range);
                }
                let pattern = self.parse_match_pattern()?;
                Some(NamedMatchPattern::Spread(pattern))
            },
            _ => {
                self.diagnostics.err_unexpected_token(&tok);
                None
            },
        }
    }
    
    fn parse_list_pattern(&mut self, lsqb: Token) -> Option<MatchPattern> {
        let (children, rsqb) = self.parse_separated_until(
            &lsqb,
            Parser::parse_positional_match_pattern,
            TokenKind::Comma,
            TokenKind::RSqb,
        )?;
        
        let mut child_patterns = Vec::new();
        let mut spread_index = None;
        
        for (i, child) in children.into_iter().enumerate() {
            let child = match child {
                PositionalMatchPattern::One(child) => child,
                PositionalMatchPattern::Spread(child) => {
                    if spread_index.is_some() {
                        self.diagnostics.syntax_error(SyntaxError::PatternMultipleSpreads, child.range())
                    }
                    spread_index = Some(i);
                    child
                },
            };
            child_patterns.push(child);
        }
        
        let range = lsqb.range.to_end(rsqb.range.end);
        let child_patterns = child_patterns.into_boxed_slice();
        Some(match spread_index {
            Some(spread_index) => MatchPattern::SpreadList(range, child_patterns, spread_index),
            None => MatchPattern::ExactList(range, child_patterns),
        })
    }
    
    fn parse_dict_pattern(&mut self, lpar: Token) -> Option<MatchPattern> {
        let (parts, rpar) = self.parse_separated_until(
            &lpar,
            |_self| _self.parse_named_match_pattern(false),
            TokenKind::Comma,
            TokenKind::RPar,
        )?;
        
        // cannot simplify `(**P)` to `P`, since the former checks that the value is a dictionary
        self.make_dict_pattern(parts, lpar.range.to_end(rpar.range.end), false)
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
        
        let (raw_attrs, rangle) = self.parse_separated_until(
            &langle,
            |_self| _self.parse_named_match_pattern(true),
            TokenKind::Whitespace,
            TokenKind::RAngle,
        )?;
        
        // can simplify `(**P)` to `P`, because the tag attributes are always a dictionary
        let attrs = self.make_dict_pattern(raw_attrs, langle.range.to_end(rangle.range.end), true)?;
        
        let self_closing = rangle.as_str() == "/>"
            || matches!(name, MatchPattern::LiteralName(_, name_id) if taginfo::is_self_closing(name_id));
        
        self.skip_whitespace();
        let (content, close_tag) = if self_closing {
            (MatchPattern::LiteralNone(rangle.range.clone()), rangle)
        } else if let Some(close_tag) = self.poll_if_kind(TokenKind::CloseTag) {
            (MatchPattern::LiteralNone(close_tag.range.clone()), close_tag)
        } else {
            let content_pattern = self.parse_match_pattern()?;
            self.skip_whitespace();
            let close_tag = self.expect_poll_kind(TokenKind::CloseTag)?;
            (content_pattern, close_tag)
        };
        
        if close_tag.kind == TokenKind::CloseTag && !close_tag.is_close_tag(&name_str) {
            self.diagnostics.syntax_error(SyntaxError::PatternIncorrectCloseTag, &close_tag.range);
        }
        
        Some(MatchPattern::Tag(
            langle.range.to_end(close_tag.range.end),
            Box::new(TagMatchPattern {name, attrs, content}),
        ))
    }
    
    fn parse_template_pattern(&mut self, open: Token) -> Option<MatchPattern> {
        let (parts, range) = self.parse_template_parts(open)?;
        
        let mut regex_str = "^(?:".to_string();
        let mut vars = Vec::new();
        for part in parts {
            match part {
                TemplatePart::Literal(range) => {
                    regex_str += &regex::escape(range.as_str());
                },
                TemplatePart::LiteralStr(s) => {
                    regex_str += &regex::escape(&s);
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
    
    fn make_dict_pattern(&mut self, parts: Vec<NamedMatchPattern>, range: SourceRange, simplify: bool) -> Option<MatchPattern> {
        let mut attrs = IndexMap::new();
        let mut spread = None;
        
        for part in parts.into_iter() {
            let part_range = part.range().clone();
            if spread.is_some() {
                self.diagnostics.syntax_error(SyntaxError::PatternNamedAfterSpread, &part_range);
                break;
            }
            match part {
                NamedMatchPattern::One(name_id, child) => {
                    if attrs.insert(name_id, child).is_some() {
                        let name = self.string_pool.get(name_id).to_string();
                        self.diagnostics.syntax_error(SyntaxError::PatternDuplicateName(name), &part_range);
                    }
                },
                NamedMatchPattern::Spread(child) => {
                    spread = Some(child);
                },
            }
        }
        
        if simplify && attrs.is_empty() && spread.is_some() {
            return spread;
        }
        
        Some(MatchPattern::Dict(range, Box::new(DictMatchPattern {
            attrs,
            spread,
        })))
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
