use indexmap::{IndexMap, IndexSet};

use crate::errors::{ice_at, SyntaxError};
use crate::utils::{taginfo, NameID};
use crate::utils::sourcefile::SourceRange;
use super::ast::*;
use super::base::Parser;
use super::token::{Token, TokenKind};

#[derive(Debug)]
enum PositionalMatchPattern {
    One(MatchPattern),
    Spread(MatchPattern),
}

#[derive(Debug)]
enum NamedMatchPattern {
    One(NameID, MatchPattern),
    Spread(MatchPattern),
}

impl NamedMatchPattern {
    fn range(&self) -> SourceRange {
        match self {
            NamedMatchPattern::One(_, p) |
            NamedMatchPattern::Spread(p) => p.range(),
        }
    }
}

impl <'a> Parser<'a> {
    pub(super) fn parse_match(&mut self, at: Token) -> Option<Match> {
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
    
    fn parse_match_branch(&mut self) -> Option<(MatchPattern, AST)> {
        let pattern = self.parse_match_pattern()?;
        self.skip_whitespace();
        self.expect_poll_kind(TokenKind::Arrow)?;
        let then = self.parse_value()?;
        Some((pattern, then))
    }
    
    fn parse_match_pattern(&mut self) -> Option<MatchPattern> {
        let mut p = self.parse_and_match_pattern()?;
        while {self.skip_whitespace(); self.poll_if_kind(TokenKind::Bar).is_some()} {
            let q = self.parse_and_match_pattern()?;
            
            let mut name_ids = IndexSet::new();
            p.get_name_ids(&mut name_ids);
            q.get_name_ids(&mut name_ids);
            let name_ids = name_ids.into_iter().collect();
            
            let pair = (p, q);
            p = MatchPattern::Or(Box::new(pair), name_ids);
        }
        Some(p)
    }
    
    fn parse_and_match_pattern(&mut self) -> Option<MatchPattern> {
        let mut p = self.parse_primary_match_pattern()?;
        while {self.skip_whitespace(); self.poll_if_kind(TokenKind::Ampersand).is_some()} {
            let pair = (p, self.parse_primary_match_pattern()?);
            p = MatchPattern::And(Box::new(pair));
        }
        Some(p)
    }
    
    fn parse_primary_match_pattern(&mut self) -> Option<MatchPattern> {
        self.skip_whitespace();
        let tok = self.expect_poll()?;
        match tok.kind {
            _ if self.tok_str(&tok) == "_" => {
                let p = MatchPattern::Ignore(tok.range);
                self.parse_optional_typed_pattern(p)
            },
            TokenKind::VarName => {
                let p = self.parse_name_pattern(tok)?;
                self.parse_optional_typed_pattern(MatchPattern::VarName(p))
            },
            
            TokenKind::Dot => Some(MatchPattern::LiteralNone(tok.range)),
            
            TokenKind::Boolean(..) |
            TokenKind::Number |
            TokenKind::Verbatim(..) => Some(MatchPattern::Literal(tok)),
            
            TokenKind::Equals => self.parse_value().map(MatchPattern::EqualsValue),
            TokenKind::LAngle => self.parse_tag_pattern(tok),
            TokenKind::LSqb => self.parse_seq_pattern(tok, TokenKind::Comma, TokenKind::RSqb).map(|(p, _)| p),
            TokenKind::LPar => self.parse_dict_pattern(tok),
            TokenKind::LBrace => self.parse_seq_pattern(tok, TokenKind::Whitespace, TokenKind::RBrace).map(|(p, _)| p),
            TokenKind::Quote(..) => self.parse_template_pattern(tok),
            
            TokenKind::Name => {
                self.syntax_error(SyntaxError::PatternBareName, tok.range);
                None
            },
            TokenKind::Asterisk => {
                self.syntax_error(SyntaxError::SpreadPositionalNotAllowed, tok.range);
                None
            },
            TokenKind::DoubleAsterisk => {
                self.syntax_error(SyntaxError::SpreadNamedNotAllowed, tok.range);
                None
            },
            _ => {
                self.err_unexpected_token(&tok);
                None
            },
        }
    }
    
    fn parse_positional_match_pattern(&mut self) -> Option<PositionalMatchPattern> {
        self.skip_whitespace();
        let spread = self.poll_if_spread(true, false).is_some();
        let pattern = self.parse_match_pattern()?;
        Some(if spread {
            PositionalMatchPattern::Spread(pattern)
        } else {
            PositionalMatchPattern::One(pattern)
        })
    }
    
    fn parse_named_match_pattern(&mut self, allow_lone_name: bool) -> Option<NamedMatchPattern> {
        self.skip_whitespace();
        if self.poll_if_spread(false, true).is_some() {
            return self.parse_match_pattern()
               .map(NamedMatchPattern::Spread);
        }
        
        let name_tok = self.expect_poll_kind(TokenKind::Name)?;
        if self.tok_str(&name_tok).starts_with('_') {
            self.syntax_error(SyntaxError::PatternNamedUnderscore, name_tok.range);
        }
        self.skip_whitespace();
        let equals = if allow_lone_name { self.poll_if_kind(TokenKind::Equals) } else { self.expect_poll_kind(TokenKind::Equals) };
        let name_id = self.tok_name_id(&name_tok, false);
        let pattern = if equals.is_some() { self.parse_match_pattern()? } else { MatchPattern::LiteralNone(name_tok.range) };
        Some(NamedMatchPattern::One(name_id, pattern))
    }
    
    fn parse_seq_pattern(&mut self, open: Token, sep_kind: TokenKind, close_kind: TokenKind) -> Option<(MatchPattern, Token)> {
        let (children, close) = self.parse_separated_until(
            &open,
            Parser::parse_positional_match_pattern,
            sep_kind,
            close_kind,
        )?;
        
        let mut spread_index = None;
        let child_patterns: Box<[MatchPattern]> = children.into_iter()
            .enumerate()
            .map(|(i, child)| match child {
                PositionalMatchPattern::One(child) => child,
                PositionalMatchPattern::Spread(child) => {
                    if spread_index.is_some() {
                        self.syntax_error(SyntaxError::PatternMultipleSpreads, child.range())
                    }
                    spread_index = Some(i as u32);
                    child
                }
            })
            .collect();
        
        let is_list = open.kind == TokenKind::LSqb;
        if !is_list {
            for child in child_patterns.iter().filter(|p| !p.can_match_html()) {
                self.syntax_error(SyntaxError::PatternCannotMatchHTML, child.range());
            }
        }
        
        let range = open.range.to_end(close.range.end);
        let pattern = match spread_index {
            Some(spread_index) => if is_list {
                MatchPattern::SpreadList(range, child_patterns, spread_index)
            } else {
                MatchPattern::SpreadHTMLSeq(range, child_patterns, spread_index)
            },
            None => if is_list {
                MatchPattern::ExactList(range, child_patterns)
            } else if child_patterns.is_empty() {
                MatchPattern::LiteralNone(range)
            } else {
                MatchPattern::ExactHTMLSeq(range, child_patterns)
            },
        };
        Some((pattern, close))
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
                if self.tok_str(&name_tok) == "_" {
                    (MatchPattern::Ignore(name_tok.range), None)
                } else {
                    let name_str = self.tok_str(&name_tok).to_ascii_lowercase();
                    let name_id = self.string_pool.insert(&name_str);
                    (MatchPattern::LiteralName(name_tok.range, name_id), Some(name_str))
                }
            },
            TokenKind::VarName => {
                let name = self.parse_name_pattern(name_tok)?;
                (MatchPattern::VarName(name), None)
            },
            _ => {
                self.err_unexpected_token(&name_tok);
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
        
        let self_closing = self.tok_str(&rangle) == "/>"
            || matches!(name, MatchPattern::LiteralName(_, name_id) if taginfo::is_self_closing(name_id));
        
        self.skip_whitespace();
        let (content, close_tag) = if self_closing {
            (MatchPattern::LiteralNone(rangle.range), rangle)
        } else if let Some(close_tag) = self.poll_if_kind(TokenKind::CloseTag) {
            (MatchPattern::LiteralNone(close_tag.range), close_tag)
        } else {
            let content = self.parse_match_pattern()?;
            self.skip_whitespace();
            let close_tag = self.expect_poll_kind(TokenKind::CloseTag)?;
            (content, close_tag)
        };
        
        if close_tag.kind == TokenKind::CloseTag && !self.is_close_tag(&close_tag, &name_str) {
            self.syntax_error(SyntaxError::PatternIncorrectCloseTag, close_tag.range);
        }
        
        Some(MatchPattern::Tag(
            langle.range.to_end(close_tag.range.end),
            Box::new(TagMatchPattern {name, attrs, content}),
        ))
    }
    
    fn parse_template_pattern(&mut self, open: Token) -> Option<MatchPattern> {
        let (parts, range) = self.parse_template_parts(open)?;
        
        let mut regex_str = "^(?s:".to_string();
        let mut vars = Vec::new();
        for part in parts {
            match part {
                TemplatePart::Literal(range) => {
                    regex_str += &regex::escape(self.src.get_span(range));
                },
                TemplatePart::LiteralStr(s) => {
                    regex_str += &regex::escape(&s);
                },
                TemplatePart::Name(Name::SimpleName(var)) => {
                    regex_str += "(.+?)";
                    vars.push(var);
                },
                TemplatePart::Name(Name::AttrName(attr)) => {
                    self.syntax_error(SyntaxError::PatternAttrAccess, attr.range);
                },
                TemplatePart::Whitespace => {
                    regex_str += r"\s+";
                },
            }
        }
        regex_str += ")$";
        
        match regex::Regex::new(&regex_str) {
            Ok(regex) => {
                let p = RegexMatchPattern {regex, names: vars.into_boxed_slice()};
                Some(MatchPattern::Regex(range, Box::new(p)))
            },
            Err(e) => ice_at(&format!("regex {regex_str} failed to parse: {e}"), range),
        }
    }
    
    fn parse_name_pattern(&mut self, token: Token) -> Option<SimpleName> {
        match self.parse_name(token)? {
            Name::SimpleName(name) => Some(name),
            Name::AttrName(attr) => {
                self.syntax_error(SyntaxError::PatternAttrAccess, attr.range);
                // try to recover from the error
                Some(attr.get_root())
            },
        }
    }
    
    fn make_dict_pattern(&mut self, parts: Vec<NamedMatchPattern>, range: SourceRange, simplify: bool) -> Option<MatchPattern> {
        let mut attrs = IndexMap::new();
        let mut spread = None;
        
        for part in parts.into_iter() {
            let part_range = part.range();
            match part {
                NamedMatchPattern::One(name_id, child) => {
                    if spread.is_some() {
                        self.syntax_error(SyntaxError::PatternNamedAfterSpread, part_range);
                    } else if attrs.insert(name_id, child).is_some() {
                        let name = self.string_pool.get(name_id).to_string();
                        self.syntax_error(SyntaxError::PatternDuplicateName(name), part_range);
                    }
                },
                NamedMatchPattern::Spread(child) => {
                    if spread.is_some() {
                        self.syntax_error(SyntaxError::ParamMultipleSpread, part_range);
                    }
                    spread = Some(child);
                },
            }
        }
        
        if simplify && attrs.is_empty() && spread.is_some() {
            return spread;
        }
        
        Some(MatchPattern::Dict(
            range,
            Box::new(DictMatchPattern {attrs, spread}),
        ))
    }
    
    fn parse_optional_typed_pattern(&mut self, pattern: MatchPattern) -> Option<MatchPattern> {
        self.skip_whitespace();
        if self.poll_if_kind(TokenKind::Colon).is_none() {
            return Some(pattern);
        }
        
        self.skip_whitespace();
        let type_pattern = if let Some(var) = self.poll_if_kind(TokenKind::VarName) {
            let var = self.parse_name_pattern(var)?;
            MatchPattern::TypeOf(var)
        } else {
            let type_ = self.parse_type()?;
            MatchPattern::Typed(type_)
        };
        
        Some(if matches!(pattern, MatchPattern::Ignore(..)) {
            type_pattern
        } else {
            let pair = (pattern, type_pattern);
            MatchPattern::And(Box::new(pair))
        })
    }
}
