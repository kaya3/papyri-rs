use std::collections::HashSet;
use std::rc::Rc;

use super::ast::*;
use super::text::text_substitutions;
use crate::utils::{Diagnostics, ice_at, SourceFile, StringPool, NameID, str_ids, taginfo};
use super::queue::TokenQueue;
use super::token::{TokenKind, Token};

pub fn parse(src: Rc<SourceFile>, diagnostics: &mut Diagnostics, string_pool: &mut StringPool) -> Vec<AST> {
    let mut q = TokenQueue::new(src, diagnostics, string_pool);
    q.parse_root()
}

impl <'a> TokenQueue<'a> {
    fn parse_root(&mut self) -> Vec<AST> {
        let (nodes, _) = self.parse_nodes_until(|_tok| false);
        nodes
    }
    
    fn parse_nodes_until(&mut self, closer: impl Fn(&Token) -> bool) -> (Vec<AST>, Option<Token>) {
        self.skip_whitespace();
        
        let mut nodes = Vec::new();
        let end = loop {
            let Some(tok) = self.poll() else {
                break None;
            };
            if closer(&tok) {
                break Some(tok);
            } else if let Some(child) = self.parse_node(tok) {
                nodes.push(child);
            }
        };
        
        while nodes.last().filter(|node| node.is_whitespace()).is_some() {
            nodes.pop();
        }
        (nodes, end)
    }
    
    fn parse_comma_separated_until<T>(&mut self, open: &Token, parse: impl Fn(&mut Self) -> Option<T>, close_kind: TokenKind) -> (Vec<T>, Option<Token>) {
        let mut children = Vec::new();
        let mut expect_end = false;
        let close = loop {
            self.skip_whitespace();
            let close = if expect_end { self.expect_poll_kind(close_kind) } else { self.poll_if_kind(close_kind) };
            if close.is_some() {
                break close;
            }
            if let Some(child) = parse(self) {
                children.push(child);
            } else {
                // error
                break None;
            }
            self.skip_whitespace();
            expect_end = self.poll_if_kind(TokenKind::Comma).is_none();
        };
        
        if close.is_none() {
            self.diagnostics.err_unmatched(open);
        }
        (children, close)
    }
    
    fn parse_value(&mut self) -> Option<AST> {
        self.skip_whitespace();
        let tok = self.expect_poll()?;
        
        match tok.kind {
            TokenKind::Dot |
            TokenKind::Boolean |
            TokenKind::Name |
            TokenKind::Number => Some(AST::LiteralValue(tok)),
            
            TokenKind::Verbatim => Some(AST::Verbatim(tok)),
            TokenKind::VarName => Some(AST::VarName(self.parse_var_name(tok))),
            TokenKind::FuncName => self.parse_func(tok),
            TokenKind::OpenTag => self.parse_tag(tok),
            TokenKind::Ellipsis => Some(self.parse_ellipsis_group(tok)),
            TokenKind::LBrace => Some(self.parse_group(tok)),
            TokenKind::LSqb => Some(self.parse_list(tok)),
            TokenKind::LDblQuote => Some(self.parse_template(tok)),
            
            _ => {
                self.diagnostics.syntax_error("expected value", &tok.range);
                None
            }
        }
    }
    
    fn parse_node(&mut self, tok: Token) -> Option<AST> {
        match tok.kind {
            TokenKind::Verbatim => Some(AST::Verbatim(tok)),
            TokenKind::VarName => Some(AST::VarName(self.parse_var_name(tok))),
            TokenKind::FuncName => self.parse_func(tok),
            
            TokenKind::OpenTag => self.parse_tag(tok),
            TokenKind::LBrace => Some(self.parse_group(tok)),
            TokenKind::LSqb => Some(self.parse_list(tok)),
            
            TokenKind::CloseTag |
            TokenKind::RBrace |
            TokenKind::RSqb => {
                self.diagnostics.err_unexpected_token(&tok);
                None
            },
            
            TokenKind::Undetermined |
            TokenKind::Comment |
            TokenKind::LAngleComment |
            TokenKind::RAngleComment => {
                ice_at("token kind should not occur here", &tok.range)
            },
            
            TokenKind::Whitespace => Some(AST::Whitespace(tok.range)),
            TokenKind::Newline => Some(AST::ParagraphBreak(tok.range)),
            TokenKind::Escape => Some(AST::Escape(tok.range)),
            TokenKind::Entity => Some(AST::Entity(tok.range)),
            _ => Some(self.parse_text(tok)),
        }
    }
    
    fn parse_tag(&mut self, langle: Token) -> Option<AST> {
        let mut attrs = Vec::new();
        
        let mut names_used: HashSet<NameID> = HashSet::new();
        while let Some((name, attr)) = self.parse_tag_attribute() {
            if !names_used.insert(attr.name_id) {
                self.diagnostics.error(&format!("repeated attribute '{}'", name.as_str()), &name.range)
            }
            attrs.push(attr);
        }
        
        let Some(rangle) = self.poll_if_kind(TokenKind::RAngle)
            .or_else(|| self.poll_ignoring_whitespace_if_kind(TokenKind::RAngleSlash))
            else {
                self.diagnostics.syntax_error("expected '>' or '/>'", &langle.range);
                return None;
            };
        
        let tag_name = langle.get_open_tag_name().to_ascii_lowercase();
        let mut tag = Tag {
            range: langle.range.to_end(rangle.range.end),
            name_id: self.string_pool.insert(&tag_name),
            attrs: attrs.into_boxed_slice(),
            children: Box::from([]),
        };
        
        if rangle.kind != TokenKind::RAngleSlash && !taginfo::is_self_closing(tag.name_id) {
            let (children, Some(close)) = self.parse_nodes_until(|tok| tok.is_close_tag(&tag_name)) else {
                self.diagnostics.syntax_error("unmatched opening tag", &tag.range);
                return None;
            };
            tag.range.end = close.range.end;
            tag.children = children.into_boxed_slice();
        }
        Some(AST::Tag(Box::new(tag)))
    }
    
    fn parse_tag_attribute(&mut self) -> Option<(Token, TagAttribute)> {
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
                None => Some((name, TagAttribute {
                    range,
                    name_id,
                    question_mark: false,
                    value: None,
                })),
            };
        }
        
        let value = self.parse_value()?;
        range.end = value.range().end;
        Some((name, TagAttribute {
            range,
            name_id,
            question_mark: question_mark.is_some(),
            value: Some(value),
        }))
    }
    
    fn parse_func(&mut self, at: Token) -> Option<AST> {
        if at.as_str() == "@fn" {
            self.parse_func_def(at)
                .map(Box::new)
                .map(AST::FuncDef)
        } else if at.as_str() == "@match" {
            self.parse_match(at)
                .map(Box::new)
                .map(AST::Match)
        } else {
            self.parse_func_call(at)
                .map(Box::new)
                .map(AST::FuncCall)
        }
    }
    
    fn parse_func_def(&mut self, at: Token) -> Option<FuncDef> {
        self.skip_whitespace();
        let name_id = self.poll_if_kind(TokenKind::Name)
            .map(|t| self.string_pool.insert(t.as_str()))
            .unwrap_or(str_ids::ANONYMOUS);
        let signature = self.parse_signature()?;
        self.skip_whitespace();
        self.expect_poll_kind(TokenKind::Arrow)?;
        let body = Rc::new(self.parse_value()?);
        Some(FuncDef {
            range: at.range.to_end(body.range().end),
            name_id,
            signature,
            body,
        })
    }
    
    fn parse_match(&mut self, at: Token) -> Option<Match> {
        let Some(args) = self.parse_args() else {
            self.diagnostics.syntax_error("expected argument", &at.range);
            return None;
        };
        if args.len() != 1 {
            self.diagnostics.syntax_error(&format!("expected exactly 1 argument, was {}", args.len()), &at.range);
            return None;
        } else if args[0].is_spread() {
            self.diagnostics.syntax_error("@match argument must not be spread", &args[0].range);
            return None;
        } else if !args[0].is_positional() {
            self.diagnostics.syntax_error("@match argument must be positional, not named", &args[0].range);
            return None;
        }
        
        self.skip_whitespace();
        let open = self.expect_poll_kind(TokenKind::LBrace)?;
        let (branches, close) = self.parse_match_branches(open)?;
        Some(Match {
            range: at.range.to_end(close.range.end),
            value: args.into_iter().next().unwrap().value,
            branches,
        })
    }
    
    fn parse_func_call(&mut self, at: Token) -> Option<FuncCall> {
        let name_id = self.string_pool.insert(at.get_func_name());
        let args = self.parse_args()?.into_boxed_slice();
        let content = self.parse_value()?;
        Some(FuncCall {
            range: at.range.to_end(content.range().end),
            name_id,
            args,
            content,
        })
    }
    
    fn parse_signature(&mut self) -> Option<Signature> {
        let mut positional_params: Vec<Param> = Vec::new();
        let mut spread_param: Option<Param> = None;
        let mut named_params: Vec<Param> = Vec::new();
        let mut spread_named_param: Option<Param> = None;
        
        self.skip_whitespace();
        let lpar = self.poll_if_kind(TokenKind::LPar);
        if let Some(lpar) = &lpar {
            let (raw_params, rpar) = self.parse_comma_separated_until(
                lpar,
                |_self| _self.parse_param(),
                TokenKind::RPar,
            );
            if rpar.is_none() { return None; };
            
            let mut names_used = HashSet::new();
            let mut any_optional_params = false;
            let mut any_optional_named_params = false;
            
            for (param, spread, is_underscore) in raw_params {
                let is_required = spread.is_none() && !param.question_mark && param.default_value.is_none();
                let is_positional = match &spread {
                    Some(spread) => spread.as_str() == "*",
                    None => is_underscore,
                };
                let is_spread = spread.is_some();
                
                if !names_used.insert(param.name_id) {
                    self.diagnostics.syntax_error(&format!("duplicate parameter name '{}'", self.string_pool.get(param.name_id)), &param.range);
                } else if is_positional && (!named_params.is_empty() || spread_named_param.is_some()) {
                    self.diagnostics.syntax_error("positional parameter cannot occur after named parameter", &param.range);
                } else if is_required && (if is_positional { any_optional_params } else { any_optional_named_params }) {
                    self.diagnostics.syntax_error("required parameter canot occur after optional parameter", &param.range);
                } else if is_positional && param.is_implicit {
                    self.diagnostics.syntax_error("positional parameter cannot be implicit", &param.range);
                } else if is_spread && param.is_implicit {
                    self.diagnostics.syntax_error("spread parameter cannot be implicit", &param.range);
                } else if param.is_implicit && param.default_value.is_some() {
                    self.diagnostics.syntax_error("implicit parameter cannot have default", &param.range);
                } else if (if is_positional { &spread_param } else { &spread_named_param }).is_some() {
                    let msg = if is_spread { "cannot have multiple spread parameters" } else { "parameter cannot occur after spread parameter" };
                    self.diagnostics.syntax_error(msg, &param.range);
                }
                
                match (is_positional, is_spread) {
                    (false, false) => {
                        named_params.push(param);
                        any_optional_named_params |= !is_required;
                    },
                    (true, false) => {
                        positional_params.push(param);
                        any_optional_params |= !is_required;
                    },
                    (false, true) => {
                        if is_underscore {
                            self.diagnostics.syntax_error("dict spread parameter name cannot begin with underscore", &param.range);
                        }
                        spread_named_param = Some(param);
                    },
                    (true, true) => {
                        if !is_underscore {
                            self.diagnostics.syntax_error("positional spread parameter name must begin with underscore", &param.range);
                        }
                        spread_param = Some(param);
                    },
                }
            }
        };
        
        let (content_param, spread, _) = self.parse_param()?;
        if let Some(spread) = spread {
            self.diagnostics.syntax_error("content parameter cannot be spread", &spread.range);
        } else if let Some(value) = content_param.default_value.as_ref() {
            self.diagnostics.syntax_error("content parameter cannot have default value", value.range());
        }
        
        let mut range = content_param.range.clone();
        if let Some(lpar) = lpar { range = lpar.range.to_end(range.end); }
        
        Some(Signature {
            range,
            positional_params: positional_params.into_boxed_slice(),
            spread_param: spread_param.map(Box::new),
            named_params: named_params.into_boxed_slice(),
            spread_named_param: spread_named_param.map(Box::new),
            content_param,
        })
    }
    
    fn parse_args(&mut self) -> Option<Vec<Arg>> {
        self.skip_whitespace();
        let Some(lpar) = self.poll_if_kind(TokenKind::LPar) else {
            return Some(Vec::new());
        };
        
        let (args, rpar) = self.parse_comma_separated_until(
            &lpar,
            |_self| _self.parse_arg(),
            TokenKind::RPar,
        );
        if rpar.is_none() { return None; }
        
        let mut any_named = false;
        let mut names_used = HashSet::new();
        
        for arg in &args {
            if !arg.name_id.is_anonymous() {
                if !names_used.insert(arg.name_id) {
                    self.diagnostics.syntax_error("duplicate named argument", &arg.range);
                }
                any_named = true;
            } else if arg.spread_kind == SpreadKind::Named {
                any_named = true;
            } else if any_named {
                self.diagnostics.syntax_error("positional argument cannot occur after named argument", &arg.range);
            }
        };
        
        Some(args)
    }
    
    fn parse_param(&mut self) -> Option<(Param, Option<Token>, bool)> {
        self.skip_whitespace();
        let spread = self.poll_if_kind(TokenKind::Asterisk);
        self.skip_whitespace();
        let name = self.expect_poll_kind(TokenKind::VarName)?;
        
        let name_id = self.string_pool.insert(name.get_var_name());
        let underscore = name.as_str().starts_with("$_");
        
        self.skip_whitespace();
        let question_mark = self.poll_if_kind(TokenKind::QuestionMark);
        self.skip_whitespace();
        let (is_implicit, type_annotation) = if self.poll_if_kind(TokenKind::Colon).is_some() {
            self.skip_whitespace();
            (self.poll_if(|t| t.as_str() == "implicit").is_some(), self.parse_type())
        } else {
            (false, None)
        };
        
        self.skip_whitespace();
        let default_value = if self.poll_if_kind(TokenKind::Equals).is_some() {
            Some(self.parse_value()?)
        } else {
            None
        };
        if default_value.is_some() && spread.is_some() {
            self.diagnostics.syntax_error("default value not allowed for spread parameter", default_value.as_ref().unwrap().range());
        }
        
        let end = default_value.as_ref().map(|v| v.range().end)
            .or_else(|| type_annotation.as_ref().map(|t| t.range().end))
            .or_else(|| question_mark.as_ref().map(|t| t.range.end))
            .unwrap_or_else(|| name.range.end);
        let range = spread.as_ref()
            .map(|t| t.range.to_end(end))
            .unwrap_or_else(|| name.range.to_end(end));
        
        Some((
            Param {
                range,
                name_id,
                question_mark: question_mark.is_some(),
                is_implicit,
                type_annotation,
                default_value: default_value.map(Box::new),
            },
            spread,
            underscore,
        ))
    }
    
    fn parse_arg(&mut self) -> Option<Arg> {
        self.skip_whitespace();
        let spread = self.poll_if_kind(TokenKind::Asterisk);
        let spread_kind = match &spread {
            None => SpreadKind::NoSpread,
            Some(t) if t.range.len() == 1 => SpreadKind::Positional,
            Some(t) if t.range.len() == 2 => SpreadKind::Named,
            Some(t) => ice_at("invalid spread token", &t.range),
        };
        self.skip_whitespace();
        let Some(name) = self.poll_if_kind(TokenKind::Name) else {
            let value = self.parse_value()?;
            let v_range = value.range();
            let range = spread.as_ref().map(|t| t.range.to_end(v_range.end)).unwrap_or_else(|| v_range.clone());
            return Some(Arg {range, spread_kind, name_id: str_ids::ANONYMOUS, value});
        };
        let name_id = self.string_pool.insert(name.as_str());
        let range_start = spread.as_ref().map(|t| t.range.clone()).unwrap_or(name.range.clone());
        
        self.skip_whitespace();
        Some(if self.poll_if_kind(TokenKind::Equals).is_some() {
            if let Some(spread) = &spread {
                self.diagnostics.syntax_error("spread not allowed for named argument (should be parameter?)", &spread.range);
            }
            let value = self.parse_value()?;
            let range = range_start.to_end(value.range().end);
            Arg {range, spread_kind: SpreadKind::NoSpread, name_id, value}
        } else {
            let range = range_start.to_end(name.range.end);
            Arg {range, spread_kind, name_id: str_ids::ANONYMOUS, value: AST::LiteralValue(name)}
        })
    }
    
    fn parse_type(&mut self) -> Option<TypeAnnotation> {
        self.skip_whitespace();
        let primitive_token = self.expect_poll_kind(TokenKind::Name)?;
        if !primitive_token.is_primitive_type() {
            self.diagnostics.syntax_error(&format!("'{}' is not a type", primitive_token.as_str()), &primitive_token.range);
            return None;
        };
        
        let mut t = TypeAnnotation::Primitive(primitive_token.range);
        while let Some(group_kind) = {
            self.skip_whitespace();
            self.poll_if(Token::is_group_type)
        } {
            t = TypeAnnotation::Group(Box::new(t), group_kind.range);
        }
        Some(t)
    }
    
    fn parse_ellipsis_group(&mut self, open: Token) -> AST {
        let mut children = Vec::new();
        while let Some(tok) = self.poll_if(|tok| !matches!(tok.kind, TokenKind::CloseTag | TokenKind::RSqb | TokenKind::RBrace)) {
            if let Some(child) = self.parse_node(tok) {
                children.push(child);
            }
        }
        
        AST::Group(AST::seq(open, children, None))
    }
    
    fn parse_match_branches(&mut self, open: Token) -> Option<(Box<[MatchBranch]>, Token)> {
        let (children, close) = self.parse_comma_separated_until(
            &open,
            |_self| _self.parse_match_branch(),
            TokenKind::RBrace,
        );
        close.map(|close| (children.into_boxed_slice(), close))
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
        let pattern = match tok.kind {
            TokenKind::Name if tok.as_str() == "_" => {
                MatchPattern::Ignore(tok.range)
            },
            TokenKind::Name => {
                self.diagnostics.syntax_error("pattern cannot be bare name; use $ for variable name or quotes for string literal", &tok.range);
                return None;
            },
            TokenKind::Dot | TokenKind::Boolean | TokenKind::Number | TokenKind::Verbatim => {
                MatchPattern::Literal(tok)
            },
            TokenKind::VarName => {
                MatchPattern::VarName(self.parse_var_name(tok))
            },
            TokenKind::LSqb => {
                let (children, rsqb) = self.parse_comma_separated_until(
                    &tok,
                    |_self| _self.parse_match_pattern(SpreadKind::Positional),
                    TokenKind::RSqb,
                );
                let Some(rsqb) = rsqb else { return None; };
                
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
                
                let range = tok.range.to_end(rsqb.range.end);
                match spread_index {
                    Some(spread_index) => MatchPattern::SpreadList(range, child_patterns, spread_index),
                    None => MatchPattern::ExactList(range, child_patterns),
                }
            },
            TokenKind::Asterisk => {
                self.skip_whitespace();
                let name = self.poll_if(|t| t.as_str() == "_")
                    .or_else(|| self.expect_poll_kind(TokenKind::VarName))?;
                let range = tok.range.to_end(name.range.end);
                match (tok.range.len(), allow_spread) {
                    (1, SpreadKind::Positional) | (2, SpreadKind::Named) => if name.as_str() == "_" {
                        MatchPattern::SpreadIgnore(range)
                    } else {
                        MatchPattern::SpreadVarName(self.parse_var_name(name))
                    },
                    _ => {
                        let msg = if tok.range.len() == 1 { "positional spread not allowed here" } else { "named spread not allowed here" };
                        self.diagnostics.syntax_error(msg, &tok.range);
                        return None;
                    },
                }
            },
            _ => {
                self.diagnostics.err_unexpected_token(&tok);
                return None;
            },
        };
        
        self.skip_whitespace();
        if self.poll_if_kind(TokenKind::Colon).is_some() {
            let type_ = self.parse_type()?;
            let range = pattern.range().to_end(type_.range().end);
            if matches!(pattern, MatchPattern::Literal(..)) {
                self.diagnostics.syntax_error("literal pattern cannot have type annotation", &range);
                None
            } else {
                Some(MatchPattern::Typed(range, Box::new(pattern), type_))
            }
        } else {
            Some(pattern)
        }
    }
    
    fn parse_group(&mut self, open: Token) -> AST {
        let (children, close) = self.parse_nodes_until(|tok| tok.kind == TokenKind::RBrace);
        if close.is_none() {
            self.diagnostics.err_unmatched(&open);
        }
        AST::Group(AST::seq(open, children, close))
    }
    
    fn parse_list(&mut self, open: Token) -> AST {
        let (children, close) = self.parse_comma_separated_until(
            &open,
            |_self| _self.parse_value(),
            TokenKind::RSqb,
        );
        AST::List(AST::seq(open, children, close))
    }
    
    fn parse_template(&mut self, open: Token) -> AST {
        let (children, close) = self.parse_nodes_until(if open.kind == TokenKind::LQuote {
            |tok: &Token| matches!(tok.kind, TokenKind::LQuote | TokenKind::RQuote)
        } else {
            |tok: &Token| matches!(tok.kind, TokenKind::LDblQuote | TokenKind::RDblQuote)
        });
        if close.is_none() {
            self.diagnostics.err_unmatched(&open);
        }
        AST::Template(AST::seq(open, children, close))
    }
    
    fn parse_var_name(&mut self, token: Token) -> VarName {
        let name_id = self.string_pool.insert(token.get_var_name());
        VarName {name_id, range: token.range}
    }
    
    fn parse_text(&mut self, first_token: Token) -> AST {
        let Some(mut text) = first_token.text() else {
            ice_at("invalid text token", &first_token.range)
        };
        let mut end = first_token.range.end;
        
        while let Some((tok, t)) = self.poll_if_map(|tok| match tok.kind {
            TokenKind::LDblQuote | TokenKind::RDblQuote => None,
            _ => tok.text(),
        }) {
            text += &t;
            end = tok.range.end;
        }
        
        text = text_substitutions(&text);
        AST::Text(Rc::from(text), first_token.range.to_end(end))
    }
}
