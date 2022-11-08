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
            let tok = match self.poll() {
                Some(tok) => tok,
                None => break None,
            };
            
            if closer(&tok) {
                break Some(tok);
            } else if let Some(child) = self.parse_node(tok) {
                nodes.push(child);
            }
        };
        
        while !nodes.is_empty() && nodes[nodes.len() - 1].is_whitespace() {
            nodes.pop();
        }
        (nodes, end)
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
            TokenKind::VarName => self.parse_var_name(tok),
            TokenKind::FuncName => self.parse_func(tok),
            TokenKind::OpenTag => self.parse_tag(tok),
            TokenKind::Ellipsis => Some(self.parse_ellipsis_group(tok)),
            TokenKind::LBrace => Some(self.parse_group(tok)),
            TokenKind::LSqb => Some(self.parse_list(tok)),
            
            _ => {
                self.diagnostics.syntax_error("expected value", &tok.range);
                None
            }
        }
    }
    
    fn parse_node(&mut self, tok: Token) -> Option<AST> {
        match tok.kind {
            TokenKind::Verbatim => Some(AST::Verbatim(tok)),
            TokenKind::VarName => self.parse_var_name(tok),
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
    
    fn parse_var_name(&mut self, tok: Token) -> Option<AST> {
        let name_id = self.string_pool.insert(tok.get_var_name());
        Some(AST::VarName(name_id, tok.range))
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
            tag.range = tag.range.to_end(close.range.end);
            tag.children = children.into_boxed_slice();
        }
        Some(AST::Tag(Box::new(tag)))
    }
    
    fn parse_tag_attribute(&mut self) -> Option<(Token, TagAttribute)> {
        let name = self.poll_ignoring_whitespace_if_kind(TokenKind::Name)?;
        let name_id = self.string_pool.insert(name.as_str());
        let question_mark = self.poll_ignoring_whitespace_if_kind(TokenKind::QuestionMark).is_some();
        if self.poll_ignoring_whitespace_if_kind(TokenKind::Equals).is_none() {
            return Some((name, TagAttribute {name_id, question_mark, value: None}));
        }
        match self.poll_ignoring_whitespace_if_kind(TokenKind::LDblQuote) {
            Some(open) => {
                let (children, close) = self.parse_nodes_until(|tok| matches!(tok.kind, TokenKind::LDblQuote | TokenKind::RDblQuote));
                if close.is_none() {
                    self.diagnostics.err_unmatched(&open);
                }
                Some((name, TagAttribute {
                    name_id,
                    question_mark,
                    value: Some(AST::Group(AST::seq(open, children, close)))
                }))
            },
            None => {
                self.skip_whitespace();
                let tok = self.expect_poll()?;
                self.parse_node(tok)
                    .map(|value| (name, TagAttribute {name_id, question_mark, value: Some(value)}))
            },
        }
    }
    
    fn parse_func(&mut self, at: Token) -> Option<AST> {
        if at.as_str() == "@fn" {
            self.skip_whitespace();
            let name_id = self.poll_if_kind(TokenKind::Name)
                .map(|t| self.string_pool.insert(t.as_str()))
                .unwrap_or(str_ids::ANONYMOUS);
            let signature = self.parse_signature()?;
            self.skip_whitespace();
            self.expect_poll_kind(TokenKind::Arrow)?;
            let body = Rc::new(self.parse_value()?);
            Some(AST::FuncDef(Box::new(FuncDef {
                range: at.range.to_end(body.range().end),
                name_id,
                signature,
                body,
            })))
        } else {
            let name_id = self.string_pool.insert(at.get_func_name());
            let args = self.parse_args()?.into_boxed_slice();
            let content = self.parse_value()?;
            Some(AST::FuncCall(Box::new(FuncCall {
                range: at.range.to_end(content.range().end),
                name_id,
                args,
                content,
            })))
        }
    }
    
    fn parse_signature(&mut self) -> Option<Signature> {
        let mut positional_params: Vec<Param> = Vec::new();
        let mut spread_param: Option<Param> = None;
        let mut named_params: Vec<Param> = Vec::new();
        let mut spread_named_param: Option<Param> = None;
        
        let mut names_used = HashSet::new();
        let mut any_optional_params = false;
        let mut any_optional_named_params = false;
        let mut expect_end = false;
        
        self.skip_whitespace();
        let lpar = self.poll_if_kind(TokenKind::LPar);
        if let Some(lpar) = &lpar {
            loop {
                self.skip_whitespace();
                let rpar = if expect_end { self.expect_poll_kind(TokenKind::RPar) } else { self.poll_if_kind(TokenKind::RPar) };
                if rpar.is_some() { break; }
                
                let Some((param, spread, is_underscore)) = self.parse_param() else {
                    self.diagnostics.err_unmatched(lpar);
                    return None;
                };
                
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
                
                self.skip_whitespace();
                expect_end = self.poll_if_kind(TokenKind::Comma).is_none();
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
        
        let mut args: Vec<Arg> = Vec::new();
        let mut any_named = false;
        let mut names_used = HashSet::new();
        let mut expect_end = false;
        
        loop {
            self.skip_whitespace();
            if self.poll_if_kind(TokenKind::RPar).is_some() { break; }
            
            let Some(arg) = self.parse_arg() else {
                self.diagnostics.err_unmatched(&lpar);
                return None;
            };
            if expect_end {
                self.diagnostics.syntax_error("expected comma", &arg.range);
            } else if !arg.name_id.is_anonymous() {
                if !names_used.insert(arg.name_id) {
                    self.diagnostics.syntax_error("duplicate named argument", &arg.range);
                }
                any_named = true;
            } else if arg.spread_kind == SpreadKind::Named {
                any_named = true;
            } else if any_named {
                self.diagnostics.syntax_error("positional argument cannot occur after named argument", &arg.range);
            }
            args.push(arg);
            
            self.skip_whitespace();
            expect_end = self.poll_if_kind(TokenKind::Comma).is_none();
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
    
    fn parse_group(&mut self, open: Token) -> AST {
        let (children, close) = self.parse_nodes_until(|tok| tok.kind == TokenKind::RBrace);
        AST::Group(AST::seq(open, children, close))
    }
    
    fn parse_list(&mut self, open: Token) -> AST {
        let mut children = Vec::new();
        let mut expect_end = false;
        let close = loop {
            self.skip_whitespace();
            let close = if expect_end { self.expect_poll_kind(TokenKind::RSqb) } else { self.poll_if_kind(TokenKind::RSqb) };
            if close.is_some() {
                break close;
            }
            if let Some(child) = self.parse_value() {
                children.push(child);
            } else {
                // error
                break None;
            }
            self.skip_whitespace();
            expect_end = self.poll_if_kind(TokenKind::Comma).is_none();
        };
        
        AST::List(AST::seq(open, children, close))
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
