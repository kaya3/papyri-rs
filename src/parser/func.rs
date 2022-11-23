use std::rc::Rc;
use indexmap::IndexSet;

use crate::utils::{str_ids, ice_at};
use super::ast::*;
use super::queue::Parser;
use super::token::{Token, TokenKind};

impl <'a> Parser<'a> {
    pub fn parse_func_def(&mut self, at: Token) -> Option<FuncDef> {
        self.skip_whitespace();
        let name_id = self.poll_if_kind(TokenKind::Name)
            .map_or(
                str_ids::ANONYMOUS,
                |t| self.string_pool.insert(t.as_str()),
            );
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
    
    pub fn parse_func_call(&mut self, at: Token) -> Option<FuncCall> {
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
            
            let mut names_used = IndexSet::new();
            let mut any_optional_params = false;
            let mut any_optional_named_params = false;
            
            for (param, spread, is_underscore) in raw_params {
                let is_required = spread.is_none() && !param.question_mark && param.default_value.is_none();
                let is_positional = match &spread {
                    Some(spread) => spread.range.len() == 1,
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
    
    pub fn parse_args(&mut self) -> Option<Vec<Arg>> {
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
        let mut names_used = IndexSet::new();
        
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
        let var_name = name.get_var_name();
        let name_id = self.string_pool.insert(var_name);
        let underscore = var_name.starts_with("_");
        
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
        let range = match spread.as_ref() {
            Some(t) => t.range.to_end(end),
            None => name.range.to_end(end),
        };
        
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
    
    pub fn parse_arg(&mut self) -> Option<Arg> {
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
            let range = match spread.as_ref() {
                Some(t) => t.range.to_end(v_range.end),
                None => v_range.clone(),
            };
            return Some(Arg {range, spread_kind, name_id: str_ids::ANONYMOUS, value});
        };
        
        let range_start = match spread.as_ref() {
            Some(t) => t.range.clone(),
            None => name.range.clone(),
        };
        
        self.skip_whitespace();
        Some(if self.poll_if_kind(TokenKind::Equals).is_some() {
            if let Some(spread) = &spread {
                self.diagnostics.syntax_error("spread not allowed for named argument", &spread.range);
            }
            let name_id = self.string_pool.insert(name.as_str());
            let value = self.parse_value()?;
            let range = range_start.to_end(value.range().end);
            Arg {range, spread_kind: SpreadKind::NoSpread, name_id, value}
        } else {
            let range = range_start.to_end(name.range.end);
            Arg {range, spread_kind, name_id: str_ids::ANONYMOUS, value: AST::LiteralValue(name)}
        })
    }
}
