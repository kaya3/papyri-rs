use std::rc::Rc;

use crate::errors::{SyntaxError, Reported};
use crate::utils::sourcefile::SourceRange;
use crate::utils::{str_ids, NameIDSet};
use super::ast::*;
use super::base::Parser;
use super::token::{Token, TokenKind, Keyword};
use super::types::Type;

impl <'a> Parser<'a> {
    pub(super) fn parse_func_def(&mut self, at: Token, allow_anonymous: bool) -> Reported<FuncDef> {
        self.skip_whitespace();
        let name_id = self.poll_if_kind(TokenKind::Name)
            .map_or(
                str_ids::ANONYMOUS,
                |t| self.tok_name_id(t),
            );
        let signature = self.parse_signature()?;
        self.skip_whitespace();
        self.expect_poll_kind(TokenKind::Arrow)?;
        let body = Rc::new(self.parse_expr()?);
        
        if name_id.is_anonymous() && !allow_anonymous {
            return Err(self.report(SyntaxError::AnonymousFunctionNotAllowed, at.range));
        }
        Ok(FuncDef {
            range: at.range.to_end(body.range().end),
            name_id,
            signature,
            body,
        })
    }
    
    pub(super) fn parse_func_call(&mut self, at: Token) -> Reported<FuncCall> {
        let func = self.parse_name(at)?;
        let args = self.parse_args()?.into_boxed_slice();
        let content = self.parse_expr_or_ellipsis()?;
        Ok(FuncCall {
            range: func.range().to_end(content.range().end),
            func,
            args,
            content,
        })
    }
    
    pub(super) fn poll_if_spread(&mut self, allow_pos: bool, allow_named: bool) -> (SpreadKind, Option<SourceRange>) {
        let Some(tok) = self.poll_if(|_self, t| matches!(t.kind, TokenKind::Asterisk | TokenKind::DoubleAsterisk)) else {
            return (SpreadKind::NoSpread, None);
        };
        let spread_kind = match tok.kind {
            TokenKind::Asterisk if allow_pos => SpreadKind::Positional,
            TokenKind::DoubleAsterisk if allow_named => SpreadKind::Named,
            _ => {
                let e = if tok.kind == TokenKind::Asterisk { SyntaxError::SpreadPositionalNotAllowed } else { SyntaxError::SpreadNamedNotAllowed };
                self.report(e, tok.range);
                SpreadKind::NoSpread
            },
        };
        (spread_kind, Some(tok.range))
    }
    
    fn parse_signature(&mut self) -> Reported<Signature> {
        let mut params = Vec::new();
        let mut positional_count = 0;
        let mut positional_spread = false;
        let mut named_count = 0;
        let mut named_spread = false;
        
        self.skip_whitespace();
        let lpar = self.poll_if_kind(TokenKind::LPar);
        if let Some(lpar) = lpar {
            let (raw_params, _) = self.parse_separated_until(
                lpar,
                Parser::parse_param,
                TokenKind::Comma,
                TokenKind::RPar,
            )?;
            
            let mut any_named_params = false;
            let mut names_used = NameIDSet::default();
            let mut any_optional_params = false;
            let mut any_optional_named_params = false;
            
            for (param, spread_kind, is_underscore) in raw_params {
                let is_required = spread_kind == SpreadKind::NoSpread && !param.question_mark && param.default_value.is_none();
                let is_spread = spread_kind != SpreadKind::NoSpread;
                let is_positional = if is_spread { spread_kind == SpreadKind::Positional } else { is_underscore };
                
                if !names_used.insert(param.name_id) {
                    let name = self.string_pool.get(param.name_id);
                    self.report(SyntaxError::ParamDuplicateName(name), param.range);
                } else if is_positional && any_named_params {
                    self.report(SyntaxError::ParamPositionalAfterNamed, param.range);
                } else if is_required && (if is_positional { any_optional_params } else { any_optional_named_params }) {
                    self.report(SyntaxError::ParamRequiredAfterOptional, param.range);
                } else if is_positional && param.is_implicit {
                    self.report(SyntaxError::ParamPositionalImplicit, param.range);
                } else if is_spread && param.is_implicit {
                    self.report(SyntaxError::ParamSpreadImplicit, param.range);
                } else if param.is_implicit && param.default_value.is_some() {
                    self.report(SyntaxError::ParamDefaultImplicit, param.range);
                } else if (is_positional && positional_spread) || (!is_positional && named_spread) {
                    let msg = if is_spread { SyntaxError::ParamMultipleSpread } else { SyntaxError::ParamAfterSpread };
                    self.report(msg, param.range);
                }
                
                match (is_positional, is_spread) {
                    (false, false) => {
                        named_count += 1;
                        any_named_params = true;
                        any_optional_named_params |= !is_required;
                    },
                    (true, false) => {
                        positional_count += 1;
                        any_optional_params |= !is_required;
                    },
                    (false, true) => {
                        if is_underscore {
                            self.report(SyntaxError::ParamNamedSpreadUnderscore, param.range);
                        }
                        any_named_params = true;
                        named_spread = true;
                    },
                    (true, true) => {
                        if !is_underscore {
                            self.report(SyntaxError::ParamPositionalSpreadNoUnderscore, param.range);
                        }
                        positional_spread = true;
                    },
                }
                params.push(param);
            }
        };
        
        self.skip_whitespace();
        let (has_content, mut range) = if let Some(dot) = self.poll_if_kind(TokenKind::Dot) {
            (false, dot.range)
        } else {
            let (c, spread_kind, _) = self.parse_param()?;
            if spread_kind != SpreadKind::NoSpread {
                self.report(SyntaxError::ParamContentSpread, c.range);
            } else if let Some(value) = c.default_value.as_ref() {
                self.report(SyntaxError::ParamContentDefault, value.range());
            }
            let range = c.range;
            params.push(c);
            (true, range)
        };
        
        if let Some(lpar) = lpar { range.start = lpar.range.start; }
        
        Ok(Signature {
            range,
            params: params.into_boxed_slice(),
            positional_count,
            positional_spread,
            named_count,
            named_spread,
            has_content,
        })
    }
    
    pub(super) fn parse_args(&mut self) -> Reported<Vec<Arg>> {
        self.skip_whitespace();
        let Some(lpar) = self.poll_if_kind(TokenKind::LPar) else {
            return Ok(Vec::new());
        };
        
        let (args, _) = self.parse_separated_until(
            lpar,
            Parser::parse_arg,
            TokenKind::Comma,
            TokenKind::RPar,
        )?;
        
        let mut any_named = false;
        let mut names_used = NameIDSet::default();
        
        for arg in args.iter() {
            if !arg.name_id.is_anonymous() {
                if !names_used.insert(arg.name_id) {
                    let name = self.string_pool.get(arg.name_id);
                    self.report(SyntaxError::ArgDuplicateName(name), arg.range);
                }
                any_named = true;
            } else if arg.spread_kind == SpreadKind::Named {
                any_named = true;
            } else if any_named {
                self.report(SyntaxError::ArgPositionalAfterNamed, arg.range);
            }
        };
        
        Ok(args)
    }
    
    fn parse_param(&mut self) -> Reported<(Param, SpreadKind, bool)> {
        self.skip_whitespace();
        let (spread_kind, spread_range) = self.poll_if_spread(true, true);
        self.skip_whitespace();
        let name_tok = self.expect_poll_kind(TokenKind::VarName)?;
        let name_id = self.tok_name_id(name_tok);
        let underscore = self.tok_str(name_tok).starts_with("$_");
        
        self.skip_whitespace();
        let question_mark = self.poll_if_kind(TokenKind::QuestionMark);
        self.skip_whitespace();
        let (is_implicit, type_annotation, type_annotation_range) = if self.poll_if_kind(TokenKind::Colon).is_some() {
            self.skip_whitespace();
            let is_implicit = self.poll_if_kind(TokenKind::Keyword(Keyword::Implicit)).is_some();
            let (type_, type_range) = self.parse_type()
                .map_or_else(
                    |_| (Type::Any, None),
                    |(t, r)| (t, Some(r)),
                );
            (is_implicit, type_, type_range)
        } else {
            (false, Type::Any, None)
        };
        
        self.skip_whitespace();
        let default_value = if self.poll_if_kind(TokenKind::Equals).is_some() {
            Some(self.parse_expr()?)
        } else {
            None
        };
        if default_value.is_some() && spread_kind != SpreadKind::NoSpread {
            self.report(SyntaxError::ParamSpreadDefault, default_value.as_ref().unwrap().range());
        }
        
        let end = default_value.as_ref()
            .map(|v| v.range().end)
            .or_else(|| type_annotation_range.map(|t| t.end))
            .or_else(|| question_mark.as_ref().map(|t| t.range.end))
            .unwrap_or(name_tok.range.end);
        let range = spread_range.unwrap_or(name_tok.range)
            .to_end(end);
        
        let p = Param {
            range,
            name_id,
            question_mark: question_mark.is_some(),
            is_implicit,
            type_annotation,
            default_value: default_value.map(Box::new),
        };
        Ok((p, spread_kind, underscore))
    }
    
    pub(super) fn parse_arg(&mut self) -> Reported<Arg> {
        self.skip_whitespace();
        let (spread_kind, spread_range) = self.poll_if_spread(true, true);
        
        self.skip_whitespace();
        let Some(name_tok) = self.poll_if_kind(TokenKind::Name) else {
            let value = self.parse_expr()?;
            let v_range = value.range();
            let range = spread_range.map_or(v_range, |r| r.to_end(v_range.end));
            return Ok(Arg {
                range,
                spread_kind,
                name_id: str_ids::ANONYMOUS,
                value,
            });
        };
        
        let range_start = spread_range.unwrap_or(name_tok.range);
        
        self.skip_whitespace();
        Ok(if self.poll_if_kind(TokenKind::Equals).is_some() {
            if let Some(spread_range) = spread_range {
                self.report(SyntaxError::ArgSpreadNamed, spread_range);
            }
            let value = self.parse_expr()?;
            Arg {
                range: range_start.to_end(value.range().end),
                spread_kind: SpreadKind::NoSpread,
                name_id: self.tok_name_id(name_tok),
                value,
            }
        } else {
            Arg {
                range: range_start.to_end(name_tok.range.end),
                spread_kind,
                name_id: str_ids::ANONYMOUS,
                value: Expr::BareString(name_tok.range),
            }
        })
    }
}
