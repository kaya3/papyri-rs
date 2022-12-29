use crate::errors::{ice_at, Warning};
use crate::parser::ast;
use super::base::Compiler;
use super::html::HTML;
use super::types::Type;
use super::value::{Value, ValueMap};

impl <'a> Compiler<'a> {
    pub(super) fn evaluate_match(&mut self, match_: &ast::Match, type_hint: &Type) -> Option<Value> {
        let value = self.evaluate_node(&match_.value, &Type::Any)?;
        for (pattern, handler) in match_.branches.iter() {
            let frame = self.frame()
                .to_inactive()
                .new_empty_child_frame();
            let r = self.evaluate_in_frame(frame, |_self| {
                if _self.bind_pattern(pattern, value.clone()) {
                    Ok(_self.evaluate_node(handler, type_hint))
                } else {
                    Err(())
                }
            });
            if let Ok(r) = r { return r; }
        }
        
        self.warning(Warning::NoMatchingBranch, match_.range);
        Some(Value::UNIT)
    }
    
    fn bind_pattern(&mut self, pattern: &ast::MatchPattern, value: Value) -> bool {
        match pattern {
            ast::MatchPattern::Ignore(..) => true,
            ast::MatchPattern::LiteralNone(..) => value.is_unit(),
            ast::MatchPattern::Literal(other) => {
                self.evaluate_literal(other)
                    .map_or(false, |other| value == other)
            },
            &ast::MatchPattern::LiteralName(_, name_id) => {
                matches!(value, Value::Str(s) if s.as_ref() == self.get_name(name_id))
            },
            ast::MatchPattern::VarName(var) => {
                self.bind_one(var, value);
                true
            },
            ast::MatchPattern::And(pair) => {
                let (left, right) = pair.as_ref();
                self.bind_pattern(left, value.clone())
                    && self.bind_pattern(right, value)
            },
            ast::MatchPattern::Or(pair, name_ids) => {
                let (left, right) = pair.as_ref();
                let r = self.bind_pattern(left, value.clone())
                    || {self.frame().unset_all(name_ids); self.bind_pattern(right, value)};
                if r {
                    self.frame().set_all_if_not_present(name_ids, Value::UNIT);
                }
                r
            },
            ast::MatchPattern::EqualsValue(child) => {
                self.evaluate_node(child, &Type::Any)
                    .map_or(false, |other| value == other)
            },
            ast::MatchPattern::Typed(type_) => {
                self.compile_type(type_).check_value(value.clone())
            },
            
            ast::MatchPattern::TypeOf(t_var) => {
                let t = value.get_type()
                    .to_string()
                    .into();
                self.bind_one(t_var, t);
                true
            },
            
            ast::MatchPattern::ExactList(_, child_patterns) => {
                let Value::List(child_values) = value else { return false; };
                child_patterns.len() == child_values.len()
                    && self.bind_all(child_patterns, child_values.as_ref().iter().cloned())
            },
            
            ast::MatchPattern::ExactHTMLSeq(_, child_patterns) => {
                let Some(html) = value.try_into_html() else {
                    return false;
                };
                match html {
                    HTML::Empty => child_patterns.is_empty(),
                    HTML::Sequence(seq) => {
                        if child_patterns.len() != seq.len() { return false; }
                        let child_values = seq.iter()
                            .map(Value::from);
                        self.bind_all(child_patterns, child_values)
                    },
                    _ => child_patterns.len() == 1 && self.bind_pattern(&child_patterns[0], html.into()),
                }
            },
            
            ast::MatchPattern::SpreadList(_, child_patterns, spread_index) => {
                let Value::List(child_values) = value else { return false; };
                self.bind_all_with_spread(
                    child_patterns,
                    *spread_index,
                    child_values.len(),
                    |i| child_values.get(i).clone(),
                    |a, b| child_values.slice(a, b).into(),
                )
            },
            
            ast::MatchPattern::SpreadHTMLSeq(_, child_patterns, spread_index) => {
                let Some(html) = value.try_into_html() else {
                    return false;
                };
                
                let html_slice = match &html {
                    HTML::Empty => {
                        if child_patterns.len() != 1 { return false; }
                        &[]
                    },
                    HTML::Sequence(seq) => {
                        if seq.len() + 1 < child_patterns.len() { return false; }
                        seq.as_ref()
                    },
                    _ => std::slice::from_ref(&html),
                };
                
                self.bind_all_with_spread(
                    child_patterns,
                    *spread_index,
                    html_slice.len(),
                    |i| html_slice[i].clone().into(),
                    |a, b| HTML::seq(html_slice[a..b].iter().cloned()).into(),
                )
            },
            
            ast::MatchPattern::Dict(_, dict_pattern) => {
                let Value::Dict(dict_value) = value else { return false; };
                if dict_pattern.spread.is_none() && dict_value.len() != dict_pattern.attrs.len() { return false; }
                
                for (name_id, child_pattern) in dict_pattern.attrs.iter() {
                    let Some(child_value) = dict_value.get(name_id) else { return false; };
                    if !self.bind_pattern(child_pattern, child_value.clone()) { return false; }
                }
                
                if let Some(spread_pattern) = dict_pattern.spread.as_ref() {
                    if !matches!(spread_pattern, ast::MatchPattern::Ignore(..)) {
                        let remaining: ValueMap = dict_value.iter()
                            .filter(|(&k, _)| !dict_pattern.attrs.contains_key(&k))
                            .map(|(&k, v)| (k, v.clone()))
                            .collect();
                        if !self.bind_pattern(spread_pattern, remaining.into()) { return false; }
                    }
                }
                true
            },
            
            ast::MatchPattern::Tag(_, tag_pattern) => {
                let Value::HTML(HTML::Tag(tag_value)) = value else { return false; };
                
                let tag_name = self.get_name(tag_value.name_id);
                if !self.bind_pattern(&tag_pattern.name, tag_name.into()) { return false; }
                
                let vs: ValueMap = tag_value.attributes
                    .iter()
                    .map(|(&k, v)| (
                        k,
                        v.clone().into(),
                    ))
                    .collect();
                
                self.bind_pattern(&tag_pattern.attrs, vs.into())
                    && self.bind_pattern(&tag_pattern.content, tag_value.content.clone().into())
            },
            
            ast::MatchPattern::Regex(pattern_range, regex_pattern) => {
                let Value::Str(value_str) = value else { return false; };
                let Some(match_) = regex_pattern.regex.captures(value_str.as_ref()) else { return false; };
                
                if match_.len() != regex_pattern.names.len() + 1 { ice_at("incorrect number of capture groups", *pattern_range); }
                let match_ = match_.iter()
                    .skip(1)
                    .zip(regex_pattern.names.iter());
                
                for (s, var) in match_ {
                    let v = s.map_or(Value::UNIT, |m| m.as_str().into());
                    self.bind_one(var, v);
                }
                true
            },
        }
    }
    
    fn bind_all_with_spread(
        &mut self,
        patterns: &[ast::MatchPattern],
        spread_index: u32,
        values_len: usize,
        f_one: impl Fn(usize) -> Value,
        f_slice: impl Fn(usize, usize) -> Value,
    ) -> bool {
        if values_len + 1 < patterns.len() { return false; }
        
        let i = spread_index as usize;
        let pre_patterns = &patterns[..i];
        let spread_pattern = &patterns[i];
        let post_patterns = &patterns[i + 1..];
        
        let j = values_len - post_patterns.len();
        
        self.bind_all(pre_patterns, (0..i).map(&f_one))
            && self.bind_pattern(spread_pattern, f_slice(i, j))
            && self.bind_all(post_patterns, (j..values_len).map(f_one))
    }
    
    fn bind_all<T: Iterator<Item=Value>>(&mut self, patterns: &[ast::MatchPattern], values: T) -> bool {
        patterns.iter()
            .zip(values)
            .all(|(p, v)| self.bind_pattern(p, v))
    }
    
    fn bind_one(&mut self, var: &ast::SimpleName, value: Value) {
        if self.frame().set(var.name_id, value, false) {
            let name = self.get_name(var.name_id).to_string();
            self.warning(Warning::PatternNameAlreadyBound(name), var.range);
        }
    }
}
