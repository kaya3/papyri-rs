use crate::errors::{ice_at, Warning};
use crate::parser::ast;
use super::compiler::Compiler;
use super::html::HTML;
use super::types::Type;
use super::value::{Value, ValueMap};

impl <'a> Compiler<'a> {
    pub fn evaluate_match(&mut self, match_: &ast::Match, type_hint: &Type) -> Option<Value> {
        let value = self.evaluate_node(&match_.value, &Type::AnyValue)?;
        let mut bindings = ValueMap::new();
        for branch in match_.branches.iter() {
            bindings.clear();
            if self.bind_pattern(&branch.pattern, value.clone(), &mut bindings) {
                let frame = self.frame().to_inactive().new_child_frame(bindings, None);
                return self.evaluate_in_frame(frame, &branch.then, type_hint);
            }
        }
        
        self.diagnostics.warning(Warning::NoMatchingBranch, &match_.range);
        Some(Value::UNIT)
    }
    
    fn bind_pattern(&mut self, pattern: &ast::MatchPattern, value: Value, bindings: &mut ValueMap) -> bool {
        match pattern {
            ast::MatchPattern::Ignore(..) => true,
            ast::MatchPattern::LiteralNone(..) => value.is_unit(),
            ast::MatchPattern::Literal(other) => {
                let Some(other) = self.evaluate_literal(other) else { return false; };
                match (value, other) {
                    (v, o) if o.is_unit() => v.is_unit(),
                    (Value::Bool(a), Value::Bool(b)) => a == b,
                    (Value::Int(i), Value::Int(j)) => i == j,
                    (Value::Int(i), Value::Str(s)) => i.to_string() == *s,
                    (Value::Str(s), Value::Str(t)) => s == t,
                    _ => false,
                }
            },
            ast::MatchPattern::LiteralName(_, name_id) => {
                matches!(value, Value::Str(s) if s.as_ref() == self.get_name(*name_id))
            },
            ast::MatchPattern::VarName(var) => {
                self.bind_one(var, value, bindings);
                true
            },
            ast::MatchPattern::Typed(_, child, type_) => {
                match self.try_coerce(value, &Type::compile(type_)) {
                    Some(value) => self.bind_pattern(child, value, bindings),
                    None => false,
                }
            },
            ast::MatchPattern::TypeOf(_, child, t_var) => {
                let t = Value::from(value.get_type().to_string());
                self.bind_one(t_var, t, bindings);
                self.bind_pattern(child, value, bindings)
            },
            ast::MatchPattern::ExactList(_, child_patterns) => {
                let Value::List(child_values) = value else { return false; };
                child_patterns.len() == child_values.len()
                    && self.bind_all(child_patterns, child_values.as_ref().iter().cloned(), bindings)
            },
            ast::MatchPattern::ExactHTMLSeq(_, child_patterns) => {
                let Value::HTML(html) = value else { return false; };
                match html {
                    HTML::Empty => child_patterns.is_empty(),
                    HTML::Sequence(seq) => {
                        if child_patterns.len() != seq.len() { return false; }
                        let child_values = seq.iter().map(Value::from);
                        self.bind_all(child_patterns, child_values, bindings)
                    },
                    _ => child_patterns.len() == 1 && self.bind_pattern(&child_patterns[0], Value::from(html), bindings),
                }
            },
            ast::MatchPattern::SpreadList(_, child_patterns, spread_index) => {
                let Value::List(child_values) = value else { return false; };
                self.bind_all_with_spread(
                    child_patterns,
                    *spread_index,
                    child_values.len(),
                    |a, b| child_values.as_ref()[a..b].iter().cloned(),
                    |a, b| Value::List(child_values.slice(a, b)),
                    bindings,
                )
            },
            ast::MatchPattern::SpreadHTMLSeq(_, child_patterns, spread_index) => {
                let Value::HTML(html) = value else { return false; };
                
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
                    |a, b| html_slice[a..b].iter().map(Value::from),
                    |a, b| HTML::seq(html_slice[a..b].iter().cloned()).into(),
                    bindings,
                )
            },
            ast::MatchPattern::Dict(_, dict_pattern) => {
                let Value::Dict(dict_value) = value else { return false; };
                if dict_pattern.spread.is_none() && dict_value.len() != dict_pattern.attrs.len() { return false; }
                
                for (name_id, child_pattern) in dict_pattern.attrs.iter() {
                    let Some(child_value) = dict_value.get(name_id) else { return false; };
                    if !self.bind_pattern(child_pattern, child_value.clone(), bindings) { return false; }
                }
                
                if let Some(spread_pattern) = dict_pattern.spread.as_ref() {
                    if !matches!(spread_pattern, ast::MatchPattern::Ignore(..)) {
                        let remaining: ValueMap = dict_value.iter()
                            .filter(|(&k, _)| !dict_pattern.attrs.contains_key(&k))
                            .map(|(&k, v)| (k, v.clone()))
                            .collect();
                        if !self.bind_pattern(spread_pattern, Value::dict(remaining), bindings) { return false; }
                    }
                }
                true
            },
            ast::MatchPattern::Tag(_, tag_pattern) => {
                let Value::HTML(HTML::Tag(tag_value)) = value else { return false; };
                
                let tag_name = self.get_name(tag_value.name_id);
                if !self.bind_pattern(&tag_pattern.name, Value::from(tag_name), bindings) { return false; }
                
                let vs: ValueMap = tag_value.attributes
                    .iter()
                    .map(|(&k, v)| (
                        k,
                        v.clone().into(),
                    ))
                    .collect();
                
                self.bind_pattern(&tag_pattern.attrs, Value::dict(vs), bindings)
                    && self.bind_pattern(&tag_pattern.content, Value::from(&tag_value.content), bindings)
            },
            ast::MatchPattern::Regex(pattern_range, regex, name_ids) => {
                let Value::Str(value_str) = value else { return false; };
                let Some(match_) = regex.captures(value_str.as_ref()) else { return false; };
                
                if match_.len() != name_ids.len() + 1 { ice_at("incorrect number of capture groups", pattern_range); }
                let match_ = match_.iter()
                    .skip(1)
                    .zip(name_ids.iter());
                
                for (s, var) in match_ {
                    let v = s.map_or(Value::UNIT, |m| m.as_str().into());
                    self.bind_one(var, v, bindings);
                }
                true
            },
        }
    }
    
    fn bind_all_with_spread<T: Iterator<Item=Value>>(&mut self, patterns: &[ast::MatchPattern], spread_index: usize, values_len: usize, f_each: impl Fn(usize, usize) -> T, f_spread: impl Fn(usize, usize) -> Value, bindings: &mut ValueMap) -> bool {
        if values_len + 1 < patterns.len() { return false; }
        
        let i = spread_index;
        let pre_patterns = &patterns[..i];
        let spread_pattern = &patterns[i];
        let post_patterns = &patterns[i + 1..];
        
        let j = values_len - post_patterns.len();
        
        self.bind_all(pre_patterns, f_each(0, i), bindings)
            && self.bind_pattern(spread_pattern, f_spread(i, j), bindings)
            && self.bind_all(post_patterns, f_each(j, values_len), bindings)
    }
    
    fn bind_all<T: Iterator<Item=Value>>(&mut self, patterns: &[ast::MatchPattern], values: T, bindings: &mut ValueMap) -> bool {
        patterns.iter()
            .zip(values)
            .all(|(p, v)| self.bind_pattern(p, v, bindings))
    }
    
    fn bind_one(&mut self, var: &ast::VarName, value: Value, bindings: &mut ValueMap) {
        if bindings.insert(var.name_id, value).is_some() {
            let name = self.get_name(var.name_id).to_string();
            self.diagnostics.warning(Warning::PatternNameAlreadyBound(name), &var.range);
        }
    }
}
