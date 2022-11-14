use std::collections::HashMap;

use crate::parser::ast;
use crate::utils::{ice_at, NameID, SourceRange, ice};
use super::compiler::Compiler;
use super::types::Type;
use super::value::{Value, ValueMap};

impl <'a> Compiler<'a> {
    pub fn evaluate_match(&mut self, match_: &ast::Match, type_hint: &Type) -> Option<Value> {
        let value = self.evaluate_node(&match_.value, &Type::AnyValue)?;
        let mut bindings = HashMap::new();
        for branch in match_.branches.iter() {
            bindings.clear();
            if self.bind_pattern(&branch.pattern, value.clone(), &mut bindings) {
                let frame = self.frame.to_inactive().new_child_frame(bindings);
                return self.evaluate_in_frame(frame, &branch.then, type_hint);
            }
        }
        
        self.diagnostics.warning("no matching branch", &match_.range);
        None
    }
    
    fn bind_pattern(&mut self, pattern: &ast::MatchPattern, value: Value, bindings: &mut ValueMap) -> bool {
        match pattern {
            ast::MatchPattern::Ignore(..) => true,
            ast::MatchPattern::Literal(other) => {
                let Some(other) = self.evaluate_literal(other) else { return false; };
                match (value, other) {
                    (Value::Unit, Value::Unit) => true,
                    (Value::Bool(a), Value::Bool(b)) => a == b,
                    (Value::Int(i), Value::Int(j)) => i == j,
                    (Value::Int(i), Value::Str(s)) => i.to_string() == *s,
                    (Value::Str(s), Value::Str(t)) => s == t,
                    _ => false,
                }
            },
            ast::MatchPattern::VarName(var) => {
                self.bind_one(var.name_id, value, bindings, &var.range);
                true
            },
            ast::MatchPattern::Typed(_, child, type_) => {
                match self.try_coerce(value, &Type::compile(type_)) {
                    Some(value) => self.bind_pattern(child, value, bindings),
                    None => false,
                }
            },
            ast::MatchPattern::ExactList(_, child_patterns) => {
                let Value::List(child_values) = value else { return false; };
                child_patterns.len() == child_values.len()
                    && self.bind_all(child_patterns, child_values.as_ref(), bindings)
            },
            ast::MatchPattern::SpreadList(_, child_patterns, spread_index) => {
                let Value::List(child_values) = value else { return false; };
                if child_values.len() < child_patterns.len() - 1 { return false; }
                
                let i = *spread_index;
                let pre_patterns = &child_patterns[..i];
                let spread_pattern = &child_patterns[i];
                let post_patterns = &child_patterns[i + 1..];
                
                let j = child_values.len() - post_patterns.len();
                
                self.bind_all(pre_patterns, &child_values.as_ref()[..i], bindings)
                    && self.bind_spread(spread_pattern, Value::List(child_values.slice(i, j)), bindings)
                    && self.bind_all(post_patterns, &child_values.as_ref()[j..], bindings)
            },
            
            ast::MatchPattern::SpreadIgnore(range) |
            ast::MatchPattern::SpreadVarName(ast::VarName {range, ..}) => {
                ice_at("spread pattern should not occur here", range);
            },
        }
    }
    
    fn bind_all(&mut self, patterns: &[ast::MatchPattern], values: &[Value], bindings: &mut ValueMap) -> bool {
        if patterns.len() != values.len() { ice("unequal numbers of patterns and values to bind"); }
        patterns.iter()
            .zip(values.iter())
            .all(|(p, v)| self.bind_pattern(p, v.clone(), bindings))
    }
    
    fn bind_spread(&mut self, pattern: &ast::MatchPattern, value: Value, bindings: &mut ValueMap) -> bool {
        match pattern {
            ast::MatchPattern::SpreadIgnore(..) => true,
            ast::MatchPattern::SpreadVarName(var) => {
                self.bind_one(var.name_id, value, bindings, pattern.range());
                true
            },
            ast::MatchPattern::Typed(_, child_pattern, type_) => {
                let type_ = &Type::compile(type_);
                let Some(coerced_value) = self.try_coerce(value, type_) else { return false; };
                self.bind_spread(child_pattern, coerced_value, bindings)
            },
            _ => ice_at("not a spread pattern", pattern.range()),
        }
    }
    
    fn bind_one(&mut self, name_id: NameID, value: Value, bindings: &mut ValueMap, range: &SourceRange) {
        if bindings.insert(name_id, value).is_some() {
            self.diagnostics.warning(&format!("name '{}' already bound in this pattern", self.get_name(name_id)), range);
        }
    }
}
