use std::collections::HashMap;

use crate::utils::{Diagnostics, ice_at, SourceRange, str_ids};
use crate::parser::{ast, Token};
use super::compiler::Compiler;
use super::html::HTML;
use super::value::Value;

#[derive(Clone, PartialEq, Eq)]
pub enum Type {
    Unit,
    AnyValue,
    AnyHTML,
    Block,
    Inline,
    Bool,
    Int,
    Str,
    Function,
    Dict(Box<Type>),
    List(Box<Type>),
    Optional(Box<Type>),
}

impl Type {
    pub fn dict(t: Type) -> Type {
        Type::Dict(Box::new(t))
    }
    
    pub fn list(t: Type) -> Type {
        Type::List(Box::new(t))
    }
    
    pub fn optional(t: Type) -> Type {
        if matches!(t, Type::Optional(_)) {
            t
        } else {
            Type::Optional(Box::new(t))
        }
    }
    
    pub fn is_html(&self) -> bool {
        matches!(self, Type::AnyHTML | Type::Block | Type::Inline)
    }
    
    pub fn component_type(&self) -> Type {
        match self {
            Type::Dict(t) | Type::List(t) => *t.clone(),
            t => t.clone(),
        }
    }
    
    pub fn least_upper_bound(&self, other: &Type) -> Type {
        match (self, other) {
            (t1, t2) if t1 == t2 => t1.clone(),
            (Type::AnyValue, t) | (t, Type::AnyValue) => t.clone(),
            
            (t1, t2) if t1.is_html() && t2.is_html() => Type::AnyHTML,
            (Type::AnyHTML, _) | (_, Type::AnyHTML) => Type::AnyHTML,
            
            (Type::List(t1), Type::List(t2)) => Type::list(t1.least_upper_bound(t2)),
            (Type::Dict(t1), Type::Dict(t2)) => Type::dict(t1.least_upper_bound(t2)),
            
            (Type::Dict(_) | Type::List(_), Type::Block) | (Type::Block, Type::Dict(_) | Type::List(_)) => Type::Block,
            (Type::Block, _) | (_, Type::Block) => Type::AnyHTML,
            _ => Type::Inline,
        }
    }
    
    pub fn compile(type_annotation: &ast::TypeAnnotation) -> Type {
        match type_annotation {
            ast::TypeAnnotation::Primitive(range) => match range.as_str() {
                "any" => Type::AnyValue,
                "html" => Type::AnyHTML,
                "block" => Type::Block,
                "inline" => Type::Inline,
                "none" => Type::Unit,
                "bool" => Type::Bool,
                "int" => Type::Int,
                "str" => Type::Str,
                "function" => Type::Function,
                _ => ice_at("not a primitive type", range)
            },
            ast::TypeAnnotation::Group(child, range) => {
                let child = Type::compile(child);
                match range.as_str() {
                    "dict" => Type::dict(child),
                    "list" => Type::list(child),
                    "?" => Type::optional(child),
                    _ => ice_at("not a group type", range)
                }
            },
        }
    }
}

impl std::fmt::Debug for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::AnyValue => write!(f, "any"),
            Type::AnyHTML => write!(f, "html"),
            Type::Block => write!(f, "block"),
            Type::Inline => write!(f, "inline"),
            Type::Unit => write!(f, "none"),
            Type::Bool => write!(f, "bool"),
            Type::Int => write!(f, "int"),
            Type::Str => write!(f, "str"),
            Type::Function => write!(f, "function"),
            Type::Dict(t) => write!(f, "{:?} dict", t),
            Type::List(t) => write!(f, "{:?} list", t),
            Type::Optional(t) => write!(f, "{:?}?", t),
        }
    }
}

impl Diagnostics {
    pub fn err_expected_type(&mut self, expected: &Type, was: &Type, range: &SourceRange) {
        self.type_error(&format!("expected {:?}, was {:?}", expected, was), range);
    }
}

impl <'a> Compiler<'a> {
    pub fn coerce(&mut self, value: Value, mut expected: &Type, range: &SourceRange) -> Option<Value> {
        if let Type::Optional(t) = expected {
            if matches!(value, Value::Unit) { return Some(value); }
            expected = t;
        }
        
        match (expected, &value) {
            (Type::AnyValue, _) |
            (Type::Unit, Value::Unit) |
            (Type::Bool, Value::Bool(_)) |
            (Type::Int, Value::Int(_)) |
            (Type::Str, Value::Str(_)) |
            (Type::AnyHTML, Value::HTML(_)) |
            (Type::Function, Value::Func(_) | Value::NativeFunc(_)) => return Some(value),
            
            (Type::Str, Value::Bool(b)) => return Some(Value::str(Token::bool_to_string(*b))),
            (Type::Str, Value::Int(i)) => return Some(Value::str(&i.to_string())),
            
            (Type::Optional(_), _) |
            (_, Value::Func(_) | Value::NativeFunc(_)) => {},
            
            (Type::Dict(t), Value::Dict(vs)) => {
                let mut coerced_vs = HashMap::new();
                let mut any_errors = false;
                for (k, v) in vs.iter() {
                    if let Some(coerced_v) = self.coerce(v.clone(), t, range) {
                        coerced_vs.insert(k.clone(), coerced_v);
                    } else {
                        any_errors = true;
                        break;
                    }
                }
                if !any_errors { return Some(Value::dict(coerced_vs)); }
            },
            (Type::List(t), Value::List(vs)) => {
                let mut coerced_vs = Vec::new();
                let mut any_errors = false;
                for v in vs.iter() {
                    if let Some(coerced_v) = self.coerce(v.clone(), t, range) {
                        coerced_vs.push(coerced_v);
                    } else {
                        any_errors = true;
                        break;
                    }
                }
                if !any_errors { return Some(Value::list(coerced_vs)); }
            },
            
            (Type::AnyHTML, _) => return Some(Value::HTML(self.compile_value(value))),
            (Type::Block, _) => {
                let mut r = self.compile_value(value);
                if !r.is_block() {
                    r = HTML::tag(str_ids::P, r);
                }
                return Some(Value::HTML(r));
            },
            (Type::Inline, Value::Dict(_) | Value::List(_)) => {},
            (Type::Inline, _) => {
                let v = self.compile_value(value);
                return if v.is_block() {
                    self.diagnostics.err_expected_type(&Type::Inline, &Type::Block, range);
                    None
                } else {
                    Some(Value::HTML(v))
                }
            },
            
            (Type::Unit, Value::HTML(HTML::Empty)) => return Some(Value::Unit),
            
            _ => {},
        }
        
        self.diagnostics.err_expected_type(expected, &value.get_type(), &range);
        None
    }
}
