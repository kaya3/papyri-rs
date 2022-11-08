use std::collections::HashMap;
use std::rc::Rc;

use crate::parser::{ast, AST, Token, TokenKind};
use crate::utils::{ice_at, NameID, str_ids};
use super::compiler::Compiler;
use super::func::Func;
use super::html::HTML;
use super::native::NativeFunc;
use super::types::Type;

#[derive(Debug, Clone)]
pub enum Value {
    Unit,
    Bool(bool),
    Int(i64),
    Str(Rc<str>),
    List(Rc<Vec<Value>>),
    Dict(Rc<ValueMap>),
    HTML(HTML),
    Func(Func),
    NativeFunc(NativeFunc),
}

pub type ValueMap = HashMap<NameID, Value>;

impl Value {
    pub fn int(i: i64) -> Value {
        Value::Int(i)
    }
    
    pub fn str(s: &str) -> Value {
        Value::Str(Rc::from(s))
    }
    
    pub fn dict(vs: ValueMap) -> Value {
        Value::Dict(Rc::new(vs))
    }
    
    pub fn list(vs: Vec<Value>) -> Value {
        Value::List(Rc::new(vs))
    }
    
    pub fn get_type(&self) -> Type {
        match self {
            Value::Unit => Type::Unit,
            Value::Bool(_) => Type::Bool,
            Value::Int(_) => Type::Int,
            Value::Str(_) => Type::Str,
            Value::Dict(vs) => {
                if vs.is_empty() { return Type::Unit; }
                let mut vs = vs.values();
                let mut t = vs.next().unwrap().get_type();
                for v in vs {
                    t = t.least_upper_bound(&v.get_type());
                }
                Type::dict(t)
            },
            Value::List(vs) => {
                if vs.is_empty() { return Type::Unit; }
                let mut t = vs[0].get_type();
                for v in &vs[1..] {
                    t = t.least_upper_bound(&v.get_type());
                }
                Type::list(t)
            },
            Value::HTML(html) => if html.is_block() { Type::Block } else { Type::Inline },
            Value::Func(_) | Value::NativeFunc(_) => Type::Function,
        }
    }
}

impl <'a> Compiler<'a> {
    pub fn compile_value(&mut self, value: Value) -> HTML {
        HTML::from_value(value, &mut self.loader.string_pool)
    }
    
    pub fn evaluate_node(&mut self, node: &AST, type_hint: &Type) -> Option<Value> {
        let v = match node {
            AST::LiteralValue(tok) => if type_hint.is_html() {
                Value::HTML(HTML::text(&tok.text()?))
            } else {
                self.evaluate_literal(tok)?
            },
            AST::Verbatim(tok) => if type_hint.is_html() {
                self.evaluate_verbatim(&tok)?
            } else {
                Value::str(tok.get_verbatim_text())
            },
            
            AST::FuncCall(call) => return self.evaluate_func_call(call, type_hint),
            AST::FuncDef(def) => Value::Func(self.compile_func_def(def)),
            AST::VarName(name_id, range) => self.get_var(*name_id, range)?,
            AST::List(list) => self.evaluate_list(list, type_hint)?,
            
            AST::Group(_) | AST::Tag(_) => Value::HTML(self.compile_node(node)),
            
            _ => ice_at("invalid AST value", node.range()),
        };
        self.coerce(v, type_hint, node.range())
    }
    
    fn evaluate_list(&mut self, list: &ast::GroupOrList, type_hint: &Type) -> Option<Value> {
        let child_type_hint = match type_hint {
            Type::List(t) => t,
            t if t.is_html() => &Type::AnyHTML,
            t => {
                self.diagnostics.err_expected_type(t, &Type::list(Type::AnyValue), &list.range);
                return None;
            },
        };
        
        let children = list.children.iter()
            .filter_map(|child| self.evaluate_node(child, child_type_hint))
            .collect();
        
        Some(Value::list(children))
    }
    
    fn evaluate_literal(&mut self, tok: &Token) -> Option<Value> {
        match tok.kind {
            TokenKind::Dot => Some(Value::Unit),
            TokenKind::Boolean => Some(Value::Bool(tok.get_bool_value())),
            TokenKind::Name => Some(Value::str(tok.as_str())),
            TokenKind::Number => match i64::from_str_radix(tok.as_str(), 10) {
                Ok(value) => Some(Value::int(value)),
                Err(err) => {
                    self.diagnostics.error(&err.to_string(), &tok.range);
                    None
                },
            },
            
            _ => ice_at("illegal token kind", &tok.range),
        }
    }
    
    fn evaluate_verbatim(&mut self, tok: &Token) -> Option<Value> {
        let f_name = if tok.is_multiline_verbatim() { str_ids::CODE_BLOCK } else { str_ids::CODE };
        let func = self.get_var(f_name, &tok.range)?;
        let Value::Func(func) = self.coerce(func, &Type::Function, &tok.range)? else {
            ice_at("coerce didn't return function", &tok.range);
        };
        
        let content = tok.get_verbatim_text();
        let bindings = func.signature().bind_synthetic_call(self, true, Value::str(content), &tok.range)?;
        self.evaluate_func_call_with_bindings(func, bindings, &Type::AnyHTML)
    }
}
