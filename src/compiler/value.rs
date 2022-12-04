use std::rc::Rc;
use indexmap::IndexMap;

use crate::errors;
use crate::parser::{ast, AST, Token, TokenKind, text};
use crate::utils::{NameID, str_ids, SliceRef, SourceRange, equals};
use super::compiler::Compiler;
use super::func::Func;
use super::html::HTML;
use super::tag::Tag;
use super::types::Type;

#[derive(Debug, Clone)]
/// A value represented during compilation of a Papyri source file.
pub enum Value {
    /// A Boolean value, either `True` or `False`.
    Bool(bool),
    
    /// A 64-bit signed integer value.
    Int(i64),
    
    /// A string value.
    Str(Rc<str>),
    
    /// A list of values.
    List(SliceRef<Value>),
    
    /// A dictionary of values. The dictionary keys are strings, and must be
    /// valid identifiers not beginning with underscores.
    Dict(Rc<ValueMap>),
    
    /// Some HTML content, represented as a value.
    HTML(HTML),
    
    /// A function, either defined in a Papyri source file, or with a native
    /// implementation.
    Func(Func),
}

pub type ValueMap = IndexMap<NameID, Value>;

impl From<&str> for Value {
    fn from(s: &str) -> Self {
        Value::Str(Rc::from(s))
    }
}

impl From<String> for Value {
    fn from(s: String) -> Self {
        Value::Str(Rc::from(s))
    }
}

impl From<Option<Rc<str>>> for Value {
    fn from(s: Option<Rc<str>>) -> Self {
        s.map_or(Value::UNIT, Value::Str)
    }
}

impl From<Vec<Value>> for Value {
    fn from(vs: Vec<Value>) -> Self {
        Value::List(vs.into())
    }
}

impl From<ValueMap> for Value {
    fn from(vs: ValueMap) -> Self {
        Value::Dict(Rc::new(vs))
    }
}

impl From<HTML> for Value {
    fn from(html: HTML) -> Self {
        Value::HTML(html)
    }
}

impl From<&HTML> for Value {
    fn from(html: &HTML) -> Self {
        Value::HTML(html.clone())
    }
}

impl Value {
    /// The unit value, of type `none`.
    pub const UNIT: Value = Value::HTML(HTML::Empty);
    
    /// Indicates whether this value is the unit value, `Value::UNIT`.
    pub fn is_unit(&self) -> bool {
        matches!(self, Value::HTML(HTML::Empty))
    }
    
    /// Convenience method for coercing a value to an optional string. Returns
    /// `Some` if this value is a string, or `None` if this is `Value::UNIT`;
    /// the value must not be of any other type.
    pub fn to_optional_rc_str(&self, range: &SourceRange) -> Option<Rc<str>> {
        match self {
            Value::Str(s) => Some(s.clone()),
            v if v.is_unit() => None,
            _ => errors::ice_at("failed to coerce", range),
        }
    }
    
    /// Returns the type of this value. If it is a heterogeneous list or
    /// dictionary, then the strongest representable type is chosen.
    pub fn get_type(&self) -> Type {
        match self {
            Value::Bool(..) => Type::Bool,
            Value::Int(..) => Type::Int,
            Value::Str(..) => Type::Str,
            Value::Dict(vs) => {
                if vs.is_empty() { return Type::dict(Type::Unit); }
                let mut vs = vs.values();
                let mut t = vs.next().unwrap().get_type();
                for v in vs {
                    t = t.least_upper_bound(&v.get_type());
                }
                Type::dict(t)
            },
            Value::List(vs) => {
                let vs = vs.as_ref();
                if vs.is_empty() { return Type::list(Type::Unit); }
                let mut t = vs[0].get_type();
                for v in &vs[1..] {
                    t = t.least_upper_bound(&v.get_type());
                }
                Type::list(t)
            },
            Value::HTML(html) => if html.is_empty() { Type::Unit } else if html.is_block() { Type::Block } else { Type::Inline },
            Value::Func(..) => Type::Function,
        }
    }
    
    /// Determines whether two values are equal; lists and dictionaries are
    /// compared deeply. Functions are never equal, not even to themselves.
    pub fn equals(&self, other: &Value) -> bool {
        match (self, other) {
            (Value::Bool(b1), Value::Bool(b2)) => b1 == b2,
            (Value::Int(i1), Value::Int(i2)) => i1 == i2,
            (Value::Str(s1), Value::Str(s2)) => s1 == s2,
            (Value::HTML(h1), Value::HTML(h2)) => h1.equals(h2),
            
            (Value::List(v1), Value::List(v2)) => equals::equal_lists(v1.as_ref(), v2.as_ref(), Value::equals),
            (Value::Dict(v1), Value::Dict(v2)) => equals::equal_maps(v1.as_ref(), v2.as_ref(), Value::equals),
            
            (Value::Str(s1), Value::HTML(h)) |
            (Value::HTML(h), Value::Str(s1)) => h.to_string().map_or(false, |s2| s1.as_ref() == s2.as_str()),
            
            _ => false,
        }
    }
}

impl <'a> Compiler<'a> {
    /// Converts a value to its HTML representation. Lists become `<ul>` tags,
    /// dictionaries become tables, and functions will be represented as
    /// `<code>(@fn name)</code>`.
    pub fn compile_value(&mut self, value: Value) -> HTML {
        use crate::parser;
        
        match value {
            Value::Bool(b) => parser::Token::bool_to_string(b).into(),
            Value::Int(t) => parser::text::substitutions(&t.to_string()).into(),
            Value::Str(t) => t.into(),
            Value::Dict(vs) => {
                let rows: Vec<HTML> = vs.iter()
                    .map(|(&k, v)| HTML::tag(str_ids::TR, HTML::seq([
                        HTML::tag(str_ids::TH, self.get_name(k).into()),
                        HTML::tag(str_ids::TD, self.compile_value(v.clone())),
                    ])))
                    .collect();
                
                Tag::new(str_ids::TABLE, HTML::seq(rows))
                    .str_attr(str_ids::CLASS, "tabular-data")
                    .into()
            },
            Value::List(vs) => {
                let items: Vec<_> = vs.as_ref()
                    .iter()
                    .map(|child| HTML::tag(str_ids::LI, self.compile_value(child.clone())))
                    .collect();
                HTML::tag(str_ids::UL, HTML::seq(items))
            },
            Value::HTML(html) => html,
            
            Value::Func(f) => {
                let name = self.get_name(f.name_id());
                HTML::tag(str_ids::CODE, format!("(@fn {name})").into())
            },
        }
    }
    
    /// Evaluates an AST node to a value. Returns `None` if a compilation error
    /// occurs.
    pub fn evaluate_node(&mut self, node: &AST, type_hint: &Type) -> Option<Value> {
        let v = match node {
            AST::LiteralValue(tok) => if tok.kind == TokenKind::Dot {
                Value::UNIT
            } else if type_hint.is_html() {
                // This is a shortcut, to avoid parsing ints and bools and then
                // converting back to text. It also avoids the possibility of a
                // parse error if an integer is not in the signed 64-bit range.
                text::substitutions(tok.text().unwrap()).into()
            } else {
                self.evaluate_literal(tok)?
            },
            AST::Verbatim(tok) => if type_hint.is_html() {
                // This gives a different result to `evaluate_literal` + `coerce`,
                // but it gives the expected result of formatting code blocks if
                // they appear where HTML is expected.
                return self.evaluate_code_or_code_block(tok, type_hint);
            } else {
                self.evaluate_literal(tok)?
            },
            
            AST::Match(m) => return self.evaluate_match(m, type_hint),
            AST::FuncCall(call) => return self.evaluate_func_call(call, type_hint),
            AST::FuncDef(def) => Value::Func(self.compile_func_def(def)),
            AST::VarName(var) => return self.evaluate_var(var, type_hint),
            AST::Template(parts, ..) => self.evaluate_template(parts),
            
            AST::Group(..) | AST::Tag(..) => self.compile_node(node).into(),
            AST::List(list, range) => self.evaluate_list(list, type_hint, range)?,
            
            _ => errors::ice_at("invalid AST value", node.range()),
        };
        self.coerce(v, type_hint, node.range())
    }
    
    /// Returns the value of the given variable, coerced to the given type, or
    /// `None` if a compilation error occurs.
    pub fn evaluate_var(&mut self, var: &ast::VarName, type_hint: &Type) -> Option<Value> {
        let value = self.get_var(var.name_id, &var.range)?;
        self.coerce(value, type_hint, &var.range)
    }
    
    fn evaluate_list(&mut self, list: &[(AST, bool)], type_hint: &Type, range: &SourceRange) -> Option<Value> {
        let child_type_hint = match type_hint {
            Type::AnyValue => type_hint,
            Type::List(t) => t,
            t if t.is_html() => &Type::AnyHTML,
            t => {
                self.err_expected_type(t, &Type::list(Type::AnyValue), range);
                return None;
            },
        };
        
        let mut children = Vec::new();
        for &(ref child, is_spread) in list.iter() {
            if is_spread {
                let Value::List(grandchildren) = self.evaluate_node(child, &Type::list(child_type_hint.clone()))? else {
                    errors::ice_at("failed to coerce", child.range());
                };
                children.extend(grandchildren.as_ref().iter().cloned());
            } else if let Some(child) = self.evaluate_node(child, child_type_hint) {
                children.push(child);
            }
        }
        Some(children.into())
    }
    
    fn evaluate_template(&mut self, parts: &[ast::TemplatePart]) -> Value {
        let mut out = "".to_string();
        for part in parts.iter() {
            match part {
                ast::TemplatePart::Literal(t) => {
                    out += t.as_str();
                }
                ast::TemplatePart::LiteralStr(s) => {
                    out += s;
                },
                ast::TemplatePart::VarName(var) => {
                    if let Some(v) = self.evaluate_var(var, &Type::optional(Type::Str)) {
                        if let Some(s) = v.to_optional_rc_str(&var.range) {
                            out += s.as_ref();
                        }
                    }
                },
                ast::TemplatePart::Whitespace => {
                    out += " ";
                },
            }
        }
        out.into()
    }
    
    /// Evaluates a literal token to a value, or returns `None` if a parse
    /// error occurs. The token must be either a `Boolean`, `Number`, `Name` or
    /// `Verbatim`.
    pub fn evaluate_literal(&mut self, tok: &Token) -> Option<Value> {
        match tok.kind {
            TokenKind::Boolean => Some(Value::Bool(tok.get_bool_value())),
            TokenKind::Name => Some(tok.as_str().into()),
            TokenKind::Verbatim => Some(tok.get_verbatim_text().into()),
            
            TokenKind::Number => match i64::from_str_radix(tok.as_str(), 10) {
                Ok(value) => Some(Value::Int(value)),
                Err(err) => {
                    self.ctx.diagnostics.syntax_error(errors::SyntaxError::TokenInvalidNumber(err), &tok.range);
                    None
                },
            },
            
            _ => errors::ice_at("illegal token kind", &tok.range),
        }
    }
    
    fn evaluate_code_or_code_block(&mut self, tok: &Token, type_hint: &Type) -> Option<Value> {
        let f_name = if tok.is_multiline_verbatim() { str_ids::CODE_BLOCK } else { str_ids::CODE };
        let func = self.get_var(f_name, &tok.range)?;
        let Value::Func(f) = self.coerce(func, &Type::Function, &tok.range)? else {
            errors::ice_at("failed to coerce", &tok.range);
        };
        
        let content = tok.get_verbatim_text().into();
        let bindings = f.signature().bind_synthetic_call(self, true, content, &tok.range)?;
        self.evaluate_func_call_with_bindings(f, bindings, type_hint, &tok.range)
    }
}
