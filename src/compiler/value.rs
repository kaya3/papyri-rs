use std::rc::Rc;

use crate::errors::TypeError;
use crate::parser::{ast, token, text, Expr, Type};
use crate::utils::{str_ids, SliceRef, taginfo, NameIDMap};
use crate::utils::sourcefile::SourceRange;
use super::base::Compiler;
use super::func::Func;
use super::html::HTML;
use super::regex_value::RegexValue;
use super::tag::Tag;

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
    
    /// Some HTML content, represented as a value. HTML text content is instead
    /// represented as `Value::Str`.
    HTML(HTML),
    
    /// A function, either defined in a Papyri source file, or with a native
    /// implementation.
    Func(Func),
    
    /// A regular expression.
    Regex(Rc<RegexValue>),
}

pub type ValueMap = NameIDMap<Value>;

impl Value {
    /// The unit value, of type `none`.
    pub(super) const UNIT: Value = Value::HTML(HTML::Empty);
    
    /// Indicates whether this value is the unit value, `Value::UNIT`.
    pub(super) fn is_unit(&self) -> bool {
        matches!(self, Value::HTML(HTML::Empty))
    }
    
    /// Returns the type of this value. If it is a heterogeneous list or
    /// dictionary, then the strongest representable type is chosen.
    pub fn get_type(&self) -> Type {
        match self {
            Value::Bool(..) => Type::Bool,
            Value::Int(..) => Type::Int,
            Value::Str(..) => Type::Str,
            Value::Dict(vs) => Value::common_type_of(vs.values()).dict(),
            Value::List(vs) => Value::common_type_of(vs.as_ref().iter()).list(),
            Value::HTML(html) => if html.is_empty() { Type::Unit } else if html.is_block() { Type::Block } else { Type::Inline },
            Value::Func(..) => Type::Function,
            Value::Regex(..) => Type::Regex,
        }
    }
    
    /// Returns the strongest type assignable from all values in the given
    /// iterator.
    pub(super) fn common_type_of<'a, T: Iterator<Item=&'a Value>>(vs: T) -> Type {
        vs.map(Value::get_type)
            .reduce(Type::least_upper_bound)
            .unwrap_or(Type::Unit)
    }
}

impl PartialEq for Value {
    /// Determines whether two values are equal; lists and dictionaries are
    /// compared deeply. Functions are never equal, not even to themselves.
    fn eq(&self, other: &Value) -> bool {
        match (self, other) {
            (Value::Bool(b1), Value::Bool(b2)) => b1 == b2,
            (Value::Int(i1), Value::Int(i2)) => i1 == i2,
            (Value::Str(s1), Value::Str(s2)) => s1 == s2,
            (Value::HTML(h1), Value::HTML(h2)) => h1 == h2,
            (Value::Regex(r1), Value::Regex(r2)) => r1.as_str() == r2.as_str(),
            (Value::List(v1), Value::List(v2)) => v1 == v2,
            (Value::Dict(v1), Value::Dict(v2)) => v1 == v2,
            
            _ => false,
        }
    }
}
impl Eq for Value {}

impl Default for Value {
    fn default() -> Value {
        Value::UNIT
    }
}

impl <'a> Compiler<'a> {
    /// Converts a value to its HTML representation. Lists become `<ul>` tags,
    /// dictionaries become tables, functions and regexes are represented as
    /// code.
    pub(super) fn compile_value(&self, value: Value) -> HTML {
        match value {
            Value::Bool(b) => token::Token::bool_to_string(b).into(),
            Value::Int(t) => text::substitutions(&t.to_string()).into(),
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
                    .cloned()
                    .map(|child| HTML::tag(str_ids::LI, self.compile_value(child)))
                    .collect();
                HTML::tag(str_ids::UL, HTML::seq(items))
            },
            Value::HTML(html) => html,
            
            Value::Func(f) => {
                let name = self.get_name(f.name_id());
                HTML::tag(str_ids::CODE, format!("(@fn {name})").into())
            },
            
            Value::Regex(r) => {
                HTML::tag(str_ids::CODE, format!("(@regex {})", r.as_str()).into())
            },
        }
    }
    
    /// Evaluates an AST node to a value. Returns `None` if a compilation error
    /// occurs.
    pub(super) fn evaluate_node(&mut self, node: &Expr, type_hint: &Type) -> Option<Value> {
        let v = match node {
            Expr::Unit(..) => Value::UNIT,
            &Expr::Bool(b, ..) => b.into(),
            &Expr::Int(i, ..) => i.into(),
            &Expr::BareString(range) => self.get_source_str(range).into(),
            &Expr::Verbatim(range) => self.evaluate_verbatim(range),
            
            Expr::FuncCall(call) => return self.evaluate_func_call(call, type_hint),
            Expr::FuncDef(def) => {
                let v = Value::Func(self.compile_func_def(def));
                if type_hint.is_html() {
                    self.report(TypeError::ExpectedWas(type_hint.clone(), v.get_type()), def.range);
                    return None;
                }
                v
            },
            Expr::LetIn(let_in) => return self.evaluate_let_in(let_in, type_hint, false),
            Expr::Match(match_) => return self.evaluate_match(match_, type_hint),
            Expr::Name(name) => return self.evaluate_name(name, type_hint),
            Expr::Template(parts, ..) => self.evaluate_template(parts),
            
            Expr::Group(children, ..) => self.compile_sequence(children, taginfo::ContentKind::ALLOW_P).into(),
            Expr::Tag(tag) => self.compile_tag(tag).into(),
            Expr::List(children, range) => self.evaluate_list(children, type_hint, *range)?,
        };
        self.coerce(v, type_hint, node.range())
    }
    
    pub(super) fn evaluate_list(&mut self, list: &[(Expr, bool)], type_hint: &Type, range: SourceRange) -> Option<Value> {
        let child_type_hint = type_hint.component_type();
        let mut children = Vec::new();
        for &(ref child, is_spread) in list.iter() {
            if is_spread {
                // can't necessarily use `type_hint` here, because it might not be a list type
                let grandchildren: SliceRef<Value> = self.evaluate_node(child, &child_type_hint.clone().list())?
                    .expect_convert();
                children.extend(grandchildren.as_ref().iter().cloned());
            } else if let Some(child) = self.evaluate_node(child, child_type_hint) {
                children.push(child);
            }
        }
        self.coerce(children.into(), type_hint, range)
    }
    
    fn evaluate_template(&mut self, parts: &[ast::TemplatePart]) -> Value {
        let mut out = "".to_string();
        for part in parts.iter() {
            match part {
                ast::TemplatePart::Literal(range) => {
                    out += self.get_source_str(*range);
                }
                ast::TemplatePart::LiteralChar(c) => {
                    out.push(*c);
                },
                ast::TemplatePart::Name(name) => {
                    if let Some(v) = self.evaluate_name(name, &Type::Str.option()) {
                        if let Some(s) = v.expect_convert::<Option<Rc<str>>>() {
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
    
    pub(super) fn evaluate_let_in(&mut self, let_in: &ast::LetIn, type_hint: &Type, do_export: bool) -> Option<Value> {
        let frame = self.frame()
            .to_inactive()
            .new_empty_child_frame();
        self.evaluate_in_frame(frame, |_self| {
            for &(name_id, ref value) in let_in.vars.iter() {
                let v = _self.evaluate_node(value, &Type::Any)?;
                let range = value.range();
                if do_export { _self.export(name_id, v.clone(), range); }
                _self.set_var(name_id, v, let_in.is_implicit, range);
            }
            _self.evaluate_node(&let_in.child, type_hint)
        })
    }
    
    pub(super) fn evaluate_verbatim(&mut self, range: SourceRange) -> Value {
        token::Token::get_verbatim_text(self.get_source_str(range))
            .into()
    }
    
    pub(super) fn compile_code_fence(&mut self, range: SourceRange, is_multiline: bool) -> Option<HTML> {
        let str_value = self.evaluate_verbatim(range);
        
        let f_name = if is_multiline { str_ids::CODE_BLOCK } else { str_ids::CODE };
        let func = self.get_var(f_name, range)?;
        let func: Func = self.coerce(func, &Type::Function, range)?
            .expect_convert();
        
        let bindings = func.bind_synthetic_call(self, true, str_value, range)?;
        let type_hint = if is_multiline { Type::Block } else { Type::Inline };
        self.evaluate_func_call_with_bindings(func, bindings, &type_hint, range)
            .map(Value::expect_convert)
    }
}
