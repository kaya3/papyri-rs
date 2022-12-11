use crate::errors;
use crate::parser::ast;
use crate::utils::{str_ids, SourceRange};
use super::base::Compiler;
use super::func::Func;
use super::html::HTML;
use super::types::Type;
use super::value::Value;

impl <'a> Compiler<'a> {
    /// Returns the value of the given variable, coerced to the given type, or
    /// `None` if a compilation error occurs.
    pub fn evaluate_name(&mut self, name: &ast::Name, type_hint: &Type) -> Option<Value> {
        let value = match name {
            ast::Name::SimpleName(name) => {
                self.get_var(name.name_id, &name.range)?
            },
            ast::Name::AttrName(attr) => {
                self.evaluate_attr(attr)?
            },
        };
        self.coerce(value, type_hint, name.range())
    }
    
    fn evaluate_attr(&mut self, attr: &ast::AttrName) -> Option<Value> {
        let subject = self.evaluate_name(&attr.subject, &Type::Any)?;
        let attr_id = attr.attr_name_id;
        let natives = &self.ctx.natives;
        
        match &subject {
            v if v.is_unit() && attr.is_coalescing => return Some(Value::UNIT),
            
            Value::Bool(_) => {},
            
            Value::Int(i) => match attr_id {
                str_ids::ADD => return self.bind_pos_arg(natives.add.clone(), subject, &attr.range),
                str_ids::NEGATE => return Some((-i).into()),
                _ => {},
            },
            
            Value::Str(s) => match attr_id {
                str_ids::ESCAPE_HTML => return self.bind_method(natives.escape_html.clone(), subject, &attr.range),
                str_ids::IS_EMPTY => return Some(s.is_empty().into()),
                str_ids::LEN => return Some(Value::Int(s.len() as i64)),
                _ => {},
            },
            
            Value::Dict(vs) => {
                if let Some(v) = vs.get(&attr_id) {
                    return Some(v.clone());
                }
            },
            
            Value::List(vs) => match attr_id {
                str_ids::IS_EMPTY => return Some(vs.is_empty().into()),
                str_ids::LEN => return Some(Value::Int(vs.len() as i64)),
                str_ids::FILTER => return self.bind_method(natives.filter.clone(), subject, &attr.range),
                str_ids::JOIN => return self.bind_method(natives.join.clone(), subject, &attr.range),
                str_ids::MAP => return self.bind_method(natives.map.clone(), subject, &attr.range),
                str_ids::SLICE => return self.bind_method(natives.slice.clone(), subject, &attr.range),
                str_ids::SORTED => return self.bind_method(natives.sorted.clone(), subject, &attr.range),
                _ => {},
            },
            
            Value::Func(f) => match attr_id {
                str_ids::BIND => return self.bind_pos_arg(natives.bind.clone(), f.clone().into(), &attr.range),
                str_ids::NAME => return Some(self.get_name(f.name_id()).into()),
                _ => {},
            },
            
            Value::HTML(h) => match attr_id {
                str_ids::ESCAPE_HTML => return self.bind_method(natives.escape_html.clone(), subject, &attr.range),
                str_ids::TAG_NAME => if let HTML::Tag(t) = h { return Some(self.get_name(t.name_id).into()); },
                _ => {},
            },
        }
        
        let name = self.get_name(attr.attr_name_id).to_string();
        self.name_error(errors::NameError::NoSuchAttribute(subject.get_type(), name), &attr.range);
        None
    }
    
    fn bind_pos_arg(&mut self, f: Func, arg: Value, attr_range: &SourceRange) -> Option<Value> {
        f.bind_pos_arg(self, arg, attr_range)
            .map(Value::from)
    }
    
    fn bind_method(&mut self, f: Func, subject: Value, attr_range: &SourceRange) -> Option<Value> {
        f.bind_content(self, subject, attr_range)
            .map(Value::from)
    }
}
