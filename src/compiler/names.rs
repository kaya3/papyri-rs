use crate::errors;
use crate::parser::{ast, Type};
use crate::utils::str_ids;
use super::base::Compiler;
use super::html::HTML;
use super::value::{Value, Int, List};

impl <'a> Compiler<'a> {
    /// Returns the value of the given variable, coerced to the given type, or
    /// `None` if a compilation error occurs.
    pub(super) fn evaluate_name(&mut self, name: &ast::Name, type_hint: &Type) -> Result<Value, errors::AlreadyReported> {
        let value = match name {
            ast::Name::Simple(name) => self.get_var(name.name_id),
            ast::Name::Attr(attr) => self.evaluate_attr(attr),
            ast::Name::Index(index) => self.evaluate_index(index),
        };
        value.and_then(|v| self.coerce(v, type_hint))
            .map_err(|e| self.report(e, name.range()))
    }
    
    fn evaluate_attr(&mut self, attr: &ast::AttrName) -> Result<Value, errors::PapyriError> {
        let subject = self.evaluate_name(&attr.subject, &Type::Any)?;
        let attr_id = attr.attr_name_id;
        
        if subject.is_unit() && attr.is_coalescing {
            return Ok(Value::UNIT);
        } else if let Value::Dict(ref vs) = subject {
            if let Some(v) = vs.get(&attr_id) {
                return Ok(v.clone());
            }
        } else if let Value::HTML(HTML::Tag(ref t)) = subject {
            if attr_id == str_ids::TAG_NAME {
                return Ok(self.get_name(t.name_id).into())
            }
        }
        
        self.evaluate_native_attr(subject, attr_id)
            .map(Value::from)
    }
    
    fn evaluate_index(&mut self, index: &ast::IndexName) -> Result<Value, errors::PapyriError> {
        let type_hint = Type::Any.list().option_if(index.is_coalescing);
        let Some(subject) = self.evaluate_name(&index.subject, &type_hint)?
            .expect_convert::<Option<List>>() else {
                return Ok(Value::UNIT);
            };
        
        self.list_get(subject, index.index)
    }
    
    pub(super) fn list_get(&mut self, list: List, i: Int) -> Result<Value, errors::PapyriError> {
        let n = list.len() as Int;
        if i >= 0 && i < n {
            Ok(list.get(i as usize).clone())
        } else if i >= -n && i < 0 {
            Ok(list.get((i + n) as usize).clone())
        } else {
            let e = errors::RuntimeError::IndexOutOfRange(i, list.len());
            Err(e.into())
        }
    }
}
