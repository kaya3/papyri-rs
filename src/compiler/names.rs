use crate::errors;
use crate::parser::{ast, Type};
use crate::utils::sourcefile::SourceRange;
use crate::utils::{str_ids, SliceRef};
use super::base::Compiler;
use super::html::HTML;
use super::value::Value;

impl <'a> Compiler<'a> {
    /// Returns the value of the given variable, coerced to the given type, or
    /// `None` if a compilation error occurs.
    pub(super) fn evaluate_name(&mut self, name: &ast::Name, type_hint: &Type) -> Option<Value> {
        let value = match name {
            ast::Name::Simple(name) => {
                self.get_var(name.name_id, name.range)?
            },
            ast::Name::Attr(attr) => {
                self.evaluate_attr(attr)?
            },
            ast::Name::Index(index) => {
                self.evaluate_index(index)?
            },
        };
        self.coerce(value, type_hint, name.range())
    }
    
    fn evaluate_attr(&mut self, attr: &ast::AttrName) -> Option<Value> {
        let subject = self.evaluate_name(&attr.subject, &Type::Any)?;
        let attr_id = attr.attr_name_id;
        
        if subject.is_unit() && attr.is_coalescing {
            return Some(Value::UNIT);
        } else if let Value::Dict(ref vs) = subject {
            if let Some(v) = vs.get(&attr_id) {
                return Some(v.clone());
            }
        } else if let Value::HTML(HTML::Tag(ref t)) = subject {
            if attr_id == str_ids::TAG_NAME {
                return Some(self.get_name(t.name_id).into())
            }
        }
        
        if let Some(f) = self.evaluate_native_attr(subject.clone(), attr_id, attr.range) {
            return Some(f.into());
        }
        
        let name = self.get_name(attr_id);
        self.name_error(errors::NameError::NoSuchAttribute(subject.get_type(), name), attr.range);
        None
    }
    
    fn evaluate_index(&mut self, index: &ast::IndexName) -> Option<Value> {
        let type_hint = Type::Any.list().option_if(index.is_coalescing);
        let subject: Option<SliceRef<Value>> = self.evaluate_name(&index.subject, &type_hint)?
            .expect_convert();
        
        if let Some(subject) = subject {
            self.list_get(subject, index.index, index.range)
        } else {
            Some(Value::UNIT)
        }
    }
    
    pub(super) fn list_get(&mut self, list: SliceRef<Value>, i: i64, range: SourceRange) -> Option<Value> {
        let n = list.len() as i64;
        if i >= 0 && i < n {
            Some(list.get(i as usize).clone())
        } else if i >= -n && i < 0 {
            Some(list.get((i + n) as usize).clone())
        } else {
            self.runtime_error(errors::RuntimeError::IndexOutOfRange(i, list.len()), range);
            None
        }
    }
}
