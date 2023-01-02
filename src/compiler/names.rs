use crate::errors;
use crate::parser::{ast, Type};
use crate::utils::str_ids;
use super::base::Compiler;
use super::html::HTML;
use super::value::Value;

impl <'a> Compiler<'a> {
    /// Returns the value of the given variable, coerced to the given type, or
    /// `None` if a compilation error occurs.
    pub(super) fn evaluate_name(&mut self, name: &ast::Name, type_hint: &Type) -> Option<Value> {
        let value = match name {
            ast::Name::SimpleName(name) => {
                self.get_var(name.name_id, name.range)?
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
        
        if subject.is_unit() && attr.is_coalescing {
            return Some(Value::UNIT);
        } else if let Value::Dict(ref vs) = subject {
            if let Some(v) = vs.get(&attr_id) {
                return Some(v.clone());
            }
        } else if let Value::HTML(HTML::Tag(ref t)) = subject {
            match attr_id {
                str_ids::TAG_NAME => return Some(self.get_name(t.name_id).into()),
                _ => {},
            }
        }
        
        if let Some(f) = self.evaluate_native_attr(subject.clone(), attr_id, attr.range) {
            return Some(f.into());
        }
        
        let name = self.get_name(attr_id).to_string();
        self.name_error(errors::NameError::NoSuchAttribute(subject.get_type(), name), attr.range);
        None
    }
}
