use std::rc::Rc;
use indexmap::IndexMap;

use crate::parser::ast;
use crate::utils::{ice_at, str_ids, NameID, taginfo, text, ice, SourceRange};
use super::compiler::Compiler;
use super::html::HTML;
use super::types::Type;
use super::value::Value;

pub type AttrMap = IndexMap<NameID, Option<Rc<str>>>;

#[derive(Debug, Clone)]
pub struct Tag {
    pub name_id: NameID,
    pub attributes: AttrMap,
    pub content: HTML,
}

impl Tag {
    pub fn new(name_id: NameID, content: HTML) -> Tag {
        Tag::new_with_attrs(name_id, AttrMap::new(), content)
    }
    
    pub fn new_with_attrs(name_id: NameID, attributes: AttrMap, content: HTML) -> Tag {
        Tag {name_id, attributes, content}
    }
    
    pub fn str_attr(mut self, k: NameID, v: &str) -> Self {
        let v = Some(Rc::from(v));
        if self.attributes.insert(k, v).is_some() {
            ice("duplicate attribute");
        }
        self
    }
}

impl From<Tag> for HTML {
    fn from(tag: Tag) -> HTML {
        HTML::Tag(Rc::new(tag))
    }
}

impl <'a> Compiler<'a> {
    pub fn compile_tag(&mut self, tag: &ast::Tag) -> HTML {
        let tag_name_id = match &tag.name {
            ast::TagName::Fixed(name_id) => *name_id,
            ast::TagName::Variable(var) => match self.evaluate_var(var, &Type::Str) {
                Some(Value::Str(name)) => {
                    if text::is_identifier(&name) {
                        self.loader.string_pool.insert(&name.to_ascii_lowercase())
                    } else {
                        self.diagnostics.error(&format!("invalid tag name '{}'", name), &var.range);
                        str_ids::ANONYMOUS
                    }
                },
                None => str_ids::ANONYMOUS,
                Some(_) => ice_at("failed to coerce", &var.range),
            }
        };
        
        let mut attrs = AttrMap::new();
        for attr in tag.attrs.iter() {
            match attr {
                ast::TagAttrOrSpread::Attr(attr) => {
                    let range = &attr.range;
                    match attr.value.as_ref() {
                        Some(node) => {
                            let expected_type = if attr.question_mark { Type::optional(Type::Str) } else { Type::Str };
                            match self.evaluate_node(node, &expected_type) {
                                Some(Value::Str(s)) => { self.add_attr(&mut attrs, attr.name_id, Some(s), range); },
                                Some(Value::Unit) | None => {},
                                Some(_) => ice_at("failed to coerce", range),
                            }
                        },
                        None => { self.add_attr(&mut attrs, attr.name_id, None, range); },
                    }
                },
                ast::TagAttrOrSpread::Spread(spread) => {
                    let range = spread.range();
                    match self.evaluate_node(spread, &Type::dict(Type::optional(Type::Str))) {
                        Some(Value::Dict(dict)) => {
                            for (k, v) in dict.iter() {
                                match v {
                                    Value::Str(s) => { self.add_attr(&mut attrs, *k, Some(s.clone()), range); },
                                    Value::Unit => { self.add_attr(&mut attrs, *k, None, range); },
                                    _ => ice_at("failed to coerce", range),
                                }
                            }
                        },
                        None => {},
                        Some(_) => ice_at("failed to coerce", range),
                    }
                },
            }
        }
        
        let children = self.compile_sequence(&tag.children, taginfo::content_kind(tag_name_id));
        if tag_name_id.is_anonymous() {
            children
        } else {
            Tag::new_with_attrs(tag_name_id, attrs, children).into()
        }
    }
    
    fn add_attr(&mut self, attrs: &mut AttrMap, name_id: NameID, value: Option<Rc<str>>, range: &SourceRange) {
        if attrs.insert(name_id, value).is_some() {
            self.diagnostics.error(&format!("repeated attribute '{}'", self.get_name(name_id)), range);
        }
    }
}
