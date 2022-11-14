use std::collections::{HashMap, HashSet};
use std::rc::Rc;

use crate::parser::ast;
use crate::utils::{ice_at, str_ids, NameID, taginfo};
use super::compiler::Compiler;
use super::html::HTML;
use super::types::Type;
use super::value::Value;

#[derive(Debug, Clone)]
pub struct PlainTag {
    pub name_id: NameID,
    pub content: HTML,
}

impl PlainTag {
    pub fn new(name_id: NameID, content: HTML) -> PlainTag {
        PlainTag {name_id, content}
    }
}

#[derive(Debug, Clone)]
pub struct Tag {
    pub name_id: NameID,
    pub attributes: HashMap<NameID, Option<Rc<str>>>,
    pub css_classes: HashSet<Box<str>>,
    pub css_style: Option<String>,
    pub content: HTML,
}

impl Tag {
    pub fn new(name_id: NameID, content: HTML) -> Tag {
        Tag {
            name_id,
            attributes: HashMap::new(),
            css_classes: HashSet::new(),
            css_style: None,
            content,
        }
    }
    
    pub fn attr(&mut self, k: NameID, v: Option<Rc<str>>) {
        self.attributes.insert(k, v);
    }
    
    pub fn add_css_class(&mut self, k: &str) {
        let ks = k.trim().split(r"\s+").map(Box::from);
        for cls in ks { self.css_classes.insert(cls); }
    }
    
    pub fn add_style(&mut self, s: &str) {
        match &mut self.css_style {
            Some(style) => {
                if !style.ends_with(";") {
                    *style += ";";
                }
                *style += s;
            },
            None => {
                self.css_style = Some(s.to_string());
            },
        };
    }
}

impl <'a> Compiler<'a> {
    pub fn compile_tag(&mut self, tag: &ast::Tag) -> HTML {
        if tag.attrs.is_empty() {
            let content = self.compile_tag_children(tag);
            return HTML::from(PlainTag::new(tag.name_id, content));
        }
        
        let mut r = Tag::new(tag.name_id, HTML::Empty);
        for attr in tag.attrs.iter() {
            match attr.value.as_ref() {
                Some(node) => {
                    let Some(v) = self.evaluate_node(node, &Type::optional(Type::Str)) else { continue; };
                    let s = match v {
                        Value::Str(s) => Some(s),
                        Value::Unit => None,
                        _ => ice_at("failed to coerce", &attr.range),
                    };
                    if s.is_some() || attr.question_mark {
                        match attr.name_id {
                            str_ids::CLASS => if let Some(s) = s { r.add_css_class(&s); },
                            str_ids::STYLE => if let Some(s) = s { r.add_style(&s); },
                            name_id => r.attr(name_id, s),
                        }
                    } else {
                        self.diagnostics.err_expected_type(&Type::Str, &Type::Unit, node.range());
                    }
                },
                None => match attr.name_id {
                    str_ids::CLASS | str_ids::STYLE => {
                        self.diagnostics.err_expected_type(&Type::Str, &Type::Unit, &attr.range);
                    },
                    name_id => r.attr(name_id, None),
                },
            }
        }
        r.content = self.compile_tag_children(tag);
        HTML::from(r)
    }
    
    fn compile_tag_children(&mut self, tag: &ast::Tag) -> HTML {
        self.compile_sequence(&tag.children, taginfo::content_kind(tag.name_id))
    }
}

impl From<PlainTag> for HTML {
    fn from(tag: PlainTag) -> HTML {
        HTML::PlainTag(Rc::new(tag))
    }
}

impl From<Tag> for HTML {
    fn from(tag: Tag) -> HTML {
        if tag.attributes.is_empty() && tag.css_classes.is_empty() && tag.css_style.is_none() {
            HTML::from(PlainTag {name_id: tag.name_id, content: tag.content})
        } else {
            HTML::Tag(Rc::new(tag))
        }
    }
}
