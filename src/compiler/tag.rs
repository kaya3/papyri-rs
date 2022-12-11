use std::rc::Rc;
use indexmap::IndexMap;

use crate::errors::{ice, ice_at, NameError, RuntimeError};
use crate::parser::ast;
use crate::utils::{str_ids, NameID, taginfo, text, SourceRange};
use super::base::Compiler;
use super::html::HTML;
use super::types::Type;
use super::value::Value;

pub(super) type AttrMap = IndexMap<NameID, Option<Rc<str>>>;

#[derive(Debug, Clone)]
pub struct Tag {
    pub(super) name_id: NameID,
    pub(super) attributes: AttrMap,
    pub(super) content: HTML,
}

impl Tag {
    pub(super) fn new(name_id: NameID, content: HTML) -> Tag {
        Tag::new_with_attrs(name_id, AttrMap::new(), content)
    }
    
    pub(super) fn new_with_attrs(name_id: NameID, attributes: AttrMap, content: HTML) -> Tag {
        Tag {name_id, attributes, content}
    }
    
    pub(super) fn str_attr(mut self, k: NameID, v: &str) -> Tag {
        let v = Some(Rc::from(v));
        if self.attributes.insert(k, v).is_some() {
            ice("duplicate attribute");
        }
        self
    }
}

impl PartialEq for Tag {
    fn eq(&self, other: &Tag) -> bool {
        self.name_id == other.name_id
            && self.attributes == other.attributes
            && self.content == other.content
    }
}
impl Eq for Tag {}

impl From<Tag> for HTML {
    fn from(tag: Tag) -> HTML {
        HTML::Tag(Rc::new(tag))
    }
}

impl From<Tag> for Value {
    fn from(tag: Tag) -> Value {
        HTML::from(tag).into()
    }
}

impl <'a> Compiler<'a> {
    pub(super) fn compile_tag(&mut self, tag: &ast::Tag) -> HTML {
        let tag_name_id = match tag.name {
            ast::TagName::Literal(name_id) => name_id,
            ast::TagName::Name(ref name) => match self.evaluate_name(name, &Type::Str) {
                Some(Value::Str(name_str)) => {
                    if text::is_identifier(&name_str) {
                        self.string_pool_mut()
                            .insert(&name_str.to_ascii_lowercase())
                    } else if name_str.eq_ignore_ascii_case("!DOCTYPE") {
                        str_ids::_DOCTYPE
                    } else {
                        self.name_error(NameError::InvalidTag(name_str), name.range());
                        str_ids::ANONYMOUS
                    }
                },
                None => str_ids::ANONYMOUS,
                Some(_) => ice_at("failed to coerce", name.range()),
            },
        };
        
        let mut attrs = AttrMap::new();
        for attr in tag.attrs.iter() {
            match attr {
                ast::TagAttrOrSpread::Attr(attr) => {
                    let range = &attr.range;
                    match attr.value.as_ref() {
                        Some(node) => {
                            let expected_type = Type::Str.option_if(attr.question_mark);
                            if let Some(v) = self.evaluate_node(node, &expected_type) {
                                let s = v.to_optional_rc_str(range);
                                if s.is_some() { self.add_attr(&mut attrs, attr.name_id, s, range); }
                            }
                        },
                        None => { self.add_attr(&mut attrs, attr.name_id, None, range); },
                    }
                },
                ast::TagAttrOrSpread::Spread(spread) => {
                    let range = spread.range();
                    match self.evaluate_node(spread, &Type::Str.option().dict()) {
                        Some(Value::Dict(dict)) => {
                            for (&k, v) in dict.iter() {
                                let s = v.to_optional_rc_str(range);
                                self.add_attr(&mut attrs, k, s, range);
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
            let name = self.get_name(name_id).to_string();
            self.runtime_error(RuntimeError::AttrMultipleValues(name), range);
        }
    }
}
