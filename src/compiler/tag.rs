use std::rc::Rc;
use indexmap::IndexMap;

use crate::errors;
use crate::parser::{ast, Type};
use crate::utils::{str_ids, NameID, taginfo, text};
use crate::utils::sourcefile::SourceRange;
use super::base::Compiler;
use super::html::HTML;
use super::value::{Value, RcStr};
use super::value_convert::TryConvert;

pub(super) type AttrMap = IndexMap<NameID, Option<RcStr>, fxhash::FxBuildHasher>;

#[derive(Debug, Clone)]
pub struct Tag {
    pub(super) name_id: NameID,
    pub(super) attributes: AttrMap,
    pub(super) content: HTML,
}

impl Tag {
    pub(super) fn new(name_id: NameID, content: HTML) -> Tag {
        Tag::new_with_attrs(name_id, AttrMap::default(), content)
    }
    
    pub(super) fn new_with_attrs(name_id: NameID, attributes: AttrMap, content: HTML) -> Tag {
        Tag {name_id, attributes, content}
    }
    
    pub(super) fn str_attr(mut self, k: NameID, v: &str) -> Tag {
        let v = Some(Rc::from(v));
        if self.attributes.insert(k, v).is_some() {
            errors::ice("Duplicate attribute");
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
    pub(super) fn compile_tag(&mut self, tag: &ast::Tag) -> errors::Reported<Tag> {
        let tag_name_id = self.compile_tag_name(&tag.name)?;
        
        let mut attrs = AttrMap::default();
        for attr in tag.attrs.iter() {
            match attr {
                ast::TagAttrOrSpread::Attr(attr) => {
                    let range = attr.range;
                    match attr.value.as_ref() {
                        Some(node) => {
                            let expected_type = Type::Str.option_if(attr.question_mark);
                            let s: Option<RcStr> = self.evaluate_node(node, &expected_type)?
                                .expect_convert();
                            if s.is_some() { self.add_attr(&mut attrs, attr.name_id, s, range)?; }
                        },
                        None => { self.add_attr(&mut attrs, attr.name_id, None, range)?; },
                    }
                },
                ast::TagAttrOrSpread::Spread(spread) => {
                    let dict: AttrMap = self.evaluate_node(spread, &AttrMap::as_type())?
                        .expect_convert();
                    let range = spread.range();
                    for (k, s) in dict.into_iter() {
                        self.add_attr(&mut attrs, k, s, range)?;
                    }
                },
            }
        }
        
        let children = self.compile_sequence(&tag.children, taginfo::ContentKind::for_(tag_name_id));
        Ok(Tag::new_with_attrs(tag_name_id, attrs, children))
    }
    
    fn compile_tag_name(&mut self, name: &ast::TagName) -> errors::Reported<NameID> {
        match name {
            ast::TagName::Literal(name_id) => Ok(*name_id),
            ast::TagName::Name(name) => {
                let name_str: RcStr = self.evaluate_name(name, &Type::Str)?
                    .expect_convert();
                if text::is_identifier(&name_str) {
                    Ok(self.string_pool_mut()
                        .insert_lowercase(name_str))
                } else if name_str.eq_ignore_ascii_case("!DOCTYPE") {
                    Ok(str_ids::_DOCTYPE)
                } else {
                    let e = errors::NameError::InvalidTag(name_str);
                    Err(self.report(e, name.range()))
                }
            },
        }
    }
    
    fn add_attr(&mut self, attrs: &mut AttrMap, name_id: NameID, value: Option<RcStr>, range: SourceRange) -> errors::Reported {
        if attrs.insert(name_id, value).is_some() {
            let name = self.get_name(name_id);
            return Err(self.report(errors::RuntimeError::AttrMultipleValues(name), range));
        }
        Ok(())
    }
}
