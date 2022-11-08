use std::rc::Rc;

use crate::parser::Token;
use crate::utils::{str_ids, StringPool, NameID, taginfo};
use super::tag::{Tag, PlainTag};
use super::value::Value;

#[derive(Debug, Clone)]
pub enum HTML {
    Tag(Rc<Tag>),
    PlainTag(Rc<PlainTag>),
    Sequence(Rc<[HTML]>),
    Text(Rc<str>),
    DocType,
    Whitespace,
    RawNewline,
    Empty,
}

impl From<PlainTag> for HTML {
    fn from(tag: PlainTag) -> HTML {
        HTML::PlainTag(Rc::new(tag))
    }
}

impl From<Tag> for HTML {
    fn from(tag: Tag) -> HTML {
        HTML::Tag(Rc::new(tag))
    }
}

impl HTML {
    pub fn from_value(value: Value, string_pool: &mut StringPool) -> HTML {
        match value {
            Value::Unit => HTML::Empty,
            Value::Bool(b) => HTML::text(Token::bool_to_string(b)),
            Value::Int(t) => HTML::text(&t.to_string()),
            Value::Str(t) => HTML::Text(t.clone()),
            Value::Dict(vs) => {
                let mut entries: Vec<_> = vs.iter()
                    .map(|(k, v)| {
                        let k_str: Rc<str> = Rc::from(string_pool.get(*k));
                        (k_str, v)
                    })
                    .collect();
                entries.sort_by_key(|t| t.0.clone());
                
                let rows: Vec<_> = entries.into_iter()
                    .map(|(k, v)| HTML::tag(str_ids::TR, HTML::seq(&[
                        HTML::tag(str_ids::TH, HTML::Text(k)),
                        HTML::tag(str_ids::TD, HTML::from_value(v.clone(), string_pool)),
                    ])))
                    .collect();
                let mut tbl = Tag::new(str_ids::TABLE, HTML::seq(&rows));
                tbl.add_css_class("tabular-data");
                tbl.into()
            },
            Value::List(vs) => {
                let items: Vec<_> = vs.iter()
                    .map(|child| HTML::tag(str_ids::LI, HTML::from_value(child.clone(), string_pool)))
                    .collect();
                HTML::tag(str_ids::UL, HTML::seq(&items))
            },
            Value::HTML(html) => html,
            
            Value::Func(f) => {
                let name = string_pool.get(f.name_id());
                HTML::tag(str_ids::CODE, HTML::text(&format!("(@fn {})", name)))
            },
            Value::NativeFunc(f) => {
                let name = string_pool.get(f.name_id());
                HTML::tag(str_ids::CODE, HTML::text(&format!("(native @fn {})", name)))
            },
        }
    }
}

impl HTML {
    pub fn text(s: &str) -> HTML {
        if s.is_empty() {
            HTML::Empty
        } else {
            HTML::Text(Rc::from(s))
        }
    }
    
    pub fn tag(name_id: NameID, content: HTML) -> HTML {
        PlainTag::new(name_id, content).into()
    }
    
    pub fn seq(content: &[HTML]) -> HTML {
        if content.is_empty() {
            HTML::Empty
        } else if content.len() == 1 {
            content[0].clone()
        } else {
            HTML::Sequence(Rc::from(content))
        }
    }
    
    pub fn is_block(&self) -> bool {
        match self {
            HTML::Tag(tag) => taginfo::is_block(tag.name_id),
            HTML::PlainTag(tag) => taginfo::is_block(tag.name_id),
            HTML::Sequence(seq) => seq[0].is_block(),
            _ => false,
        }
    }
    
    pub fn is_all(&self, allowed_tag_names: &[NameID]) -> bool {
        match self {
            HTML::Tag(tag) => allowed_tag_names.contains(&tag.name_id),
            HTML::PlainTag(tag) => allowed_tag_names.contains(&tag.name_id),
            HTML::Sequence(seq) => seq.iter().all(|child| child.is_all(allowed_tag_names)),
            HTML::Empty => true,
            _ => false,
        }
    }
    
    pub fn wrap_page(self, title: Option<HTML>, web_root: &str) -> HTML {
        let mut link_tag = Tag::new(str_ids::LINK, HTML::Empty);
        link_tag.attr(str_ids::REL, Some(Box::from("stylesheet")));
        link_tag.attr(str_ids::TYPE, Some(Box::from("text/css")));
        link_tag.attr(str_ids::HREF, Some(format!("{}papyri.css", web_root).into_boxed_str()));
        
        let mut script_tag = Tag::new(str_ids::SCRIPT, HTML::Empty);
        script_tag.attr(str_ids::TYPE, Some(Box::from("text/javascript")));
        script_tag.attr(str_ids::SRC, Some(format!("{}papyri.js", web_root).into_boxed_str()));
        
        HTML::seq(&[
            HTML::DocType,
            HTML::tag(str_ids::HTML, HTML::seq(&[
                HTML::tag(str_ids::HEAD, HTML::seq(&[
                    title.map(|t| HTML::tag(str_ids::TITLE, t))
                        .unwrap_or(HTML::Empty),
                    HTML::from(link_tag),
                ])),
                HTML::tag(str_ids::BODY, HTML::seq(&[
                    HTML::tag(str_ids::ARTICLE, self),
                    HTML::from(script_tag),
                ])),
            ]))
        ])
    }
}
