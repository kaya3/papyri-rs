use std::rc::Rc;

use crate::parser::Token;
use crate::utils::{str_ids, StringPool, NameID, taginfo};
use super::tag::Tag;
use super::value::Value;

#[derive(Debug, Clone)]
pub enum HTML {
    Empty,
    Tag(Rc<Tag>),
    Sequence(Rc<[HTML]>),
    Text(Rc<str>),
    DocType,
    Whitespace,
    RawNewline,
}

impl From<&str> for HTML {
    fn from(s: &str) -> Self {
        HTML::text(s)
    }
}

impl From<String> for HTML {
    fn from(s: String) -> Self {
        s.as_str().into()
    }
}

impl HTML {
    pub fn from_value(value: Value, string_pool: &mut StringPool) -> HTML {
        match value {
            Value::Unit => HTML::Empty,
            Value::Bool(b) => Token::bool_to_string(b).into(),
            Value::Int(t) => t.to_string().replace("-", "\u{2212}").into(),
            Value::Str(t) => HTML::Text(t),
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
                
                Tag::new(str_ids::TABLE, HTML::seq(&rows))
                    .str_attr(str_ids::CLASS, "tabular-data")
                    .into()
            },
            Value::List(vs) => {
                let items: Vec<_> = vs.as_ref()
                    .iter()
                    .map(|child| HTML::tag(str_ids::LI, HTML::from_value(child.clone(), string_pool)))
                    .collect();
                HTML::tag(str_ids::UL, HTML::seq(&items))
            },
            Value::HTML(html) => html,
            
            Value::Func(f) => {
                let name = string_pool.get(f.name_id());
                HTML::tag(str_ids::CODE, format!("(@fn {})", name).into())
            },
        }
    }
    
    pub fn text(s: &str) -> HTML {
        if s.is_empty() {
            HTML::Empty
        } else {
            HTML::Text(Rc::from(s))
        }
    }
    
    pub fn tag(name_id: NameID, content: HTML) -> HTML {
        Tag::new(name_id, content).into()
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
    
    pub fn is_whitespace(&self) -> bool {
        matches!(self, HTML::Whitespace | HTML::RawNewline | HTML::Empty)
    }
    
    pub fn is_block(&self) -> bool {
        match self {
            HTML::Tag(tag) => taginfo::is_block(tag.name_id),
            HTML::Sequence(seq) => seq[0].is_block(),
            _ => false,
        }
    }
    
    pub fn is_all(&self, allowed_tag_names: &[NameID]) -> bool {
        match self {
            HTML::Tag(tag) => allowed_tag_names.contains(&tag.name_id),
            HTML::Sequence(seq) => seq.iter().all(|child| child.is_all(allowed_tag_names)),
            HTML::Empty => true,
            _ => false,
        }
    }
    
    pub fn wrap_page(self, title: Option<HTML>, web_root: &str) -> HTML {
        let link_tag = Tag::new(str_ids::LINK, HTML::Empty)
            .str_attr(str_ids::REL, "stylesheet")
            .str_attr(str_ids::TYPE, "text/css")
            .str_attr(str_ids::HREF, &format!("{}papyri.css", web_root));
        
        let script_tag = Tag::new(str_ids::SCRIPT, HTML::Empty)
            .str_attr(str_ids::TYPE, "text/javascript")
            .str_attr(str_ids::SRC, &format!("{}papyri.js", web_root));
        
        HTML::seq(&[
            HTML::DocType,
            HTML::tag(str_ids::HTML, HTML::seq(&[
                HTML::tag(str_ids::HEAD, HTML::seq(&[
                    title.map_or(HTML::Empty, |t| HTML::tag(str_ids::TITLE, t)),
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
