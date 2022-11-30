use std::rc::Rc;

use crate::parser::{Token, text};
use crate::utils::{str_ids, StringPool, NameID, taginfo};
use super::tag::Tag;
use super::value::Value;

#[derive(Debug, Clone)]
pub enum HTML {
    Empty,
    Tag(Rc<Tag>),
    Sequence(Rc<[HTML]>),
    Text(Rc<str>),
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
            Value::Bool(b) => Token::bool_to_string(b).into(),
            Value::Int(t) => text::substitutions(&t.to_string()).into(),
            Value::Str(t) => HTML::Text(t),
            Value::Dict(vs) => {
                let rows: Vec<HTML> = vs.iter()
                    .map(|(&k, v)| HTML::tag(str_ids::TR, HTML::seq([
                        HTML::tag(str_ids::TH, HTML::text(string_pool.get(k))),
                        HTML::tag(str_ids::TD, HTML::from_value(v.clone(), string_pool)),
                    ])))
                    .collect();
                
                Tag::new(str_ids::TABLE, HTML::seq(rows))
                    .str_attr(str_ids::CLASS, "tabular-data")
                    .into()
            },
            Value::List(vs) => {
                let items: Vec<_> = vs.as_ref()
                    .iter()
                    .map(|child| HTML::tag(str_ids::LI, HTML::from_value(child.clone(), string_pool)))
                    .collect();
                HTML::tag(str_ids::UL, HTML::seq(items))
            },
            Value::HTML(html) => html,
            
            Value::Func(f) => {
                let name = string_pool.get(f.name_id());
                HTML::tag(str_ids::CODE, format!("(@fn {name})").into())
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
    
    pub fn seq<T: IntoIterator<Item=HTML>>(content: T) -> HTML {
        let mut seq = Vec::new();
        for child in content.into_iter() {
            match child {
                HTML::Empty => {},
                HTML::Sequence(grandchildren) => seq.extend(grandchildren.iter().cloned()),
                _ => seq.push(child.clone()),
            }
        }
        
        if seq.len() >= 2 {
            HTML::Sequence(Rc::from(seq))
        } else {
            seq.pop().unwrap_or(HTML::Empty)
        }
    }
    
    pub fn is_empty(&self) -> bool {
        matches!(self, HTML::Empty)
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
}
