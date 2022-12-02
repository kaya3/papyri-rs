use std::rc::Rc;

use crate::parser::{Token, text};
use crate::utils::{str_ids, StringPool, NameID, taginfo};
use super::tag::Tag;
use super::value::Value;

#[derive(Debug, Clone)]
/// Some HTML content, possibly empty. HTML content is classified as either
/// "block", "inline" or "empty".
pub enum HTML {
    /// Represents the absence of any HTML content.
    Empty,
    
    /// An HTML Tag.
    Tag(Rc<Tag>),
    
    /// A sequence of HTML content. The sequence is normalised so that it does
    /// not include `Empty` values or other sequences, and its length is at
    /// least 2.
    Sequence(Rc<[HTML]>),
    
    /// Raw text content. May contain special characters which will need to be
    /// escaped when rendering to HTML.
    Text(Rc<str>),
    
    /// A single space. Sequences of whitespace are collapsed to this.
    Whitespace,
    
    /// A literal newline. This is only useful in pre-formatted content, such
    /// as a `<pre>` tag; otherwise it is equivalent to whitespace.
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
    /// Converts a value to its HTML representation. Lists become `<ul>` tags,
    /// dictionaries become tables, and functions will be represented as
    /// `<code>(@fn name)</code>`.
    pub fn from_value(value: Value, string_pool: &StringPool) -> HTML {
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
    
    /// Converts a string to an HTML item. Empty strings become `HTML::Empty`,
    /// but whitespace does not become `HTML::Whitespace`.
    pub fn text(s: &str) -> HTML {
        if s.is_empty() {
            HTML::Empty
        } else {
            HTML::Text(Rc::from(s))
        }
    }
    
    /// Creates an HTML tag with no attributes. Use `Tag::new` for more complex
    /// use-cases.
    pub fn tag(name_id: NameID, content: HTML) -> HTML {
        Tag::new(name_id, content).into()
    }
    
    /// Converts a sequence of HTML items into one normalised HTML item. Empty
    /// items are dropped, and nested sequences are flattened, and if there are
    /// fewer than two items then a single item (or `HTML::Empty`) is returned
    /// instead of an `HTML::Sequence`.
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
    
    /// Indicates whether this HTML item is `HTML::Empty`. Whitespace and
    /// literal newlines are not considered to be empty.
    pub fn is_empty(&self) -> bool {
        matches!(self, HTML::Empty)
    }
    
    /// Indicates whether this HTML item is block-level content, or otherwise
    /// does not need to be wrapped in a block-level element. Empty content is
    /// not considered to be block-level.
    pub fn is_block(&self) -> bool {
        match self {
            HTML::Tag(tag) => taginfo::is_block(tag.name_id),
            HTML::Sequence(seq) => seq[0].is_block(),
            _ => false,
        }
    }
    
    /// Indicates whether this HTML item is inline content which may need to be
    /// wrapped in a block-level element. Empty content is not considered to be
    /// inline, since it doesn't need to be wrapped.
    pub fn is_inline(&self) -> bool {
        match self {
            HTML::Tag(tag) => !taginfo::is_block(tag.name_id),
            HTML::Sequence(seq) => seq[0].is_inline(),
            HTML::Text(_) |
            HTML::Whitespace |
            HTML::RawNewline => true,
            _ => false,
        }
    }
    
    /// Indicates whether this HTML item is whitespace, including a literal
    /// newline or `HTML::Empty`.
    pub fn is_whitespace(&self) -> bool {
        matches!(self, HTML::Whitespace | HTML::RawNewline | HTML::Empty)
    }
    
    /// Indicates whether this HTML item matches a given set of allowed tag
    /// names. This is used to ensure e.g. that a `<ul>` tag only directly
    /// contains `<li>` tags.
    pub fn is_all(&self, allowed_tag_names: &[NameID]) -> bool {
        match self {
            HTML::Tag(tag) => allowed_tag_names.contains(&tag.name_id),
            HTML::Sequence(seq) => seq.iter().all(|child| child.is_all(allowed_tag_names)),
            HTML::Empty => true,
            _ => false,
        }
    }
}
