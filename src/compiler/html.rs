use std::rc::Rc;

use crate::errors;
use crate::utils::{NameID, taginfo, text, str_ids};
use super::tag::Tag;
use super::value::RcStr;

#[derive(Debug, Clone)]
/// Some HTML content, possibly empty. HTML content is classified as either
/// "block", "inline" or "empty".
/// 
/// HTML values are normalised so sequences are not empty or a single item, and
/// they do not contain nested sequences. Additionally, text is not empty, or a
/// single space or newline character. This is necessary in order to ensure
/// consistent matching of HTML sequences, and to test equality of HTML values.
pub enum HTML {
    /// Represents the absence of any HTML content.
    Empty,
    
    /// An HTML tag.
    Tag(Rc<Tag>),
    
    /// A sequence of HTML content. The sequence is normalised so that it does
    /// not include `Empty` values, consecutive text nodes or nested sequences,
    /// and its length is at least 2.
    Sequence(Rc<[HTML]>),
    
    /// Raw text content. May contain special characters which will need to be
    /// escaped when rendering to HTML.
    Text(RcStr),
    
    /// A single space. Sequences of whitespace are collapsed to this.
    Whitespace,
    
    /// A literal newline. This is only useful in pre-formatted content, such
    /// as a `<pre>` tag; otherwise it is equivalent to whitespace.
    RawNewline,
}

impl <T: AsRef<str> + Into<RcStr>> From<T> for HTML {
    fn from(s: T) -> HTML {
        HTML::text(s)
    }
}

impl FromIterator<HTML> for HTML {
    /// Converts a sequence of HTML items into one normalised HTML item. Empty
    /// items are dropped, consecutive text nodes are merged, and nested
    /// sequences are flattened. The result is then wrapped in an `HTML::Sequence`
    /// only if there are at least two items.
    fn from_iter<T: IntoIterator<Item=HTML>>(iter: T) -> HTML {
        let mut builder = HTMLSeqBuilder::new();
        for child in iter {
            builder.push(child);
        }
        builder.build()
    }
}

impl HTML {
    /// Converts a string to a normalised HTML item. Empty strings become
    /// `HTML::Empty`, single spaces become `HTML::Whitespace`, and newlines
    /// become `HTML::RawNewline`.
    pub(super) fn text<T: AsRef<str> + Into<RcStr>>(s: T) -> HTML {
        match s.as_ref() {
            "" => HTML::Empty,
            " " => HTML::Whitespace,
            "\n" => HTML::RawNewline,
            _ => HTML::Text(s.into()),
        }
    }
    
    /// Creates an HTML tag with no attributes. Use `Tag::new` for more complex
    /// use-cases.
    pub(super) fn tag(name_id: NameID, content: HTML) -> HTML {
        Tag::new(name_id, content).into()
    }
    
    /// Wraps this HTML content in a tag.
    pub(super) fn in_tag(self, name_id: NameID) -> HTML {
        HTML::tag(name_id, self)
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
        self.block_kind().is_some()
    }
    
    /// Returns the name of the first block tag in this HTML content, if there
    /// is one.
    pub(super) fn block_kind(&self) -> Option<NameID> {
        match self {
            HTML::Tag(tag) => if tag.name_id == str_ids::A {
                tag.content.block_kind()
            } else {
                taginfo::is_block(tag.name_id).then_some(tag.name_id)
            },
            HTML::Sequence(seq) => seq.iter().find_map(HTML::block_kind),
            _ => None,
        }
    }
    
    /// Indicates whether this HTML item is inline content which may need to be
    /// wrapped in a block-level element. Empty content is not considered to be
    /// inline, since it doesn't need to be wrapped.
    pub fn is_inline(&self) -> bool {
        match self {
            HTML::Text(_) |
            HTML::Whitespace |
            HTML::RawNewline => true,
            
            HTML::Tag(tag) => !taginfo::is_block(tag.name_id),
            HTML::Sequence(seq) => seq.iter().all(HTML::is_inline),
            
            HTML::Empty => false,
        }
    }
    
    /// Indicates whether this HTML item is whitespace, including a literal
    /// newline or `HTML::Empty`.
    pub fn is_whitespace(&self) -> bool {
        match self {
            HTML::Empty |
            HTML::Whitespace |
            HTML::RawNewline => true,
            HTML::Text(t) => text::is_whitespace(t),
            // a sequence cannot be pure whitespace, since it would be
            // collapsed to a single text node.
            _ => false,
        }
    }
    
    /// Indicates whether this HTML item matches a given set of allowed tag
    /// names. This is used to ensure e.g. that a `<ul>` tag only directly
    /// contains `<li>` tags.
    pub(super) fn is_all(&self, allowed_tag_names: &[NameID]) -> bool {
        match self {
            HTML::Tag(tag) => allowed_tag_names.contains(&tag.name_id),
            HTML::Sequence(seq) => seq.iter().all(|child| child.is_all(allowed_tag_names)),
            HTML::Empty => true,
            _ => false,
        }
    }
    
    pub(super) fn nodes(&self) -> &[HTML] {
        match self {
            HTML::Empty => &[],
            HTML::Sequence(seq) => seq.as_ref(),
            _ => std::slice::from_ref(self),
        }
    }
}

impl PartialEq for HTML {
    /// Determines whether two HTML values are equal. Sequences and tags are
    /// compared deeply.
    fn eq(&self, other: &HTML) -> bool {
        match (self, other) {
            (HTML::Empty, HTML::Empty) |
            (HTML::Whitespace, HTML::Whitespace) |
            (HTML::RawNewline, HTML::RawNewline) => true,
            
            (HTML::Tag(t1), HTML::Tag(t2)) => t1 == t2,
            (HTML::Text(t1), HTML::Text(t2)) => t1 == t2,
            
            (HTML::Sequence(s1), HTML::Sequence(s2)) => s1 == s2,
            
            _ => false,
        }
    }
}
impl Eq for HTML {}

struct HTMLSeqBuilder {
    children: Vec<HTML>,
    current_text_node: Vec<HTML>,
}

impl HTMLSeqBuilder {
    fn new() -> HTMLSeqBuilder {
        HTMLSeqBuilder {
            children: Vec::new(),
            current_text_node: Vec::new(),
        }
    }
    
    fn push(&mut self, child: HTML) {
        match child {
            HTML::Empty => {},
            HTML::Sequence(grandchildren) => {
                for grandchild in grandchildren.iter().cloned() {
                    self.push(grandchild);
                }
            },
            HTML::Tag(_) => {
                self.close_text_node();
                self.children.push(child);
            },
            _ => {
                self.current_text_node.push(child);
            },
        }
    }
    
    fn close_text_node(&mut self) {
        let mut nodes = std::mem::take(&mut self.current_text_node);
        if nodes.len() >= 2 {
            let mut s = "".to_string();
            for node in nodes.into_iter() {
                match node {
                    HTML::Text(t) => s += t.as_ref(),
                    HTML::Whitespace => s += " ",
                    HTML::RawNewline => s += "\n",
                    _ => errors::ice("Not a text node"),
                }
            }
            self.children.push(s.into());
        } else if let Some(node) = nodes.pop() {
            self.children.push(node);
        }
    }
    
    fn build(mut self) -> HTML {
        self.close_text_node();
        if self.children.len() >= 2 {
            HTML::Sequence(Rc::from(self.children))
        } else {
            self.children.pop().unwrap_or(HTML::Empty)
        }
    }
}
