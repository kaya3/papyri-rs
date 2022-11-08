use std::collections::{HashMap, HashSet};

use crate::parser::{ast, AST};
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
    pub attributes: HashMap<NameID, Option<Box<str>>>,
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
    
    pub fn attr(&mut self, k: NameID, v: Option<Box<str>>) {
        if k == str_ids::CLASS {
            if let Some(v) = v { self.add_css_class(&v); }
        } else if k == str_ids::STYLE {
            if let Some(v) = v { self.add_style(&v); }
        } else {
            self.attributes.insert(k, v);
        }
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
            match &attr.value {
                Some(node) => {
                    let v = self.evaluate_tag_attribute(node);
                    if v.is_some() {
                        r.attr(attr.name_id, v);
                    } else if !attr.question_mark {
                        self.diagnostics.err_expected_type(&Type::Str, &Type::Unit, node.range());
                    }
                },
                None => {
                    r.attr(attr.name_id, None);
                }
            }
        }
        r.content = self.compile_tag_children(tag);
        HTML::from(r)
    }
    
    fn compile_tag_children(&mut self, tag: &ast::Tag) -> HTML {
        self.compile_sequence(&tag.children, taginfo::content_kind(tag.name_id))
    }
    
    fn evaluate_tag_attribute(&mut self, value: &AST) -> Option<Box<str>> {
        let mut out = "".to_string();
        self.write_tag_attribute(value, &mut out)
            .then_some(Box::from(out))
    }
    
    fn write_tag_attribute(&mut self, value: &AST, out: &mut String) -> bool {
        match value {
            AST::FuncCall(_) | AST::VarName(_, _) => {
                match self.evaluate_node(value, &Type::optional(Type::Str)) {
                    Some(Value::Str(t)) => { *out += &t; }
                    Some(Value::Unit) => return false,
                    None => {},
                    _ => ice_at("coercion failed", value.range()),
                }
            },
            
            AST::Group(group) => {
                for child in group.children.iter() {
                    self.write_tag_attribute(child, out);
                }
            },
            AST::List(list) => {
                let mut sep = false;
                for child in list.children.iter() {
                    if sep { *out += " "; }
                    self.write_tag_attribute(child, out);
                    sep = true;
                }
            },
            AST::Text(t, _) => {
                *out += &t;
            }
            AST::Whitespace(_) => {
                *out += " ";
            },
            AST::Entity(tok) => {
                *out += &self.decode_entity(tok);
            },
            AST::Escape(tok) => {
                *out += self.unescape_char(&tok).encode_utf8(&mut [0; 4]);
            },
            AST::Verbatim(tok) => {
                *out += tok.get_verbatim_text();
            },
            
            _ => {
                self.diagnostics.syntax_error("not allowed in tag attribute", value.range());
            },
        }
        true
    }
}
