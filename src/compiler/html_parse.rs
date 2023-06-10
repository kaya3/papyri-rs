use html5ever::parse_document;
use html5ever::tendril::TendrilSink;
use markup5ever_rcdom::{Node, NodeData, RcDom};

use crate::errors;
use crate::utils::StringPool;
use super::tag::{Tag, AttrMap};
use super::html::HTML;

impl HTML {
    /// Parses the given string as HTML; may break on insane input.
    pub(super) fn parse(text: &str, pool: &mut StringPool) -> errors::PapyriResult<HTML> {
        let dom = parse_document(RcDom::default(), Default::default())
            .from_utf8()
            .read_from(&mut text.as_bytes())
            .unwrap_or_else(|e| errors::ice(&format!("IOError while parsing HTML: {}", e)));
        
        let parse_error = dom.errors
            .into_iter()
            // Ignore duplicate attribute errors, since apparently a lot of sites have them.
            .find(|e| e != "Duplicate attribute");
        
        match parse_error {
            None => Ok(node_to_html(&dom.document, pool)),
            Some(e) => {
                let err = errors::RuntimeError::ParseHtmlError(e.into());
                Err(err.into())
            },
        }
    }
}

fn node_to_html(node: &Node, pool: &mut StringPool) -> HTML {
    match &node.data {
        NodeData::Comment {..} => HTML::Empty,
        
        NodeData::Document {..} => node.children.borrow()
            .iter()
            .map(|node| node_to_html(node, pool))
            .collect(),
        
        // TODO: represent doctypes properly
        NodeData::Doctype {..} => HTML::Empty,
        
        NodeData::Element {name, attrs, ..} => {
            let name_id = pool.insert_lowercase(name.local.as_ref());
            let attr_map: AttrMap = attrs.borrow()
                .iter()
                .map(|attr| (
                    pool.insert_lowercase(attr.name.local.as_ref()),
                    // TODO: correctly represent boolean attributes as None
                    Some(attr.value.as_ref().into()),
                ))
                .collect();
            let children: HTML = node.children.borrow()
                .iter()
                .map(|node| node_to_html(node, pool))
                .collect();
            
            HTML::Tag(Tag::new_with_attrs(
                name_id,
                attr_map,
                children,
            ).into())
        },
        
        // TODO: normalise whitespace?
        NodeData::Text {contents} => contents.borrow()
            .to_string()
            .into(),
        
        other => errors::ice(&format!("Unexpected HTML node data: {:#?}", other)),
    }
}
