use crate::errors::TypeError;
use crate::parser::ast::AST;
use crate::utils::taginfo::{ContentKind, content_kind};
use crate::utils::{str_ids, NameID};
use super::base::Compiler;
use super::html::HTML;

impl ContentKind {
    fn wrap_with(self) -> NameID {
        match self {
            ContentKind::RequireOneOf(tag_name_ids) => tag_name_ids[0],
            ContentKind::RequireBlock(tag_name_id) | ContentKind::AllowBlock(tag_name_id) => tag_name_id,
            _ => str_ids::P,
        }
    }
}

impl <'a> Compiler<'a> {
    pub fn compile_sequence(&mut self, sequence: &[AST], content_kind: ContentKind) -> HTML {
        let require_inline = matches!(content_kind, ContentKind::RequireInline | ContentKind::RequireInlineNoLineBreaks);
        let forbid_breaks = matches!(content_kind, ContentKind::RequireInlineNoLineBreaks);
        
        let mut comp = SequenceBuilder::new(content_kind);
        for child in sequence {
            if let AST::ParagraphBreak(range) = child {
                if forbid_breaks {
                    self.type_error(TypeError::InlineParagraphBreak, range);
                }
                comp.newline();
            } else {
                let child_html = self.compile_node(child);
                if require_inline {
                    if let Some(block_tag_name) = child_html.block_kind() {
                        let block_tag_name = self.get_name(block_tag_name).to_string();
                        self.type_error(TypeError::TagNotAllowed(block_tag_name), child.range());
                    }
                }
                comp.push(child_html);
            }
        }
        
        let html = comp.into_html();
        if matches!(content_kind, ContentKind::RequireEmpty) && !html.is_empty() {
            let range = &sequence[0].range().to_end(sequence.last().unwrap().range().end);
            self.type_error(TypeError::NoContentAllowed, range);
        }
        html
    }
}

struct SequenceBuilder {
    content_kind: ContentKind,
    children: Vec<HTML>,
    next_child: Option<Box<SequenceBuilder>>,
}

impl SequenceBuilder {
    fn new(block_kind: ContentKind) -> SequenceBuilder {
        SequenceBuilder {
            content_kind: block_kind,
            children: Vec::new(),
            next_child: None,
        }
    }
    
    fn into_html(mut self) -> HTML {
        if matches!(self.content_kind, ContentKind::AllowBlock(..)) && self.children.is_empty() {
            return self.next_child.map_or(HTML::Empty, |c| c.into_html());
        }
        
        self.close_child();
        while matches!(self.children.last(), Some(c) if c.is_whitespace()) {
            self.children.pop();
        }
        HTML::seq(self.children)
    }
    
    fn newline(&mut self) {
        match self.content_kind {
            ContentKind::RequireBlock(..) |
            ContentKind::RequireOneOf(..) |
            ContentKind::AllowBlock(..) => {
                self.close_child();
            },
            ContentKind::RequireInline => {
                self.push(HTML::tag(str_ids::BR, HTML::Empty));
            },
            _ => {},
        }
    }
    
    fn close_child(&mut self) {
        if let Some(child) = std::mem::take(&mut self.next_child) {
            let html = child.into_html();
            if !html.is_empty() {
                self.children.push(HTML::tag(self.content_kind.wrap_with(), html));
            }
        }
    }
    
    fn push(&mut self, child: HTML) {
        // do nothing
        if child.is_empty() { return; }
        
        let is_allowed = match self.content_kind {
            ContentKind::RequireOneOf(tag_names) => child.is_all(tag_names),
            ContentKind::RequireBlock(..) |
            ContentKind::AllowBlock(..) => child.is_block(),
            _ => true,
        };
        if is_allowed {
            self.close_child();
            if !self.children.is_empty() || !child.is_whitespace() {
                self.children.push(child);
            }
        } else if self.next_child.is_some() || !child.is_whitespace() {
            self.next_child.get_or_insert_with(|| {
                let child_content_kind = content_kind(self.content_kind.wrap_with());
                Box::new(SequenceBuilder::new(child_content_kind))
            }).push(child);
        }
    }
}
