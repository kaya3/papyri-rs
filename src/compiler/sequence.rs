use crate::errors::TypeError;
use crate::parser::ast::AST;
use crate::utils::taginfo::{ContentKind, content_kind};
use crate::utils::{str_ids, NameID};
use super::compiler::Compiler;
use super::html::HTML;

impl ContentKind {
    pub const REQUIRE_P: ContentKind = ContentKind::RequireBlock(str_ids::P);
    pub const ALLOW_P: ContentKind = ContentKind::AllowBlock(str_ids::P);
    
    fn wrap_with(self) -> NameID {
        match self {
            ContentKind::RequireOneOf(tag_name_ids) => tag_name_ids[0],
            ContentKind::RequireBlock(tag_name_id) | ContentKind::AllowBlock(tag_name_id) => tag_name_id,
            _ => str_ids::P,
        }
    }
}

impl <'a> Compiler<'a> {
    pub fn compile_sequence(&mut self, sequence: &[AST], block_kind: ContentKind) -> HTML {
        let allow_blocks = matches!(block_kind, ContentKind::RequireOneOf(..) | ContentKind::RequireBlock(..) | ContentKind::AllowBlock(..));
        let forbid_breaks = matches!(block_kind, ContentKind::RequireInlineNoLineBreaks);
        
        let mut comp = SequenceBuilder::new(block_kind);
        for child in sequence {
            if let AST::ParagraphBreak(range) = child {
                if forbid_breaks {
                    self.diagnostics.type_error(TypeError::InlineParagraphBreak, range);
                }
                comp.newline();
            } else {
                let child_html = self.compile_node(child);
                if child_html.is_block() && !allow_blocks {
                    self.diagnostics.type_error(TypeError::InlineBlockContent, child.range());
                }
                comp.push(child_html);
            }
        }
        
        comp.to_html()
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
    
    fn to_html(mut self) -> HTML {
        if matches!(self.content_kind, ContentKind::AllowBlock(..)) && self.children.is_empty() {
            return self.next_child.map_or(HTML::Empty, |c| c.to_html());
        }
        
        self.close_child();
        while matches!(self.children.last(), Some(c) if c.is_whitespace()) {
            self.children.pop();
        }
        HTML::seq(&self.children)
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
            ContentKind::RequireInlineNoLineBreaks => {},
        }
    }
    
    fn close_child(&mut self) {
        if let Some(child) = std::mem::take(&mut self.next_child) {
            let html = child.to_html();
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
