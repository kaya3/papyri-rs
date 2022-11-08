use crate::parser::ast::AST;
use crate::utils::taginfo::ContentKind;
use crate::utils::{str_ids, NameID};
use super::compiler::Compiler;
use super::html::HTML;

impl ContentKind {
    pub const REQUIRE_P: ContentKind = ContentKind::RequireBlock(str_ids::P);
    pub const ALLOW_P: ContentKind = ContentKind::AllowBlock(str_ids::P);
    
    fn wrap_with(&self) -> NameID {
        match self {
            ContentKind::RequireOneOf(tag_name_ids) => tag_name_ids[0],
            ContentKind::RequireBlock(tag_name_id) | ContentKind::AllowBlock(tag_name_id) => *tag_name_id,
            _ => str_ids::P,
        }
    }
}

impl <'a> Compiler<'a> {
    pub fn compile_sequence(&mut self, sequence: &[AST], block_kind: ContentKind) -> HTML {
        let allow_blocks = matches!(block_kind, ContentKind::RequireOneOf(_) | ContentKind::RequireBlock(_) | ContentKind::AllowBlock(_));
        let forbid_breaks = matches!(block_kind, ContentKind::RequireInlineNoLineBreaks);
        
        let mut comp = SequenceBuilder::new(block_kind);
        for child in sequence {
            if let AST::ParagraphBreak(range) = child {
                if forbid_breaks {
                    self.diagnostics.error("paragraph break not allowed here", range);
                }
                comp.newline();
            } else {
                let child_html = self.compile_node(child);
                if child_html.is_block() && !allow_blocks {
                    self.diagnostics.warning("block content not allowed here", child.range());
                }
                comp.push(child_html);
            }
        }
        
        comp.to_html()
    }
}

struct SequenceBuilder {
    content_kind: ContentKind,
    blocks: Vec<HTML>,
    inline: Vec<HTML>,
}

impl SequenceBuilder {
    fn new(block_kind: ContentKind) -> SequenceBuilder {
        SequenceBuilder {
            content_kind: block_kind,
            blocks: Vec::new(),
            inline: Vec::new(),
        }
    }
    
    fn to_html(mut self) -> HTML {
        if matches!(self.content_kind, ContentKind::RequireBlock(_) | ContentKind::RequireOneOf(_)) || !self.blocks.is_empty() {
            self.close_inline();
            HTML::seq(&self.blocks)
        } else {
            HTML::seq(&self.inline)
        }
    }
    
    fn newline(&mut self) {
        match self.content_kind {
            ContentKind::RequireBlock(_) |
            ContentKind::RequireOneOf(_) |
            ContentKind::AllowBlock(_) => {
                self.close_inline();
            },
            ContentKind::RequireInline => {
                self.inline.push(HTML::tag(str_ids::BR, HTML::Empty));
            },
            ContentKind::RequireInlineNoLineBreaks => {},
        }
    }
    
    fn close_inline(&mut self) {
        while matches!(self.inline.last(), Some(HTML::Whitespace)) {
            self.inline.pop();
        }
        if !self.inline.is_empty() {
            // TODO: paragraph contents might not be valid in wrapper. need to recurse?
            let paragraph = std::mem::take(&mut self.inline);
            self.blocks.push(HTML::tag(self.content_kind.wrap_with(), HTML::seq(&paragraph)));
        }
    }
    
    fn push(&mut self, child: HTML) {
        // do nothing
        if matches!(child, HTML::Empty) { return; }
        
        let is_block = match self.content_kind {
            ContentKind::RequireOneOf(tag_names) => child.is_all(tag_names),
            _ => child.is_block(),
        };
        if is_block {
            self.close_inline();
            self.blocks.push(child);
        } else if !matches!(child, HTML::Whitespace) || !self.inline.is_empty() {
            self.inline.push(child);
        }
    }
}
