use crate::errors::TypeError;
use crate::parser::ast::AST;
use crate::utils::sourcefile::SourceRange;
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
    pub(super) fn compile_sequence(&mut self, sequence: &[AST], content_kind: ContentKind) -> HTML {
        let mut comp = SequenceBuilder::new(content_kind);
        for child in sequence {
            let r = if let AST::ParagraphBreak(..) = child {
                comp.newline()
            } else {
                let child_html = self.compile_node(child);
                comp.push(child_html)
            };
            if let Err(e) = r {
                self.sequence_error(e, child.range());
            }
        }
        
        comp.into_html().unwrap_or_else(|e| {
            let range = sequence[0].range().to_end(sequence.last().unwrap().range().end);
            self.sequence_error(e, range);
            HTML::Empty
        })
    }
    
    fn sequence_error(&mut self, e: SequenceError, range: SourceRange) {
        let e = match e {
            SequenceError::TagNotAllowed(name_id) => TypeError::TagNotAllowed(self.get_name(name_id).to_string()),
            SequenceError::ParagraphBreakNotAllowed => TypeError::ParagraphBreakNotAllowed,
            SequenceError::NoContentAllowed => TypeError::NoContentAllowed,
        };
        self.type_error(e, range);
    }
}

struct SequenceBuilder {
    content_kind: ContentKind,
    children: Vec<HTML>,
    next_child: Option<Box<SequenceBuilder>>,
    require_inline: bool,
}

enum SequenceError {
    TagNotAllowed(NameID),
    ParagraphBreakNotAllowed,
    NoContentAllowed,
}

impl SequenceBuilder {
    fn new(content_kind: ContentKind) -> SequenceBuilder {
        SequenceBuilder {
            content_kind,
            children: Vec::new(),
            next_child: None,
            require_inline: matches!(content_kind, ContentKind::RequireInline | ContentKind::RequireInlineNoLineBreaks),
        }
    }
    
    fn into_html(mut self) -> Result<HTML, SequenceError> {
        if matches!(self.content_kind, ContentKind::AllowBlock(..)) && self.children.is_empty() {
            return self.next_child.map_or(Ok(HTML::Empty), |c| c.into_html());
        }
        
        self.close_child()?;
        while matches!(self.children.last(), Some(c) if c.is_whitespace()) {
            self.children.pop();
        }
        let html = HTML::seq(self.children);
        
        if matches!(self.content_kind, ContentKind::RequireEmpty) && !html.is_empty() {
            Err(SequenceError::NoContentAllowed)
        } else {
            Ok(html)
        }
    }
    
    fn newline(&mut self) -> Result<(), SequenceError> {
        match self.content_kind {
            ContentKind::RequireBlock(..) |
            ContentKind::RequireOneOf(..) |
            ContentKind::AllowBlock(..) => {
                self.close_child()
            },
            ContentKind::RequireInline => {
                self.push(HTML::tag(str_ids::BR, HTML::Empty))
            },
            ContentKind::RequireInlineNoLineBreaks => {
                Err(SequenceError::ParagraphBreakNotAllowed)
            },
            ContentKind::RequireEmpty => {
                // this is in the stdlib or a self-closing tag; safe to ignore a paragraph break
                Ok(())
            },
        }
    }
    
    fn close_child(&mut self) -> Result<(), SequenceError> {
        if let Some(child) = std::mem::take(&mut self.next_child) {
            let html = child.into_html()?;
            if !html.is_empty() {
                self.children.push(HTML::tag(self.content_kind.wrap_with(), html));
            }
        }
        Ok(())
    }
    
    fn push(&mut self, child: HTML) -> Result<(), SequenceError> {
        // do nothing
        if child.is_empty() { return Ok(()); }
        
        if self.require_inline {
            if let Some(block_kind) = child.block_kind() {
                return Err(SequenceError::TagNotAllowed(block_kind));
            }
        }
        
        let is_allowed = match self.content_kind {
            ContentKind::RequireOneOf(tag_names) => child.is_all(tag_names),
            ContentKind::RequireBlock(..) |
            ContentKind::AllowBlock(..) => child.is_block(),
            _ => true,
        };
        if is_allowed {
            self.close_child()?;
            if !self.children.is_empty() || !child.is_whitespace() {
                self.children.push(child);
            }
            Ok(())
        } else if self.next_child.is_some() || !child.is_whitespace() {
            self.next_child.get_or_insert_with(|| {
                let child_content_kind = content_kind(self.content_kind.wrap_with());
                Box::new(SequenceBuilder::new(child_content_kind))
            }).push(child)
        } else {
            // whitespace at start, ignore
            Ok(())
        }
    }
}
