use crate::errors;
use crate::parser::{AST, Type};
use crate::utils::taginfo::ContentKind;
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
    pub(super) fn compile_sequence_in_tag(&mut self, sequence: &[AST], tag_name: NameID) -> HTML {
        self._compile_sequence(sequence, ContentKind::for_(tag_name), Some(tag_name))
    }
    
    pub(super) fn compile_sequence(&mut self, sequence: &[AST], content_kind: ContentKind) -> HTML {
        self._compile_sequence(sequence, content_kind, None)
    }
    
    fn _compile_sequence(&mut self, sequence: &[AST], content_kind: ContentKind, parent_tag: Option<NameID>) -> HTML {
        let mut builder = SequenceBuilder::new(content_kind, parent_tag);
        for child in sequence {
            if let Err(e) = self.compile_node(&mut builder, child) {
                self.report(e, child.range());
            }
        }
        
        builder.into_html().unwrap_or_else(|e| {
            let range = sequence[0].range().to_end(sequence.last().unwrap().range().end);
            self.report(e, range);
            HTML::Empty
        })
    }
    
    fn compile_node(&mut self, builder: &mut SequenceBuilder, node: &AST) -> errors::PapyriResult {
        match node {
            AST::Export(export) => {
                self.compile_export(export)?;
            }
            AST::Expr(expr) => {
                let v = self.evaluate_node(expr, &Type::Html)?;
                builder.push(self, v.expect_convert())?;
            },
            AST::FuncDef(def) => {
                let f = self.compile_func_def(def).into();
                self.set_var(def.name_id, f, false, def.signature.range);
            },
            &AST::CodeFence(range, is_multiline) => {
                let h = self.compile_code_fence(range, is_multiline)?;
                builder.push(self, h)?;
            },
            AST::Text(text, ..) => builder.push(self, text.clone().into())?,
            AST::Char(c, ..) => builder.push(self, c.to_string().into())?,
            AST::Whitespace(..) => builder.push(self, HTML::Whitespace)?,
            AST::ParagraphBreak(..) => builder.newline()?,
        }
        Ok(())
    }
}

struct SequenceBuilder {
    parent_tag: Option<NameID>,
    content_kind: ContentKind,
    children: Vec<HTML>,
    next_child: Option<Box<SequenceBuilder>>,
    require_inline: bool,
}

impl SequenceBuilder {
    fn new(content_kind: ContentKind, parent_tag: Option<NameID>) -> SequenceBuilder {
        SequenceBuilder {
            parent_tag,
            content_kind,
            children: Vec::new(),
            next_child: None,
            require_inline: matches!(content_kind, ContentKind::RequireInline | ContentKind::RequireInlineNoLineBreaks),
        }
    }
    
    fn into_html(mut self) -> errors::PapyriResult<HTML> {
        if matches!(self.content_kind, ContentKind::AllowBlock(..)) && self.children.is_empty() {
            return self.next_child.map_or(Ok(HTML::Empty), |c| c.into_html());
        }
        
        self.close_child()?;
        while matches!(self.children.last(), Some(c) if c.is_whitespace()) {
            self.children.pop();
        }
        let html = HTML::from_iter(self.children);
        
        if matches!(self.content_kind, ContentKind::RequireEmpty) && !html.is_empty() {
            Err(errors::TypeError::NoContentAllowed.into())
        } else {
            Ok(html)
        }
    }
    
    fn newline(&mut self) -> errors::PapyriResult {
        match self.content_kind {
            ContentKind::RequireBlock(..) |
            ContentKind::RequireOneOf(..) |
            ContentKind::AllowBlock(..) => {
                self.close_child()
            },
            ContentKind::RequireInline => {
                self.close_child()?;
                self.children.push(HTML::tag(str_ids::BR, HTML::Empty));
                Ok(())
            },
            ContentKind::RequireInlineNoLineBreaks => {
                Err(errors::TypeError::ParagraphBreakNotAllowed.into())
            },
            ContentKind::RequireEmpty => {
                // this is in the stdlib or a self-closing tag; safe to ignore a paragraph break
                Ok(())
            },
        }
    }
    
    fn close_child(&mut self) -> errors::PapyriResult {
        if let Some(child) = std::mem::take(&mut self.next_child) {
            let html = child.into_html()?;
            if !html.is_empty() {
                let name_id = self.content_kind.wrap_with();
                self.children.push(html.in_tag(name_id));
            }
        }
        Ok(())
    }
    
    fn push(&mut self, compiler: &Compiler, child: HTML) -> errors::PapyriResult {
        // do nothing
        if child.is_empty() { return Ok(()); }
        
        if self.require_inline && child.is_block() {
            let child_name = child.block_kind().map(|tag| compiler.get_name(tag));
            let parent_name = self.parent_tag.map(|tag| compiler.get_name(tag));
            
            let e = match (child_name, parent_name) {
                (Some(child), Some(parent)) => errors::TypeError::TagNotAllowedIn(child, parent),
                (Some(child), None) => errors::TypeError::TagNotAllowed(child),
                
                // shouldn't happen, because `is_block()` is equivalent to `block_kind().is_some()`
                // but might be needed in future
                (None, Some(parent)) => errors::TypeError::BlockNotAllowedIn(parent),
                (None, None) => errors::TypeError::BlockNotAllowed,
            };
            return Err(e.into());
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
                let parent_tag = self.content_kind.wrap_with();
                let child_content_kind = ContentKind::for_(parent_tag);
                Box::new(SequenceBuilder::new(
                    child_content_kind,
                    Some(parent_tag),
                ))
            }).push(compiler, child)
        } else {
            // whitespace at start, ignore
            Ok(())
        }
    }
}
