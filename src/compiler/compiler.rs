use crate::errors::{ice_at, Warning};
use crate::parser::{AST, text};
use crate::utils::taginfo;
use super::context::Context;
use super::frame::ActiveFrame;
use super::html::HTML;
use super::types::Type;
use super::value::{Value, ValueMap};

/// The result of compiling a Papyri source file. The output may be incomplete
/// if there were errors during compilation. 
pub struct CompileResult {
    /// The direct HTML output from compiling a Papyri source file. Other output
    /// may have been queued in an `OutFiles` collector, if the Papyri source
    /// file used the `@write_file` function to produce its output.
    pub out: HTML,
    
    /// The values exported by this Papyri source file, using the `@export`
    /// function.
    pub exports: ValueMap,
}

pub struct Compiler<'a> {
    pub ctx: &'a mut Context,
    pub call_stack: Vec<ActiveFrame>,
    pub exports: ValueMap,
}

impl <'a> Compiler<'a> {
    pub fn new(ctx: &'a mut Context) -> Compiler<'a> {
        let frame = ctx.module_cache.get_initial_frame();
        Compiler {
            ctx,
            call_stack: vec![frame],
            exports: ValueMap::new(),
        }
    }
    
    pub fn compile_node(&mut self, node: &AST) -> HTML {
        match node {
            AST::FuncDef(def) => {
                if def.name_id.is_anonymous() {
                    self.warning(Warning::AnonymousFunctionInText, &def.range);
                } else {
                    let f = self.compile_func_def(def);
                    self.set_var(def.name_id, Value::Func(f), false, node.range());
                }
                HTML::Empty
            },
            
            AST::Group(group, ..) => self.compile_sequence(group, taginfo::ContentKind::ALLOW_P),
            AST::Tag(tag) => self.compile_tag(tag),
            
            AST::FuncCall(..) |
            AST::Match(..) |
            AST::VarName(..) |
            AST::Verbatim(..) |
            AST::List(..) => {
                self.evaluate_node(node, &Type::AnyHTML)
                    .map_or(HTML::Empty, |v| self.compile_value(v))
            },
            
            AST::LiteralValue(tok) => {
                tok.text()
                    .map(text::substitutions)
                    .map_or(HTML::Empty, HTML::from)
            },
            AST::Text(text, ..) => text.clone().into(),
            AST::Whitespace(..) => HTML::Whitespace,
            
            AST::ParagraphBreak(range) => ice_at("paragraph break should be handled in SequenceCompiler", range),
            AST::Template(.., range) => ice_at("template should not occur in non-value context", range),
        }
    }
}
