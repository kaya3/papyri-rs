use crate::errors;
use crate::parser::AST;
use super::context::Context;
use super::frame::ActiveFrame;
use super::html::HTML;
use super::types::Type;
use super::value::ValueMap;

/// The result of compiling a Papyri source file. The output may be incomplete
/// if there were errors during compilation. 
pub struct CompileResult {
    /// The direct HTML output from compiling a Papyri source file. Other output
    /// may have been queued in an `OutFiles` collector, if the Papyri source
    /// file used the `@write_file` function to produce its output.
    pub out: HTML,
    
    /// The values exported by this Papyri source file.
    pub exports: ValueMap,
}

pub(super) struct Compiler<'a> {
    pub(super) ctx: &'a mut Context,
    pub(super) call_stack: Vec<ActiveFrame>,
    pub(super) exports: ValueMap,
}

impl <'a> Compiler<'a> {
    pub(super) fn new(ctx: &'a mut Context) -> Compiler<'a> {
        let mut call_stack = Vec::with_capacity(16);
        call_stack.push(ctx.get_initial_frame());
        Compiler {
            ctx,
            call_stack,
            exports: ValueMap::default(),
        }
    }
    
    pub(super) fn compile_node(&mut self, node: &AST) -> HTML {
        match node {
            AST::Export(e) => {
                self.compile_export(e);
                HTML::Empty
            }
            AST::Expr(expr) => {
                self.evaluate_node(expr, &Type::HTML)
                    .map_or(HTML::Empty, |v| self.compile_value(v))
            },
            AST::FuncDef(def) => {
                let f = self.compile_func_def(def).into();
                self.set_var(def.name_id, f, false, def.signature.range);
                HTML::Empty
            },
            AST::Text(text, ..) => text.clone().into(),
            AST::Whitespace(..) => HTML::Whitespace,
            
            AST::ParagraphBreak(range) => errors::ice_at("paragraph break should be handled in SequenceCompiler", *range),
        }
    }
}
