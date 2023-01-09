use super::context::Context;
use super::frame::ActiveFrame;
use super::html::HTML;
use super::value::Dict;

/// The result of compiling a Papyri source file. The output may be incomplete
/// if there were errors during compilation. 
pub struct CompileResult {
    /// The direct HTML output from compiling a Papyri source file. Other output
    /// may have been queued in an `OutFiles` collector, if the Papyri source
    /// file used the `@write_file` function to produce its output.
    pub out: HTML,
    
    /// The values exported by this Papyri source file.
    pub exports: Dict,
}

pub(super) struct Compiler<'a> {
    pub(super) ctx: &'a mut Context,
    pub(super) call_stack: Vec<ActiveFrame>,
    pub(super) exports: Dict,
}

impl <'a> Compiler<'a> {
    pub(super) fn new(ctx: &'a mut Context) -> Compiler<'a> {
        let mut call_stack = Vec::with_capacity(16);
        call_stack.push(ctx.get_initial_frame());
        Compiler {
            ctx,
            call_stack,
            exports: Dict::default(),
        }
    }
}
