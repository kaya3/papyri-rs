use std::rc::Rc;

use crate::errors;
use crate::utils::{OutFiles, NameID, StringPool, text};
use crate::utils::sourcefile::{SourceRange, SourceFileCache, SourceFile};
use super::base::Compiler;
use super::frame::InactiveFrame;
use super::html::HTML;
use super::module_loader::ModuleCache;
use super::native::NativeDefs;
use super::types::Type;

/// Holds the context for a compilation job.
pub struct Context {
    /// The source files loaded in this context.
    pub(crate) source_files: SourceFileCache,
    
    /// The diagnostic collector for this compiler context.
    pub diagnostics: errors::Diagnostics,
    
    /// The module cache for this compiler context.
    pub(super) module_cache: ModuleCache,
    
    /// A cache containing the native functions.
    pub(super) natives: NativeDefs,
    
    /// A stack frame containing the native functions.
    pub(super) natives_frame: InactiveFrame,
    
    /// The pool of interned names for this compiler context.
    pub(super) string_pool: StringPool,
    
    /// The unique ID generator for this compiler context.
    pub(super) unique_ids: text::UniqueIDGenerator,
    
    /// The output files collector for this compiler context, if it has one.
    pub out_files: Option<OutFiles<HTML>>,
}

impl Context {
    /// Creates a new compiler context. This includes compiling the standard
    /// library.
    pub fn new(reporting_level: errors::ReportingLevel, out_dir: Option<&std::path::Path>) -> Context {
        let natives = NativeDefs::build();
        let natives_frame = natives.to_frame().to_inactive();
        let mut ctx = Context {
            source_files: SourceFileCache::new(),
            string_pool: StringPool::new(),
            diagnostics: errors::Diagnostics::new(reporting_level),
            module_cache: ModuleCache::new(),
            natives,
            natives_frame,
            unique_ids: text::UniqueIDGenerator::new(),
            out_files: out_dir.map(OutFiles::new),
        };
        ctx.compile_stdlib();
        ctx
    }
    
    pub(crate) fn get_source_str(&self, range: SourceRange) -> &str {
        self.source_files.get_str(range)
    }
    
    /// Adds an output file to this context's collector. The operation may fail
    /// if this context has no output file collector, or if the path is not
    /// within the output directory.
    pub(super) fn push_out_file(&mut self, path: std::rc::Rc<str>, content: HTML) -> Result<(), errors::RuntimeError> {
        let Some(sink) = self.out_files.as_mut() else {
            return Err(errors::RuntimeError::WriteFileNotAllowed);
        };
        if sink.try_push(path.as_ref(), content) {
            Ok(())
        } else {
            Err(errors::RuntimeError::PathNotInOutDir(path))
        }
    }
    
    /// Clears any state from the previous compile job. Any `out_files` must
    /// already have been handled before calling this method.
    pub fn reset(&mut self) {
        self.diagnostics.clear();
        self.unique_ids.clear();
        if matches!(self.out_files, Some(ref o) if !o.is_empty()) {
            errors::ice("Output files were not consumed");
        }
    }
}

impl <'a> Compiler<'a> {
    pub(super) fn get_source_file(&self, range: SourceRange) -> Rc<SourceFile> {
        self.ctx.source_files.get(range.src_id)
    }
    
    pub(super) fn syntax_error(&mut self, e: errors::SyntaxError, range: SourceRange) {
        self.ctx.diagnostics.syntax_error(e, self.get_source_file(range), range);
    }
    
    pub(super) fn name_error(&mut self, e: errors::NameError, range: SourceRange) {
        self.ctx.diagnostics.name_error(e, self.stack_trace(), self.get_source_file(range), range);
    }
    
    pub(super) fn type_error(&mut self, e: errors::TypeError, range: SourceRange) {
        self.ctx.diagnostics.type_error(e, self.stack_trace(), self.get_source_file(range), range);
    }
    
    pub(super) fn runtime_error(&mut self, e: errors::RuntimeError, range: SourceRange) {
        self.ctx.diagnostics.runtime_error(e, self.stack_trace(), self.get_source_file(range), range);
    }
    
    pub(super) fn warning(&mut self, e: errors::Warning, range: SourceRange) {
        self.ctx.diagnostics.warning(e, self.get_source_file(range), range);
    }
    
    pub(super) fn runtime_warning(&mut self, e: errors::RuntimeWarning, range: SourceRange) {
        self.ctx.diagnostics.runtime_warning(e, self.stack_trace(), self.get_source_file(range), range);
    }
    
    pub(super) fn module_error(&mut self, path: &std::path::Path, e: errors::ModuleError, range: SourceRange) {
        self.ctx.diagnostics.module_error(path, e, self.get_source_file(range), range);
    }
    
    pub(super) fn err_expected_type(&mut self, expected: Type, was: Type, range: SourceRange) {
        self.type_error(errors::TypeError::ExpectedWas(expected, was), range);
    }
    
    pub(super) fn string_pool(&self) -> &StringPool {
        &self.ctx.string_pool
    }
    
    pub(super) fn string_pool_mut(&mut self) -> &mut StringPool {
        &mut self.ctx.string_pool
    }
    
    pub(super) fn get_name(&self, name_id: NameID) -> &str {
        self.string_pool().get(name_id)
    }
}
