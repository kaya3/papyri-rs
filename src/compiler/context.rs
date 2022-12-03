use crate::errors;
use crate::utils::{OutFiles, SourceRange, NameID, StringPool, text};
use super::compiler::Compiler;
use super::html::HTML;
use super::module_loader::ModuleCache;
use super::types::Type;

/// Holds the context for a compilation job.
pub struct Context {
    /// The diagnostic collector for this compiler context.
    pub diagnostics: errors::Diagnostics,
    
    /// The module cache for this compiler context.
    pub module_cache: ModuleCache,
    
    /// The pool of interned names for this compiler context.
    pub string_pool: StringPool,
    
    /// The unique ID generator for this compiler context.
    pub unique_ids: text::UniqueIDGenerator,
    
    /// The output files collector for this compiler context, if it has one.
    pub out_files: Option<OutFiles<HTML>>,
}

impl Context {
    /// Creates a new compiler context. This includes compiling the standard
    /// library.
    pub fn new(reporting_level: errors::ReportingLevel, out_dir: Option<&std::path::Path>) -> Context {
        let mut ctx = Context {
            string_pool: StringPool::new(),
            diagnostics: errors::Diagnostics::new(reporting_level),
            module_cache: ModuleCache::new(),
            unique_ids: text::UniqueIDGenerator::new(),
            out_files: out_dir.map(OutFiles::new),
        };
        ctx.compile_stdlib();
        ctx
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
    pub fn name_error(&mut self, e: errors::NameError, range: &SourceRange) {
        self.ctx.diagnostics.name_error(e, range);
    }
    
    pub fn type_error(&mut self, e: errors::TypeError, range: &SourceRange) {
        self.ctx.diagnostics.type_error(e, range);
    }
    
    pub fn runtime_error(&mut self, e: errors::RuntimeError, range: &SourceRange) {
        self.ctx.diagnostics.runtime_error(e, self.stack_trace(), range);
    }
    
    pub fn warning(&mut self, e: errors::Warning, range: &SourceRange) {
        self.ctx.diagnostics.warning(e, range);
    }
    
    pub fn runtime_warning(&mut self, e: errors::RuntimeWarning, range: &SourceRange) {
        self.ctx.diagnostics.runtime_warning(e, self.stack_trace(), range);
    }
    
    pub fn module_error(&mut self, path: &std::path::Path, e: errors::ModuleError, range: &SourceRange) {
        self.ctx.diagnostics.module_error(path, e, range);
    }
    
    pub fn err_expected_type(&mut self, expected: &Type, was: &Type, range: &SourceRange) {
        self.type_error(errors::TypeError::ExpectedWas(expected.clone(), was.clone()), range);
    }
    
    pub fn string_pool_mut(&mut self) -> &mut StringPool {
        &mut self.ctx.string_pool
    }
    
    pub fn get_name(&self, name_id: NameID) -> &str {
        self.ctx.string_pool.get(name_id)
    }
}
