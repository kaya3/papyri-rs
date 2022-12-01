use crate::errors::{Diagnostics, RuntimeError, RuntimeWarning, TypeError, Warning, ModuleError, NameError};
use crate::utils::{OutFiles, SourceRange, NameID, StringPool};
use super::compiler::Compiler;
use super::html::HTML;
use super::loader::ModuleLoader;
use super::types::Type;

/// Holds the context for a compilation job.
pub struct Context<'a> {
    /// The diagnostic collector for this compiler context.
    pub diagnostics: &'a mut Diagnostics,
    
    /// The module loader for this compiler context.
    pub loader: &'a mut ModuleLoader,
    
    /// The output files collector for this compiler context, if it has one.
    pub out_files: Option<&'a mut OutFiles<HTML>>,
}

impl <'a> Compiler<'a> {
    pub fn name_error(&mut self, e: NameError, range: &SourceRange) {
        self.ctx.diagnostics.name_error(e, range);
    }
    
    pub fn type_error(&mut self, e: TypeError, range: &SourceRange) {
        self.ctx.diagnostics.type_error(e, range);
    }
    
    pub fn runtime_error(&mut self, e: RuntimeError, range: &SourceRange) {
        self.ctx.diagnostics.runtime_error(e, self.stack_trace(), range);
    }
    
    pub fn warning(&mut self, e: Warning, range: &SourceRange) {
        self.ctx.diagnostics.warning(e, range);
    }
    
    pub fn runtime_warning(&mut self, e: RuntimeWarning, range: &SourceRange) {
        self.ctx.diagnostics.runtime_warning(e, self.stack_trace(), range);
    }
    
    pub fn module_error(&mut self, path: &std::path::Path, e: ModuleError, range: &SourceRange) {
        self.ctx.diagnostics.module_error(path, e, range);
    }
    
    pub fn err_expected_type(&mut self, expected: &Type, was: &Type, range: &SourceRange) {
        self.type_error(TypeError::ExpectedWas(expected.clone(), was.clone()), range);
    }
    
    pub fn string_pool_mut(&mut self) -> &mut StringPool {
        &mut self.ctx.loader.string_pool
    }
    
    pub fn get_name(&self, name_id: NameID) -> &str {
        self.ctx.loader.string_pool.get(name_id)
    }
}
