use std::{fs, path};
use std::rc::Rc;
use indexmap::IndexMap;

use crate::errors::ModuleError;
use crate::parser;
use crate::utils::{SourceFile, taginfo};
use super::base::{Compiler, CompileResult};
use super::context::Context;
use super::frame::{InactiveFrame, ActiveFrame};
use super::html::HTML;
use super::value::ValueMap;

/// A module cache is used to compile a set of Papyri source files, including
/// loading of other Papyri source files by the `@import` and `@include`
/// functions, without compiling the imported source files more than once.
pub struct ModuleCache {
    /// A stack frame containing the compiled declarations from the Papyri
    /// standard library. It is only `None` during compilation of the standard
    /// library itself.
    stdlib: Option<InactiveFrame>,
    
    /// The cache of compiled modules.
    cache: IndexMap<Box<path::Path>, ModuleState>,
}

type CachedCompileResult = (HTML, Rc<ValueMap>);

#[derive(Debug, Clone)]
enum ModuleState {
    NotLoaded,
    Loaded(CachedCompileResult),
    Busy,
    Error,
}

impl Default for ModuleCache {
    fn default() -> ModuleCache {
        ModuleCache::new()
    }
}

impl ModuleCache {
    /// Creates a new module cache. Call `Context::compile_stdlib` to compile
    /// the standard library before using this cache for anything else.
    pub fn new() -> ModuleCache {
        ModuleCache {
            stdlib: None,
            cache: IndexMap::new(),
        }
    }
    
    fn get(&mut self, path: &path::Path) -> ModuleState {
        self.cache.get(path)
            .map_or(ModuleState::NotLoaded, ModuleState::clone)
    }
    
    fn set(&mut self, path: Box<path::Path>, state: ModuleState) -> usize {
        let (index, _) = self.cache.insert_full(path, state);
        index
    }
    
    fn set_by_index(&mut self, index: usize, state: ModuleState) {
        let (_, value) = self.cache.get_index_mut(index).unwrap();
        *value = state;
    }
}

impl Context {
    /// Creates and returns a new stack frame in which a Papyri module can be
    /// compiled. The new stack frame normally contains all native functions
    /// and the standard library.
    pub(super) fn get_initial_frame(&self) -> ActiveFrame {
        self.module_cache.stdlib
            .as_ref()
            .unwrap_or(&self.natives_frame)
            .new_empty_child_frame()
    }
    
    /// Compiles a Papyri source file.
    pub fn compile(&mut self, src: Rc<SourceFile>) -> CompileResult {
        self._compile(src, taginfo::ContentKind::REQUIRE_P)
    }
    
    /// Loads a Papyri source file from the filesystem and compiles it. This
    /// only fails if the source file cannot be read; any other errors which
    /// occur during compilation are reported through `self.diagnostics`.
    pub fn load_uncached(&mut self, path: &path::Path) -> Result<CompileResult, ModuleError> {
        SourceFile::from_path(path)
            .map(|src| self.compile(src))
            .map_err(ModuleError::IOError)
    }
    
    /// Loads a Papyri source file from the filesystem and compiles it, or
    /// returns a cached result if the source file has already been loaded and
    /// compiled. This fails if the source file cannot be read, if a circular
    /// import is detected, or if either of those two failures occurred during
    /// a previous attempt to load the same module; any other errors which
    /// occur during compilation are reported through `self.diagnostics`.
    /// 
    /// This method should only be used to load a module included or imported
    /// by another Papyri source file.
    pub fn load_cached(&mut self, path: &path::Path) -> Result<CachedCompileResult, ModuleError> {
        let k = fs::canonicalize(path)
            .map_err(ModuleError::IOError)?;
        
        match self.module_cache.get(&k) {
            ModuleState::NotLoaded => {
                let index = self.module_cache.set(k.into_boxed_path(), ModuleState::Busy);
                
                // compile with no `out_files`
                let old_out_files = std::mem::take(&mut self.out_files);
                let result = self.load_uncached(path)
                    .map(|r| (r.out, Rc::new(r.exports)));
                
                self.out_files = old_out_files;
                
                let state = result.as_ref()
                    .cloned()
                    .map_or(ModuleState::Error, ModuleState::Loaded);
                
                self.module_cache.set_by_index(index, state);
                result
            },
            ModuleState::Loaded(cached_result) => Ok(cached_result),
            ModuleState::Busy => Err(ModuleError::CircularImport),
            ModuleState::Error => Err(ModuleError::PreviousError),
        }
    }
    
    /// Compiles the standard library, and caches the result in this context's
    /// module cache.
    pub(super) fn compile_stdlib(&mut self) {
        use crate::errors;
        
        let src = SourceFile::synthetic("stdlib", include_str!("../std.papyri"));
        let result = self._compile(src, taginfo::ContentKind::RequireEmpty);
        let frame = ActiveFrame::new(
            Some(self.natives_frame.clone()),
            result.exports,
            None,
        );
        self.module_cache.stdlib = Some(frame.to_inactive());
        
        if !self.diagnostics.is_empty() {
            self.diagnostics.print();
            errors::ice("Standard library had errors or warnings");
        }
    }
    
    fn _compile(&mut self, src: Rc<SourceFile>, content_kind: taginfo::ContentKind) -> CompileResult {
        let root = parser::parse(src, &mut self.diagnostics, &mut self.string_pool);
        let mut compiler = Compiler::new(self);
        let out = compiler.compile_sequence(&root, content_kind);
        CompileResult {
            out,
            exports: compiler.exports,
        }
    }
}
