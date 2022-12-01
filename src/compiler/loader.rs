use std::{fs, path};
use std::rc::Rc;
use indexmap::IndexMap;

use crate::errors::{Diagnostics, ModuleError, ReportingLevel};
use crate::utils::{SourceFile, StringPool, OutFiles};
use super::compile;
use super::compiler::{Compiler, CompileResult};
use super::context::Context;
use super::frame::{InactiveFrame, ActiveFrame};
use super::html::HTML;
use super::native::get_natives_frame;
use super::value::ValueMap;

/// A module loader is used to compile a set of Papyri source files, including
/// loading of other Papyri source files by the `@import` and `@include`
/// functions. Modules loaded this way are cached, so a library imported by
/// multiple files is only compiled once.
pub struct ModuleLoader {
    /// The pool of interned strings which may be used in modules, and may be
    /// added to during compilation.
    pub string_pool: StringPool,
    
    /// A stack frame containing the compiled declarations from the Papyri
    /// standard library. It is only `None` during compilation of the standard
    /// library itself.
    stdlib: Option<InactiveFrame>,
    
    /// The cache of compiled modules.
    cache: IndexMap<Box<path::Path>, ModuleState>,
}

type CachedCompileResult = (HTML, Rc<ValueMap>);

#[derive(Clone)]
enum ModuleState {
    NotLoaded,
    Loaded(CachedCompileResult),
    Busy,
    Error,
}

impl ModuleLoader {
    /// Creates a new module loader. This includes compiling the standard
    /// library.
    pub fn new() -> ModuleLoader {
        let mut loader = ModuleLoader {
            string_pool: StringPool::new(),
            stdlib: None,
            cache: IndexMap::new(),
        };
        loader.compile_stdlib();
        loader
    }
    
    /// Creates and returns a new stack frame in which a Papyri module can be
    /// compiled. The new stack frame normally contains all native functions
    /// and the standard library.
    pub fn get_initial_frame(&self) -> ActiveFrame {
        self.stdlib.as_ref().map_or_else(
            get_natives_frame,
            |stdlib| stdlib.new_child_frame(ValueMap::new(), None),
        )
    }
    
    /// Loads a Papyri source file from the filesystem and compiles it. This
    /// only fails if the source file cannot be read; any other errors which
    /// occur during compilation are reported through `diagnostics`.
    pub fn load_uncached(&mut self, path: &path::Path, diagnostics: &mut Diagnostics, out_files: Option<&mut OutFiles<HTML>>) -> Result<CompileResult, ModuleError> {
        SourceFile::from_path(path)
            .map(|src| compile(src, Context {loader: self, diagnostics, out_files}))
            .map_err(ModuleError::IOError)
    }
    
    /// Loads a Papyri source file from the filesystem and compiles it, or
    /// returns a cached result if the source file has already been loaded and
    /// compiled. This fails if the source file cannot be read, if a circular
    /// import is detected, or if either of those two failures occurred during
    /// a previous attempt to load the same module; any other errors which
    /// occur during compilation are reported through `diagnostics`.
    /// 
    /// This method should only be used to load a module included or imported
    /// by another Papyri source file.
    pub fn load_cached(&mut self, path: &path::Path, diagnostics: &mut Diagnostics) -> Result<CachedCompileResult, ModuleError> {
        let k = fs::canonicalize(&path)
            .map_err(ModuleError::IOError)?;
        
        match self.get(&k) {
            ModuleState::NotLoaded => {
                let index = self.set(k.into_boxed_path(), ModuleState::Busy);
                let result = self.load_uncached(path, diagnostics, None)
                    .map(|r| (r.out, Rc::new(r.exports)));
                
                let state = result.as_ref()
                    .map(CachedCompileResult::clone)
                    .map_or(ModuleState::Error, ModuleState::Loaded);
                
                self.set_by_index(index, state);
                result
            },
            ModuleState::Loaded(cached_result) => Ok(cached_result),
            ModuleState::Busy => Err(ModuleError::CircularImport),
            ModuleState::Error => Err(ModuleError::PreviousError),
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

    fn compile_stdlib(&mut self) {
        use crate::errors;
        use crate::parser;
        use crate::utils::taginfo;
        
        let stdlib_src = SourceFile::synthetic("stdlib", include_str!("../std.papyri"));
        
        let mut diagnostics = Diagnostics::new(ReportingLevel::All);
        let root = parser::parse(stdlib_src, &mut diagnostics, &mut self.string_pool);
        
        let mut compiler = Compiler::new(Context {
            diagnostics: &mut diagnostics,
            loader: self,
            out_files: None,
        });
        
        let html = compiler.compile_sequence(&root, taginfo::ContentKind::REQUIRE_P);
        let exports = compiler.exports;
        self.stdlib = Some(InactiveFrame::from(compiler.call_stack.pop().unwrap()));
        
        if !diagnostics.is_empty() {
            diagnostics.print();
            errors::ice("Standard library had errors or warnings");
        } else if !html.is_empty() {
            errors::ice("Standard library had non-empty output");
        } else if !exports.is_empty() {
            errors::ice("Standard library had exports");
        }
        
    }
}
