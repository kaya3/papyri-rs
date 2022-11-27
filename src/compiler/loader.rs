use std::{fs, path};
use std::rc::Rc;
use indexmap::IndexMap;

use crate::errors::{Diagnostics, ModuleError};
use crate::utils::{SourceFile, StringPool};
use super::compiler::compile_stdlib;
use super::frame::{InactiveFrame, ActiveFrame};
use super::html::HTML;
use super::native::get_natives_frame;
use super::value::ValueMap;
use super::{CompileResult, compile};

pub struct ModuleLoader {
    pub string_pool: StringPool,
    stdlib: Option<InactiveFrame>,
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
    pub fn new() -> ModuleLoader {
        let mut loader = ModuleLoader {
            string_pool: StringPool::new(),
            stdlib: None,
            cache: IndexMap::new(),
        };
        loader.stdlib = Some(compile_stdlib(&mut loader));
        loader
    }
    
    pub fn get_initial_frame(&self) -> ActiveFrame {
        self.stdlib.as_ref().map_or_else(
            get_natives_frame,
            |stdlib| stdlib.new_child_frame(ValueMap::new(), None),
        )
    }
    
    pub fn load_uncached(&mut self, path: &path::Path, diagnostics: &mut Diagnostics) -> Result<CompileResult, ModuleError> {
        SourceFile::from_path(path)
            .map(|src| compile(src, self, diagnostics))
            .map_err(ModuleError::IOError)
    }
    
    pub fn load_cached(&mut self, path: &path::Path, diagnostics: &mut Diagnostics) -> Result<CachedCompileResult, ModuleError> {
        let k = fs::canonicalize(&path)
            .map_err(ModuleError::IOError)?;
        
        match self.get(&k) {
            ModuleState::NotLoaded => {
                let index = self.set(k.into_boxed_path(), ModuleState::Busy);
                let result = self.load_uncached(path, diagnostics)
                    .map(|result| (result.out, Rc::new(result.exports)));
                
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
}
