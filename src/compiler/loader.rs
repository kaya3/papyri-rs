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
    cache: IndexMap<path::PathBuf, ModuleState>,
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
        match self.stdlib.as_ref() {
            Some(stdlib) => stdlib.new_child_frame(ValueMap::new()),
            None => get_natives_frame(),
        }
    }
    
    pub fn load_uncached(&mut self, path: path::PathBuf, diagnostics: &mut Diagnostics) -> Result<CompileResult, ModuleError> {
        let src = SourceFile::from_path(path)
            .map_err(ModuleError::IOError)?;
        Ok(compile(src, self, diagnostics))
    }
    
    pub fn load_cached(&mut self, path: path::PathBuf, diagnostics: &mut Diagnostics) -> Result<CachedCompileResult, ModuleError> {
        let k = fs::canonicalize(&path)
            .map_err(ModuleError::IOError)?;
        match self.get(&k) {
            ModuleState::NotLoaded => {
                self.set(k.clone(), ModuleState::Busy);
                match self.load_uncached(path, diagnostics) {
                    Ok(result) => {
                        let cached_result = (result.out, Rc::new(result.exports));
                        self.set(k, ModuleState::Loaded(cached_result.clone()));
                        Ok(cached_result)
                    },
                    Err(e) => {
                        self.set(k, ModuleState::Error);
                        Err(e)
                    }
                }
            },
            ModuleState::Loaded(result) => Ok(result),
            ModuleState::Busy => Err(ModuleError::CircularImport),
            ModuleState::Error => Err(ModuleError::PreviousError),
        }
    }
    
    fn get(&mut self, path: &path::PathBuf) -> ModuleState {
        match self.cache.get(path) {
            Some(s) => s.clone(),
            None => ModuleState::NotLoaded,
        }
    }
    
    fn set(&mut self, path: path::PathBuf, state: ModuleState) {
        self.cache.insert(path, state);
    }
}
