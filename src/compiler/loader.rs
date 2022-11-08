use std::collections::HashMap;
use std::{fs, path};
use std::rc::Rc;

use crate::utils::{Diagnostics, SourceFile, StringPool};
use super::compiler::compile_stdlib;
use super::frame::InactiveFrame;
use super::{CompileResult, compile};

pub struct ModuleLoader {
    pub string_pool: StringPool,
    pub stdlib: Option<InactiveFrame>,
    cache: HashMap<path::PathBuf, ModuleState>,
}

#[derive(Clone)]
pub enum ModuleState {
    NotLoaded,
    Loaded(Rc<CompileResult>),
    Busy,
    Error,
}

impl ModuleLoader {
    pub fn new() -> ModuleLoader {
        let mut loader = ModuleLoader {
            string_pool: StringPool::new(),
            stdlib: None,
            cache: HashMap::new(),
        };
        loader.stdlib = Some(compile_stdlib(&mut loader));
        loader
    }
    
    pub fn load_uncached(&mut self, path: &path::PathBuf, diagnostics: &mut Diagnostics) -> Result<CompileResult, String> {
        let src = SourceFile::from_path(path)?;
        Ok(compile(src, self, diagnostics))
    }
    
    pub fn load_cached(&mut self, path: &path::PathBuf, diagnostics: &mut Diagnostics) -> Result<Rc<CompileResult>, String> {
        let k = fs::canonicalize(path)
            .map_err(|e| e.to_string())?;
        match self.get(&k) {
            ModuleState::NotLoaded => {
                self.set(k.clone(), ModuleState::Busy);
                match self.load_uncached(path, diagnostics) {
                    Ok(r) => {
                        let r = Rc::new(r);
                        self.set(k, ModuleState::Loaded(r.clone()));
                        Ok(r)
                    }
                    Err(e) => {
                        self.set(k, ModuleState::Error);
                        Err(e)
                    }
                }
            },
            ModuleState::Loaded(result) => Ok(result),
            ModuleState::Busy => Err("circular import detected".to_string()),
            ModuleState::Error => Err("previous error loading module".to_string()),
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
