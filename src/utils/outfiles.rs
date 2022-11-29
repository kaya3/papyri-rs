use std::path::{Path, PathBuf};

use super::relpath;

pub struct OutFiles<T> {
    base_path: Box<Path>,
    v: Vec<(Box<Path>, T)>,
}

impl <T> OutFiles<T> {
    pub fn new<P: AsRef<Path>>(base_path: P) -> OutFiles<T> {
        OutFiles {
            base_path: Box::from(base_path.as_ref()),
            v: Vec::new(),
        }
    }
    
    pub fn is_empty(&self) -> bool {
        self.v.is_empty()
    }
    
    pub fn push<P: AsRef<Path>>(&mut self, path: P, t: T) -> bool {
        let Some(p) = relpath::make_relative(&self.base_path, path.as_ref()) else {
            return false
        };
        self.v.push((p.into_boxed_path(), t));
        true
    }
    
    pub fn clear(&mut self) {
        self.v.clear();
    }
    
    pub fn iter(&self) -> impl Iterator<Item=(PathBuf, &T)> {
        self.v.iter()
            .map(|(p, t)| (self.base_path.join(p), t))
    }
}
