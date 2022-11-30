use std::path::{Path, PathBuf};

use super::relpath;

/// Collects pairs of (path, contents) representing files to be written to the
/// filesystem if compilation is successful.
pub struct OutFiles<T> {
    base_path: Box<Path>,
    v: Vec<(Box<Path>, T)>,
}

impl <T> OutFiles<T> {
    /// Creates a new collector of (path, contents) pairs. File paths will be
    /// resolved relative to the given base path.
    pub fn new<P: AsRef<Path>>(base_path: P) -> OutFiles<T> {
        OutFiles {
            base_path: Box::from(base_path.as_ref()),
            v: Vec::new(),
        }
    }
    
    /// Indicates whether this collector holds any files to be written.
    pub fn is_empty(&self) -> bool {
        self.v.is_empty()
    }
    
    /// Attempts to add a (path, contents) pair to this collector, and returns
    /// a bool indicating success. Failure occurs when the given path is
    /// outside of this collector's base path; this ensures that files can only
    /// be written to the designated output directory.
    pub fn try_push<P: AsRef<Path>>(&mut self, path: P, t: T) -> bool {
        let Some(p) = relpath::make_relative(&self.base_path, path.as_ref()) else {
            return false
        };
        self.v.push((p.into_boxed_path(), t));
        true
    }
    
    /// Empties this collector, and returns an iterator of the (path, contents)
    /// pairs which were held in it.
    pub fn take_iter(&mut self) -> impl Iterator<Item=(PathBuf, T)> + '_ {
        std::mem::take(&mut self.v)
            .into_iter()
            .map(|(p, t)| (self.base_path.join(p), t))
    }
}
