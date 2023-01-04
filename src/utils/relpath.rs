//! This module contains helper functions for dealing with relative paths.

use std::path::{Path, PathBuf};
use normalize_path::NormalizePath;

use crate::errors;

/// Converts the given path `rel` into a normalised relative path using the
/// given `base` path. Returns `None` if the resulting path would not be a
/// descendant of `base`.
pub fn make_relative(base: &Path, rel: &Path) -> Option<PathBuf> {
    base.join(rel)
        .normalize()
        .strip_prefix(base)
        .map(Path::to_path_buf)
        .ok()
}

/// Returns a list of paths to all Papyri source files (files with a `.papyri`
/// extension) contained recursively in the given base path. The paths returned
/// are relative to the given base path.
/// 
/// Any filesystem errors which occur are reported to the `on_error` callback;
/// `None` is returned if the given base path is erroneous.
pub fn find_papyri_source_files_in_dir(path: &Path, mut on_error: impl FnMut(&Path, std::io::Error)) -> Option<Vec<PathBuf>> {
    let canonical_path = std::fs::canonicalize(path)
        .map_err(|e| on_error(path, e))
        .ok()?;
    
    let entry_to_path = |entry: Result<walkdir::DirEntry, walkdir::Error>| -> Option<PathBuf> {
        match entry {
            Ok(entry) => {
                let entry_path = entry.path()
                    .canonicalize()
                    .map_err(|e| on_error(entry.path(), e))
                    .ok()?;
                make_relative(&canonical_path, &entry_path)
            },
            Err(e) => {
                let path = e.path()
                    .unwrap_or(&canonical_path)
                    .to_path_buf();
                let io_err = e.into_io_error()
                    .unwrap_or_else(|| errors::ice("Not an IO error"));
                on_error(&path, io_err);
                None
            },
        }
    };
    
    let mut paths: Vec<_> = walkdir::WalkDir::new(path)
        .into_iter()
        .filter_map(entry_to_path)
        .filter(|p| super::sourcefile::is_papyri_file(p))
        .collect();
    
    paths.sort();
    Some(paths)
}
