use std::path::{Path, PathBuf};
use normalize_path::NormalizePath;

use crate::errors;

pub fn make_relative(base: &Path, rel: &Path) -> Option<PathBuf> {
    base.join(rel)
        .normalize()
        .strip_prefix(&base)
        .ok()
        .map(Path::to_path_buf)
}

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
                let path = e.path().unwrap_or_else(|| &canonical_path).to_path_buf();
                let io_err = e.into_io_error().unwrap_or_else(|| errors::ice("Not an IO error"));
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
