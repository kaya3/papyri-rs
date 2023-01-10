//! This module contains declarations for Papyri source files and spans.

use std::{fs, path};
use std::rc::Rc;
use nonmax::NonMaxU32;
use once_cell::unsync::OnceCell;

use crate::errors;

/// Determines whether the given file path has the `.papyri` extension.
pub fn is_papyri_file(path: &path::Path) -> bool {
    matches!(path.extension(), Some(ext) if ext.eq_ignore_ascii_case("papyri"))
}

/// Determines whether the given file path appears to a Papyri library, i.e. a
/// file with the `.lib.papyri` extension.
pub fn is_papyri_library(path: &path::Path) -> bool {
    let s = path.to_string_lossy().to_ascii_lowercase();
    s.ends_with(".lib.papyri")
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct SourceFileID(NonMaxU32);

impl SourceFileID {
    pub(crate) const ANONYMOUS: SourceFileID = SourceFileID::of(u32::MAX - 1);
    
    const fn of(id: u32) -> SourceFileID {
        let Some(id) = NonMaxU32::new(id) else { panic!() };
        SourceFileID(id)
    }
}

#[derive(Debug, Clone, Copy)]
/// Represents a span of code in a Papyri source file. The source must be
/// smaller than 4GiB.
pub struct SourceRange {
    /// The Papyri source file which this span occurs in.
    pub(crate) src_id: SourceFileID,
    
    /// The index of the start of this span.
    pub(crate) start: u32,
    
    /// The index of the end of this span.
    pub(crate) end: u32,
}

impl SourceRange {
    /// Returns a new span, starting at the same position as this one, with a
    /// new end index.
    pub(crate) fn to_end(self, end: u32) -> SourceRange {
        SourceRange {
            src_id: self.src_id,
            start: self.start,
            end,
        }
    }
}

/// Represents a Papyri source file. The source as a string must be smaller
/// than 4 GiB.
pub struct SourceFile {
    /// This source file's ID, if it is allocated in a cache; otherwise, the id
    /// is `SourceFileID::ANONYMOUS`.
    pub(crate) id: SourceFileID,
    
    /// The path to this source file, as a `path::Path`. If the source file is
    /// synthetic, then this path is empty.
    pub(crate) path: Box<path::Path>,
    
    /// The path to this source file, as a string. If the source file is
    /// synthetic, then this string is surrounded by angle-brackets, e.g.
    /// `<string>`.
    pub(crate) path_str: Box<str>,
    
    /// The contents of this source file.
    pub(crate) src: Box<str>,
    
    line_col_coords: OnceCell<Box<[(u32, u32)]>>,
}

impl SourceFile {
    fn new(id: SourceFileID, path: Box<path::Path>, path_str: Box<str>, src: Box<str>) -> SourceFile {
        SourceFile {
            id,
            path,
            path_str,
            src,
            line_col_coords: OnceCell::new(),
        }
    }
    
    /// Loads a synthetic source file without an arena.
    pub(crate) fn new_anonymous_synthetic(src: &str) -> SourceFile {
        SourceFile::new(
            SourceFileID::ANONYMOUS,
            path::PathBuf::new().into_boxed_path(),
            Box::from("<anonymous>"),
            Box::from(src),
        )
    }
    
    pub(crate) fn get_span(&self, range: SourceRange) -> &str {
        if self.id != range.src_id { errors::ice_at("span does not belong to this source file", self, range); }
        &self.src[range.start as usize..range.end as usize]
    }
    
    /// Returns a span at the end of this source file. Used to report syntax
    /// errors where an unexpected end-of-file occurs.
    pub(crate) fn eof_range(&self) -> SourceRange {
        let end = self.src.len() as u32;
        SourceRange {src_id: self.id, start: end, end}
    }
    
    /// Converts an index in this source file to (line, col) numbers, used for
    /// reporting diagnostics.
    pub(crate) fn index_to_line_col(&self, index: u32) -> (u32, u32) {
        self.line_col_coords.get_or_init(|| {
            let mut coords = Vec::with_capacity(self.src.len() + 1);
            coords.push((1, 1));
            let mut line = 1;
            let mut col = 1;
            for c in self.src.chars() {
                if c == '\n' {
                    line += 1; col = 1;
                } else {
                    col += 1;
                }
                coords.push((line, col));
            }
            coords.into_boxed_slice()
        })[index as usize]
    }
}

pub(crate) struct SourceFileCache {
    files: Vec<Rc<SourceFile>>,
}

impl SourceFileCache {
    pub(crate) fn new() -> SourceFileCache {
        SourceFileCache {
            files: Vec::new(),
        }
    }
    
    pub(crate) fn get(&self, id: SourceFileID) -> Rc<SourceFile> {
        self.files[id.0.get() as usize].clone()
    }
    
    pub(crate) fn get_str(&self, range: SourceRange) -> &str {
        &self.files[range.src_id.0.get() as usize]
            .src[range.start as usize..range.end as usize]
    }
    
    fn next_id(&self) -> SourceFileID {
        SourceFileID::of(self.files.len() as u32)
    }
    
    /// Creates a new synthetic source file which does not exist on the file
    /// system. This is used for the standard library, syntax highlighting of
    /// Papyri snippets, and tests.
    pub(crate) fn load_synthetic(&mut self, path_str: &str, src: &str) -> Rc<SourceFile> {
        let s = Rc::new(SourceFile::new(
            self.next_id(),
            path::PathBuf::new().into_boxed_path(),
            Box::from(path_str),
            Box::from(src),
        ));
        self.files.push(s.clone());
        s
    }
    
    /// Loads a source file from the given path.
    pub(crate) fn load_from_path(&mut self, path: &path::Path) -> std::io::Result<Rc<SourceFile>> {
        let src = fs::read_to_string(path)?;
        let s = Rc::new(SourceFile::new(
            self.next_id(),
            Box::from(path),
            path.to_string_lossy().into(),
            src.into_boxed_str(),
        ));
        self.files.push(s.clone());
        Ok(s)
    }
}
