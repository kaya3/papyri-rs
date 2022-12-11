use std::{fs, path};
use std::rc::Rc;
use once_cell::unsync::OnceCell;

use super::range::SourceRange;

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

/// Represents a Papyri source file. The source as a string must be smaller
/// than 4 GiB.
pub struct SourceFile {
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
    /// Creates a new synthetic source file which does not exist on the file
    /// system. This is used for the standard library, syntax highlighting of
    /// Papyri snippets, and tests.
    pub(crate) fn synthetic(path_str: &str, src: &str) -> Rc<SourceFile> {
        SourceFile::new(
            path::PathBuf::new().into_boxed_path(),
            format!("<{path_str}>").into_boxed_str(),
            Box::from(src),
        )
    }
    
    /// Loads a source file from the given path.
    pub(crate) fn from_path(path: &path::Path) -> Result<Rc<SourceFile>, std::io::Error> {
        fs::read_to_string(path)
            .map(|src| {
                let path_str: Box<str> = path.to_string_lossy().into();
                SourceFile::new(Box::from(path), path_str, src.into_boxed_str())
            })
    }
    
    fn new(path: Box<path::Path>, path_str: Box<str>, src: Box<str>) -> Rc<SourceFile> {
        Rc::new(SourceFile {
            path,
            path_str,
            src,
            line_col_coords: OnceCell::new(),
        })
    }
    
    /// Converts an index in this source file to (line, col) numbers, used for
    /// reporting diagnostics.
    pub(crate) fn index_to_line_col(&self, index: u32) -> (u32, u32) {
        self.line_col_coords.get_or_init(|| {
            // upper bound for the needed capacity
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

impl SourceRange {
    /// Returns a span at the end of this source file. Used to report syntax
    /// errors where an unexpected end-of-file occurs.
    pub(crate) fn eof(src: Rc<SourceFile>) -> SourceRange {
        let end = src.src.len() as u32;
        SourceRange {src, start: end, end}
    }
}
