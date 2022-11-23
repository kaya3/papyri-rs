use std::fs;
use std::path::{PathBuf, Path};
use std::rc::Rc;
use once_cell::unsync::OnceCell;

use super::range::SourceRange;

/// Indicates whether the given file path has a `.papyri` file extension.
pub fn has_papyri_extension(path: &PathBuf) -> bool {
    matches!(path.extension(), Some(s) if s.to_string_lossy().to_ascii_lowercase() == "papyri")
}

/// Represents a Papyri source file. The source as a string must be smaller
/// than 4 GiB.
pub struct SourceFile {
    pub path: Box<Path>,
    pub path_str: Box<str>,
    pub src: Box<str>,
    line_col_coords: OnceCell<Box<[(u32, u32)]>>,
}

impl SourceFile {
    /// Creates a new synthetic source file which does not exist on the file
    /// system. This is used for the standard library, syntax highlighting of
    /// Papyri snippets, and tests.
    pub fn synthetic(path_str: &str, src: &str) -> Rc<SourceFile> {
        SourceFile::new(PathBuf::new(), Box::from(path_str), Box::from(src))
    }
    
    /// Loads a source file from the given path.
    pub fn from_path(path: PathBuf) -> Result<Rc<SourceFile>, String> {
        fs::read_to_string(&path)
            .map(|src| {
                let path_str: Box<str> = Box::from(path.to_string_lossy());
                SourceFile::new(path, path_str, src.into_boxed_str())
            })
            .map_err(|e| e.to_string())
    }
    
    fn new(path: PathBuf, path_str: Box<str>, src: Box<str>) -> Rc<SourceFile> {
        Rc::new(SourceFile {
            path: path.into_boxed_path(),
            path_str,
            src,
            line_col_coords: OnceCell::new(),
        })
    }
    
    /// Returns a span at the end of this source file. Used to report syntax
    /// errors where an unexpected end-of-file occurs.
    pub fn eof_range(src: Rc<SourceFile>) -> SourceRange {
        let end = src.src.len() as u32;
        SourceRange {src, start: end, end}
    }
    
    /// Converts an index in this source file to (line, col) numbers, used for
    /// reporting diagnostics.
    pub fn index_to_line_col(&self, index: u32) -> (u32, u32) {
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
