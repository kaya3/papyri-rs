use std::fs;
use std::path::PathBuf;
use std::rc::Rc;
use once_cell::unsync::OnceCell;

use super::SourceRange;

pub fn has_papyri_extension(path: &PathBuf) -> bool {
    matches!(path.extension(), Some(s) if s.to_string_lossy() == "papyri")
}

pub struct SourceFile {
    pub path_str: String,
    pub src: Box<str>,
    line_col_coords: OnceCell<Box<[(u32, u32)]>>,
}

impl SourceFile {
    pub fn synthetic(path_str: &str, src: &str) -> Rc<SourceFile> {
        Rc::new(SourceFile {
            path_str: path_str.to_string(),
            src: Box::from(src),
            line_col_coords: OnceCell::new(),
        })
    }
    
    pub fn from_path(path: &PathBuf) -> Result<Rc<SourceFile>, String> {
        SourceFile::new(path, String::from(path.to_string_lossy()))
    }
    
    fn new(path: &PathBuf, path_str: String) -> Result<Rc<SourceFile>, String> {
        let src = fs::read_to_string(&path)
            .map_err(|e| e.to_string())?;
        Ok(Rc::new(SourceFile {
            path_str,
            src: Box::from(src),
            line_col_coords: OnceCell::new(),
        }))
    }
    
    pub fn eof_range(src: Rc<SourceFile>) -> SourceRange {
        let end = src.src.len() as u32;
        SourceRange {src, start: end, end}
    }
    
    pub fn index_to_line_col(&self, index: u32) -> (u32, u32) {
        self.line_col_coords.get_or_init(|| {
            let mut coords = Vec::new();
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
