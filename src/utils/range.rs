use std::rc::Rc;

use super::SourceFile;

#[derive(Clone)]
pub struct SourceRange {
    pub src: Rc<SourceFile>,
    pub start: u32,
    pub end: u32,
}

impl SourceRange {
    pub fn len(&self) -> u32 {
        self.end - self.start
    }
    
    pub fn as_str(&self) -> &str {
        &self.src.src[self.start as usize..self.end as usize]
    }
    
    pub fn to_end(&self, end: u32) -> SourceRange {
        SourceRange {
            src: self.src.clone(),
            start: self.start,
            end,
        }
    }
    
    pub fn str_start(&self) -> String {
        pos_to_string(self.src.index_to_line_col(self.start))
    }
    
    pub fn str_end(&self) -> String {
        pos_to_string(self.src.index_to_line_col(self.end))
    }
}

fn pos_to_string(pos: (u32, u32)) -> String {
    format!("line {}, col {}", pos.0, pos.1)
}

impl std::fmt::Debug for SourceRange {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} to {}", self.str_start(), self.str_end())
    }
}
