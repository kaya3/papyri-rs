use std::rc::Rc;

use super::SourceFile;

#[derive(Clone)]
/// Represents a span of code in a Papyri source file. The source must be
/// smaller than 4GiB.
pub struct SourceRange {
    /// The Papyri source file which this span occurs in.
    pub src: Rc<SourceFile>,
    
    /// The index of the start of this span.
    pub start: u32,
    
    /// The index of the end of this span.
    pub end: u32,
}

impl SourceRange {
    /// Returns the length of this span.
    pub fn len(&self) -> u32 {
        self.end - self.start
    }
    
    /// Returns the source at this span.
    pub fn as_str(&self) -> &str {
        &self.src.src[self.start as usize..self.end as usize]
    }
    
    /// Returns a new span, starting at the same position as this one, with a
    /// new end index.
    pub fn to_end(&self, end: u32) -> SourceRange {
        SourceRange {
            src: self.src.clone(),
            start: self.start,
            end,
        }
    }
    
    /// Returns a new span, starting at the end of this one, with a new end
    /// index.
    pub fn from_end(&self, end: u32) -> SourceRange {
        SourceRange {
            src: self.src.clone(),
            start: self.end,
            end,
        }
    }
    
    /// Returns a new span, with a starting index offset relative to this
    /// one's, ending at the same position as this one, 
    pub fn offset_start(&self, offset: u32) -> SourceRange {
        SourceRange {
            src: self.src.clone(),
            start: self.start + offset,
            end: self.end,
        }
    }
    
    /// Converts the starting position of this span to a descriptive string, in
    /// the form "line Y, col X".
    pub fn str_start(&self) -> String {
        pos_to_string(self.src.index_to_line_col(self.start))
    }
    
    /// Converts the ending position of this span to a descriptive string, in
    /// the form "line Y, col X".
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
