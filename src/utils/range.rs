use std::rc::Rc;

use super::SourceFile;

#[derive(Clone)]
/// Represents a span of code in a Papyri source file. The source must be
/// smaller than 4GiB.
pub struct SourceRange {
    /// The Papyri source file which this span occurs in.
    pub(crate) src: Rc<SourceFile>,
    
    /// The index of the start of this span.
    pub(crate) start: u32,
    
    /// The index of the end of this span.
    pub(crate) end: u32,
}

impl SourceRange {
    /// Returns the source at this span.
    pub(crate) fn as_str(&self) -> &str {
        &self.src.src[self.start as usize..self.end as usize]
    }
    
    /// Returns a new span, starting at the same position as this one, with a
    /// new end index.
    pub(crate) fn to_end(&self, end: u32) -> SourceRange {
        SourceRange {
            src: self.src.clone(),
            start: self.start,
            end,
        }
    }
    
    /*
    /// Returns a new span, starting at the end of this one, with a new end
    /// index.
    pub(crate) fn from_end(&self, end: u32) -> SourceRange {
        SourceRange {
            src: self.src.clone(),
            start: self.end,
            end,
        }
    }*/
    
    /// Converts the starting position of this span to a descriptive string, in
    /// the form "line Y, col X".
    pub(crate) fn str_start(&self) -> String {
        pos_to_string(self.src.index_to_line_col(self.start))
    }
    
    /// Converts the ending position of this span to a descriptive string, in
    /// the form "line Y, col X".
    pub(crate) fn str_end(&self) -> String {
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
