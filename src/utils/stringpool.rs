use indexmap::IndexSet;

use super::const_strs::CONST_STRS;
use super::diagnostics::ice;

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
/// Represents an interned name. Two names in the same `StringPool` will have
/// the same ID if and only if they are equal as strings.
pub struct NameID(pub u32);

impl NameID {
    /// Indicates whether this name ID represents the absence of a name.
    pub fn is_anonymous(self) -> bool {
        self.0 == 0
    }
}

/// A pool of interned string names, which assigns unique IDs to names, and can
/// be used to look up names by ID. The pool's maximum capacity is `u32::MAX`.
pub struct StringPool(IndexSet<Box<str>>);

impl StringPool {
    /// Creates a new pool, containing the names which are always pooled; see
    /// `const_strs.rs` for a full list.
    pub fn new() -> StringPool {
        let strings = CONST_STRS.iter()
            .map(|s| Box::from(*s));
        
        StringPool(IndexSet::from_iter(strings))
    }
    
    /// Inserts a name into this pool if it is not already present, and returns
    /// its unique ID.
    pub fn insert(&mut self, s: &str) -> NameID {
        match self.0.get_index_of(s) {
            Some(id) => NameID(id as u32),
            None => {
                let s_owned: Box<str> = Box::from(s);
                let (id, _) = self.0.insert_full(s_owned);
                NameID(id as u32)
            },
        }
    }
    
    /// Returns the name associated with the given ID, as a string. Should only
    /// be called with IDs assigned by this pool, or constant IDs assigned in
    /// the `str_ids` module.
    pub fn get(&self, id: NameID) -> &str {
        let Some(s) = self.0.get_index(id.0 as usize) else {
            ice(&format!("no string with ID {}", id.0));
        };
        s.as_ref()
    }
}
