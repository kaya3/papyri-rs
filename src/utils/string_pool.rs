use indexmap::IndexSet;
use nonmax::NonMaxU32;

use crate::errors;
use super::const_strs::CONST_STRS;
use super::str_ids;

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
/// Represents an interned name. Two names in the same `StringPool` will have
/// the same ID if and only if they are equal as strings.
pub struct NameID(NonMaxU32);

impl NameID {
    pub(super) const fn of(id: u32) -> NameID {
        let Some(id) = NonMaxU32::new(id) else { panic!() };
        NameID(id)
    }
    
    /// Indicates whether this name ID represents the absence of a name.
    pub fn is_anonymous(self) -> bool {
        self == str_ids::ANONYMOUS
    }
}

/// A set of NameIDs.
pub type NameIDSet = IndexSet<NameID, fxhash::FxBuildHasher>;

/// A pool of interned string names, which assigns unique IDs to names, and can
/// be used to look up names by ID. The pool's maximum capacity is `u32::MAX`.
pub struct StringPool(IndexSet<Box<str>, fxhash::FxBuildHasher>);

impl Default for StringPool {
    fn default() -> StringPool {
        StringPool::new()
    }
}

impl StringPool {
    /// Creates a new pool, containing the names which are always pooled; see
    /// `const_strs.rs` for a full list.
    pub(crate) fn new() -> StringPool {
        let strings = CONST_STRS.iter()
            .map(|&s| Box::from(s));
        
        StringPool(IndexSet::from_iter(strings))
    }
    
    /// Inserts a name into this pool if it is not already present, and returns
    /// its unique ID.
    pub(crate) fn insert(&mut self, s: &str) -> NameID {
        match self.0.get_index_of(s) {
            Some(id) => NameID::of(id as u32),
            None => {
                let s_owned: Box<str> = Box::from(s);
                let (id, _) = self.0.insert_full(s_owned);
                NameID::of(id as u32)
            },
        }
    }
    
    /// Returns the name associated with the given ID, as a string. Should only
    /// be called with IDs assigned by this pool, or constant IDs assigned in
    /// the `str_ids` module.
    pub fn get(&self, id: NameID) -> &str {
        let id = id.0.get() as usize;
        let Some(s) = self.0.get_index(id) else {
            errors::ice(&format!("no string with ID {id}"));
        };
        s
    }
}
