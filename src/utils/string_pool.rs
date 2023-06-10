use std::rc::Rc;
use indexmap::{IndexSet, IndexMap};
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

/// A set of NameIDs.
pub type NameIDMap<T> = IndexMap<NameID, T, fxhash::FxBuildHasher>;

/// A pool of interned string names, which assigns unique IDs to names, and can
/// be used to look up names by ID. The pool's maximum capacity is `u32::MAX - 1`.
pub struct StringPool(IndexSet<Rc<str>, fxhash::FxBuildHasher>);

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
            .copied()
            .map(Rc::from);
        
        StringPool(IndexSet::from_iter(strings))
    }
    
    /// Inserts a name into this pool if it is not already present, and returns
    /// its unique ID.
    pub(crate) fn insert<T>(&mut self, s: T) -> NameID
    where T: Into<Rc<str>> + AsRef<str> {
        match self.0.get_index_of(s.as_ref()) {
            Some(id) => NameID::of(id as u32),
            None => {
                let (id, _) = self.0.insert_full(s.into());
                NameID::of(id as u32)
            },
        }
    }
    
    /// Inserts a name as lowercase into this pool if it is not already present,
    /// and returns its unique ID.
    pub(crate) fn insert_lowercase<T>(&mut self, s: T) -> NameID
    where T: AsRef<str> {
        self.insert(s.as_ref().to_lowercase())
    }
    
    pub(crate) fn get_id_if_present(&self, s: &str) -> Option<NameID> {
        self.0.get_index_of(s)
            .map(|id| NameID::of(id as u32))
    }
    
    /// Returns the name associated with the given ID, as a string. Should only
    /// be called with IDs assigned by this pool, or constant IDs assigned in
    /// the `str_ids` module.
    pub(crate) fn get(&self, id: NameID) -> Rc<str> {
        let id = id.0.get() as usize;
        self.0.get_index(id)
            .unwrap_or_else(|| errors::ice(&format!("No string with ID {id}")))
            .clone()
    }
}
