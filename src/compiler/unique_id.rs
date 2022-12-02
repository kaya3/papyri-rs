use std::rc::Rc;
use indexmap::IndexSet;

use crate::utils::text;

pub struct UniqueIDGenerator {
    ids_used: IndexSet<Rc<str>>,
}

impl UniqueIDGenerator {
    pub fn new() -> UniqueIDGenerator {
        UniqueIDGenerator {
            ids_used: IndexSet::new(),
        }
    }
    
    pub fn clear(&mut self) {
        self.ids_used.clear();
    }
    
    /// Returns a valid identifier based on the given `id_base` string, which
    /// is distinct from all other identifiers returned by this generator since
    /// the last call to `clear()`.
    /// 
    /// The identifier's length is at most `max_len` unless it is necessary to
    /// exceed that length in order to ensure uniqueness.
    pub fn get_unique_id(&mut self, id_base: Rc<str>, max_len: usize) -> Rc<str> {
        let id = if !text::is_identifier(&id_base) {
            Rc::from(text::make_identifier(&id_base, max_len))
        } else if id_base.len() > max_len {
            Rc::from(&id_base[..max_len])
        } else {
            id_base.clone()
        };
        
        if self.ids_used.insert(id.clone()) { return id; }
        
        let mut id_base = id_base.as_ref();
        if id_base.len() + 2 > max_len && max_len >= 3 {
            id_base = &id_base[..max_len - 2];
        }
        
        let mut counter = 2;
        let id: Rc<str> = loop {
            let id = format!("{id_base}_{counter}");
            if !self.ids_used.contains(id.as_str()) {
                break Rc::from(id);
            }
            
            counter += 1;
            // check if we're about to overflow
            if id.len() >= max_len && id_base.len() > 1 && id.trim_end_matches('9').ends_with('_') {
                id_base = &id_base[..id_base.len() - 1];
            }
        };
        
        self.ids_used.insert(id.clone());
        id
    }
}
