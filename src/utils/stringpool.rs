use indexmap::IndexSet;

use super::const_strs::CONST_STRS;
use super::diagnostics::ice;

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct NameID(pub u32);

impl NameID {
    pub fn is_anonymous(self) -> bool {
        self.0 == 0
    }
}

pub struct StringPool(IndexSet<Box<str>>);

impl StringPool {
    pub fn new() -> StringPool {
        let strings = CONST_STRS.iter()
            .map(|s| Box::from(*s));
        
        StringPool(IndexSet::from_iter(strings))
    }
    
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
    
    pub fn get(&self, id: NameID) -> &str {
        self.0.get_index(id.0 as usize)
            .unwrap_or_else(|| ice(&format!("no string with ID {}", id.0)))
            .as_ref()
    }
}
