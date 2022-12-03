//! This module contains helper functions for text or string operations.

use std::rc::Rc;
use deunicode;
use htmlentity::entity;
use indexmap::IndexSet;

/// Indicates whether the given string is a valid name, matching `[a-zA-Z_][a-zA-Z0-9_]*`.
pub fn is_identifier(s: &str) -> bool {
    let mut s_chars = s.chars();
    matches!(s_chars.next(), Some('a'..='z' | 'A'..='Z' | '_'))
        && s_chars.all(|c| matches!(c, 'a'..='z' | 'A'..='Z' | '0'..='9' | '_'))
}

/// Returns a usually-reasonable approximation of the given string, using only
/// valid identifier characters. The result matches `[a-zA-Z_][a-zA-Z0-9_]*`
/// and has length at most `max_len` (which should be at least 1).
pub fn make_identifier(s: &str, max_len: usize) -> String {
    let s = deunicode::deunicode(s);
    let mut s_chars = s.chars();
    let mut id = "".to_string();
    match s_chars.next() {
        Some(c) if matches!(c, 'a'..='z' | 'A'..='Z') => id.push(c),
        _ => id.push('_'),
    }
    for c in s_chars {
        if id.len() >= max_len {
            break;
        } else if matches!(c, 'a'..='z' | 'A'..='Z' | '0'..='9') {
            id.push(c);
        } else if !id.ends_with("_") {
            id += "_";
        }
    }
    id
}

/// Indicates whether the given string contains any of the characters '*', '?',
/// '[' or ']', suggesting it may be a glob pattern.
pub fn looks_like_glob(s: &str) -> bool {
    s.contains(|c| matches!(c, '*' | '?' | '[' | ']'))
}

/// Indicates whether the given string is all ASCII whitespace. A non-breaking
/// space character is not ASCII whitespace.
pub fn is_whitespace(s: &str) -> bool {
    s.chars().all(|c| c.is_ascii_whitespace())
}

/// Returns either the empty string, or the string "s", to pluralise a word
/// given a quantity.
pub fn pluralise(quantity: u32) -> &'static str {
    if quantity == 1 { "" } else { "s" }
}

/// Encodes the characters `<`, `>` and `&` in a string as HTML entities. If
/// `escape_quotes` is true, the characters `'` and `"` are additionally encoded.
pub fn encode_entities(s: &str, escape_quotes: bool) -> String {
    entity::encode(
        s,
        if escape_quotes { entity::EntitySet::SpecialChars } else { entity::EntitySet::Html },
        entity::EncodeType::NamedOrHex,
    ).into_iter().collect()
}

/// Strips indentation from the start of each line of the given string. The
/// indentation of the first line with any non-whitespace characters is removed
/// from all lines. Leading and trailing whitespace of the whole string is also
/// removed.
pub fn fix_indentation(s: &str) -> String {
    let mut indentation_to_remove: Option<&str> = None;
    let mut out = "".to_string();
    for line in s.trim_end().lines() {
        match indentation_to_remove {
            Some(indentation) => {
                if line.starts_with(indentation) {
                    out += &line[indentation.len()..];
                } else {
                    out += &line.trim_start();
                }
                out += "\n";
            },
            None => {
                if let Some((index, _)) = line.chars().enumerate().find(|(_, c)| !c.is_whitespace()) {
                    // shortcut if there is no indentation
                    if index == 0 { return s.trim().to_string(); }
                    
                    indentation_to_remove = Some(&line[..index]);
                    out += &line[index..];
                    out += "\n";
                }
            },
        }
    }
    out
}

/// If the given string's first line is a valid identifier, then this function
/// returns a pair of that identifier and the remainder of the string with
/// indentation stripped. Otherwise, `None` is returned.
pub fn get_source_language_hint(src: &str) -> Option<(&str, &str)> {
    let k = src.find('\n')?;
    let first_line = src[..k].trim_end();
    if is_identifier(first_line) {
        Some((first_line, &src[k + 1..]))
    } else {
        None
    }
}

/// A generator of unique string IDs. It converts arbitrary strings into valid
/// identifiers; IDs generated will be distinct until the `clear()` method is
/// called.
pub struct UniqueIDGenerator {
    ids_used: IndexSet<Rc<str>>,
}

impl UniqueIDGenerator {
    /// Creates a new unique ID generator.
    pub fn new() -> UniqueIDGenerator {
        UniqueIDGenerator {
            ids_used: IndexSet::new(),
        }
    }
    
    /// Clears this generator, allowing previously-issued IDs to be reused.
    pub fn clear(&mut self) {
        self.ids_used.clear();
    }
    
    /// Returns a valid identifier based on the given `id_base` string, which
    /// distinct from all other identifiers returned by this generator since
    /// the last call to `clear()`.
    /// 
    /// The identifier is lowercase, and its length is at most `max_len` unless
    /// it is necessary to exceed that length to ensure uniqueness.
    pub fn get_unique_id(&mut self, id_base: &str, max_len: usize) -> Rc<str> {
        let mut id = if !is_identifier(&id_base) {
            make_identifier(&id_base, max_len)
        } else if id_base.len() > max_len {
            id_base[..max_len].to_string()
        } else {
            id_base.to_string()
        };
        id.make_ascii_lowercase();
        
        let id: Rc<str> = Rc::from(id);
        if self.ids_used.insert(id.clone()) { return id; }
        
        let mut id_base = id_base;
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
