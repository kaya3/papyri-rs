//! This module contains helper functions for text or string operations.

use htmlentity::entity;

/// Indicates whether the given string is a valid name, matching `[a-zA-Z_][a-zA-Z0-9_]*`.
pub fn is_identifier(s: &str) -> bool {
    let mut s_chars = s.chars();
    matches!(s_chars.next(), Some('a'..='z' | 'A'..='Z' | '_'))
        && s_chars.all(|c| matches!(c, 'a'..='z' | 'A'..='Z' | '0'..='9' | '_'))
}

/// Indicates whether the given string contains any of the characters '*', '?',
/// '[' or ']', suggesting it may be a glob pattern.
pub fn looks_like_glob(s: &str) -> bool {
    s.contains(|c| matches!(c, '*' | '?' | '[' | ']'))
}

/// Indicates whether the given string is all whitespace.
pub fn is_whitespace(s: &str) -> bool {
    s.chars().all(char::is_whitespace)
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
                    indentation_to_remove = Some(&line[..index]);
                    out += &line[index..];
                    out += "\n";
                }
            },
        }
    }
    out
}
