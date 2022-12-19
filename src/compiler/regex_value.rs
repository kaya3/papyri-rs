use regex::Regex;

use crate::utils::{SourceRange, NameID, text};
use crate::errors;
use super::base::Compiler;
use super::value::{Value, ValueMap};

#[derive(Debug)]
enum RegexKind {
    NoGroups,
    NumberedGroups,
    NamedGroups(Box<[NameID]>),
}

impl RegexKind {
    fn captures_to_value(&self, m: regex::Captures) -> Value {
        match self {
            RegexKind::NoGroups => {
                m.get(0).map_or(
                    Value::UNIT,
                    |s| s.as_str().into(),
                )
            },
            RegexKind::NumberedGroups => {
                m.iter()
                    .map(|g| g.map_or(Value::UNIT, |s| s.as_str().into()))
                    .collect::<Vec<_>>()
                    .into()
            },
            RegexKind::NamedGroups(name_ids) => {
                let v = m.iter()
                    .skip(1)
                    .map(|g| g.map_or(Value::UNIT, |s| s.as_str().into()));
                ValueMap::from_iter(name_ids.iter().copied().zip(v))
                    .into()
            },
        }
    }
}

#[derive(Debug)]
pub struct RegexValue {
    regex: Regex,
    kind: RegexKind,
}

impl RegexValue {
    pub(super) fn as_str(&self) -> &str {
        self.regex.as_str()
    }
    
    pub(super) fn find(&self, s: &str) -> Value {
        self.regex.captures(s).map_or(
            Value::UNIT,
            |m| self.kind.captures_to_value(m),
        )
    }
    
    pub(super) fn find_all(&self, s: &str) -> Value {
        self.regex.captures_iter(s)
            .map(|m| self.kind.captures_to_value(m))
            .collect::<Vec<_>>()
            .into()
    }
}

impl <'a> Compiler<'a> {
    pub(super) fn compile_regex(&mut self, regex_str: &str, range: &SourceRange) -> Option<RegexValue> {
        let regex = Regex::new(regex_str)
            .map_err(|e| self.runtime_error(errors::RuntimeError::RegexSyntaxError(e), range))
            .ok()?;
        
        let kind = if regex.captures_len() == 1 {
            RegexKind::NoGroups
        } else if regex.capture_names().skip(1).all(|n| n.is_none()) {
            RegexKind::NumberedGroups
        } else if regex.capture_names().skip(1).all(|n| n.is_some()) {
            let names = regex.capture_names()
                .skip(1)
                .map(|name| {
                    let name = name.unwrap();
                    if !text::is_identifier(name) {
                        self.runtime_error(errors::RuntimeError::RegexInvalidGroupName(name.to_string()), range);
                    }
                    self.ctx.string_pool.insert(name)
                })
                .collect();
            RegexKind::NamedGroups(names)
        } else {
            self.runtime_error(errors::RuntimeError::RegexMixedGroupKinds, range);
            return None;
        };
        
        Some(RegexValue {regex, kind})
    }
}
