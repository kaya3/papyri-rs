use std::rc::Rc;

use crate::utils::{NameID, text};
use crate::errors::{RuntimeError, PapyriResult};
use super::base::Compiler;
use super::value::{Value, Dict};

#[derive(Debug)]
enum RegexKind {
    Simple,
    NumberedGroups,
    NamedGroups(Box<[NameID]>),
}

impl RegexKind {
    /// Converts the regex capture groups to a value - either a string, list or
    /// dict depending on this regex's kind.
    fn captures_to_value(&self, m: regex::Captures) -> Value {
        match self {
            RegexKind::Simple => {
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
                Dict::from_iter(name_ids.iter().copied().zip(v))
                    .into()
            },
        }
    }
}

#[derive(Debug)]
pub struct RegexValue {
    regex: regex::Regex,
    kind: RegexKind,
}
pub(super) type RcRegex = Rc<RegexValue>;

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
    
    pub(super) fn find_all(&self, s: &str) -> Vec<Value> {
        self.regex.captures_iter(s)
            .map(|m| self.kind.captures_to_value(m))
            .collect()
    }
    
    pub(super) fn test(&self, s: &str) -> bool {
        self.regex.is_match(s)
    }
    
    pub(super) fn count(&self, s: &str) -> usize {
        self.regex.find_iter(s)
            .count()
    }
    
    pub(super) fn split(&self, s: &str) -> Vec<Value> {
        self.regex.split(s)
            .into_iter()
            .map(Value::from)
            .collect()
    }
}

impl <'a> Compiler<'a> {
    pub(super) fn compile_regex(&mut self, regex_str: &str) -> PapyriResult<RcRegex> {
        let regex = regex::Regex::new(regex_str)
            .map_err(RuntimeError::RegexSyntaxError)?;
        
        let kind = if regex.captures_len() == 1 {
            RegexKind::Simple
        } else if regex.capture_names().skip(1).all(|n| n.is_none()) {
            RegexKind::NumberedGroups
        } else if regex.capture_names().skip(1).all(|n| n.is_some()) {
            let mut names = Vec::with_capacity(regex.captures_len() - 1);
            for name in regex.capture_names().skip(1) {
                let name = name.unwrap();
                if !text::is_identifier(name) {
                    let e = RuntimeError::RegexInvalidGroupName(name.into());
                    return Err(e.into());
                }
                names.push(self.ctx.string_pool.insert(name));
            }
            RegexKind::NamedGroups(names.into_boxed_slice())
        } else {
            let e = RuntimeError::RegexMixedGroupKinds;
            return Err(e.into());
        };
        
        Ok(Rc::new(RegexValue {regex, kind}))
    }
}
