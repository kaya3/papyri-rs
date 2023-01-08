use crate::parser::Type;

#[derive(Debug)]
#[allow(missing_docs)]
/// Represents an error which occurs at runtime due to an undefined or invalid
/// name; there is an associated stack trace.
pub enum NameError {
    NoSuchVariable(std::rc::Rc<str>),
    NoSuchParameter(std::rc::Rc<str>),
    NoSuchAttribute(Type, std::rc::Rc<str>),
    
    InvalidTag(std::rc::Rc<str>),
}

#[derive(Debug)]
#[allow(missing_docs)]
/// Represents an error which occurs at runtime; there is an associated stack
/// trace.
pub enum RuntimeError {
    AttrMultipleValues(std::rc::Rc<str>),
    
    ParamMissing(std::rc::Rc<str>),
    ParamMissingImplicit(std::rc::Rc<str>),
    ParamExistsButNotImplicit(std::rc::Rc<str>),
    ParamMultipleValues(std::rc::Rc<str>),
    ParamMustBePositive(std::rc::Rc<str>, i64),
    
    RegexSyntaxError(regex::Error),
    RegexMixedGroupKinds,
    RegexInvalidGroupName(std::rc::Rc<str>),
    
    Raised(std::rc::Rc<str>),
    NoMatchingBranch,
    IndexOutOfRange(i64, usize),
    ParseIntError(std::num::ParseIntError),
    FileReadError(std::rc::Rc<str>, std::io::Error),
    PathNotInOutDir(std::rc::Rc<str>),
    WriteFileNotAllowed,
}

impl std::fmt::Display for NameError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            NameError::NoSuchVariable(name) => write!(f, "no such variable '{name}'"),
            NameError::NoSuchParameter(name) => write!(f, "no such parameter '{name}'"),
            NameError::NoSuchAttribute(type_, name) => write!(f, "value of type '{type_}' has no such attribute '{name}'"),
            NameError::InvalidTag(name) => write!(f, "invalid tag name '{name}'"),
        }
    }
}

impl std::fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RuntimeError::AttrMultipleValues(name) => write!(f, "received multiple values for attribute '{name}'"),
            RuntimeError::ParamMissing(name) => write!(f, "missing required parameter '{name}'"),
            RuntimeError::ParamMissingImplicit(name) => write!(f, "missing required implicit parameter '{name}'"),
            RuntimeError::ParamExistsButNotImplicit(name) => write!(f, "required implicit parameter '{name}'; this name exists but is not declared implicit"),
            RuntimeError::ParamMultipleValues(name) => write!(f, "received multiple values for parameter '{name}'"),
            RuntimeError::ParamMustBePositive(name, was) => write!(f, "parameter '{name}' must be positive (was {was})"),
            RuntimeError::RegexSyntaxError(e) => write!(f, "regex syntax error ({e})"),
            RuntimeError::RegexMixedGroupKinds => f.write_str("regex cannot have both named and unnamed capture groups"),
            RuntimeError::RegexInvalidGroupName(name) => write!(f, "regex group name '{name}' is not a valid identifier"),
            RuntimeError::Raised(msg) => f.write_str(msg),
            RuntimeError::NoMatchingBranch => f.write_str("no matching branch in @match"),
            RuntimeError::IndexOutOfRange(i, len) => write!(f, "index out of bounds (index {i}, length {len})"),
            RuntimeError::ParseIntError(e) => write!(f, "failed to parse int ({e})"),
            RuntimeError::FileReadError(path, e) => write!(f, "failed to read file \"{path}\" ({e})"),
            RuntimeError::PathNotInOutDir(path) => write!(f, "path \"{path}\" is not within output directory"),
            RuntimeError::WriteFileNotAllowed => f.write_str("no output directory for '@file::write'; use '--out'"),
        }
    }
}
