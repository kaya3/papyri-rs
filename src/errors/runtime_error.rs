#[derive(Debug)]
#[allow(missing_docs)]
/// Represents an error which occurs at runtime due to an undefined or invalid
/// name; there is an associated stack trace.
pub enum NameError {
    NoSuchVariable(String),
    NoSuchParameter(String),
    NoSuchAttribute(String),
    
    InvalidTag(std::rc::Rc<str>),
}

#[derive(Debug)]
#[allow(missing_docs)]
/// Represents an error which occurs at runtime; there is an associated stack
/// trace.
pub enum RuntimeError {
    AttrMultipleValues(String),
    
    ParamMissing(String),
    ParamMissingImplicit(String),
    ParamMultipleValues(String),
    ParamMustBePositive(String, i64),
    
    Raised(std::rc::Rc<str>),
    PathNotInOutDir(std::rc::Rc<str>),
    WriteFileNotAllowed,
}

impl std::fmt::Display for NameError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            NameError::NoSuchVariable(name) => write!(f, "no such variable '{name}'"),
            NameError::NoSuchParameter(name) => write!(f, "no such parameter '{name}'"),
            NameError::NoSuchAttribute(name) => write!(f, "no such attribute '{name}'"),
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
            RuntimeError::ParamMultipleValues(name) => write!(f, "received multiple values for parameter '{name}'"),
            RuntimeError::ParamMustBePositive(name, was) => write!(f, "parameter '{name}' must be positive (was {was})"),
            RuntimeError::Raised(msg) => f.write_str(msg),
            RuntimeError::PathNotInOutDir(path) => write!(f, "path \"{path}\" is not within output directory"),
            RuntimeError::WriteFileNotAllowed => f.write_str("no output directory for '@write_file'; use '--out'"),
        }
    }
}
