pub enum NameError {
    NoSuchVariable(String),
    NoSuchParameter(String),
    
    InvalidTag(std::rc::Rc<str>),
}

pub enum RuntimeError {
    ParamMissing(String),
    ParamMissingImplicit(String),
    ParamMultipleValues(String),
    
    AttrMultipleValues(String),
    
    Raised(std::rc::Rc<str>),
}

impl std::fmt::Display for NameError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            NameError::NoSuchVariable(name) => write!(f, "no such variable '{name}'"),
            NameError::NoSuchParameter(name) => write!(f, "no such parameter '{name}'"),
            NameError::InvalidTag(name) => write!(f, "invalid tag name '{name}'"),
        }
    }
}

impl std::fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RuntimeError::ParamMissing(name) => write!(f, "missing required parameter '{name}'"),
            RuntimeError::ParamMissingImplicit(name) => write!(f, "missing required implicit parameter '{name}'"),
            RuntimeError::ParamMultipleValues(name) => write!(f, "received multiple values for parameter '{name}'"),
            RuntimeError::AttrMultipleValues(name) => write!(f, "received multiple values for attribute '{name}'"),
            RuntimeError::Raised(msg) => f.write_str(msg),
        }
    }
}
