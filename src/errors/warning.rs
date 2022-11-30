#[allow(missing_docs)]
/// Represents a warning which occurs during compilation of a Papyri source
/// file.
pub enum Warning {
    AnonymousFunctionInText,
    NameAlreadyDeclared(String),
    NoMatchingBranch,
    PatternNameAlreadyBound(String),
    NameAlreadyExported(String),
}

#[allow(missing_docs)]
/// Represents a warning which occurs at runtime; there is an associated stack
/// trace.
pub enum RuntimeWarning {
    NameNotImplicit(String),
    InlineHighlightEnumerate,
    InlineHighlightMultiline,
    BrokenLink(std::rc::Rc<str>),
}

impl std::fmt::Display for Warning {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Warning::AnonymousFunctionInText => f.write_str("anonymous function not expected here"),
            Warning::NameAlreadyDeclared(name) => write!(f, "name '{name}' already declared"),
            Warning::NoMatchingBranch => f.write_str("no matching branch in @match"),
            Warning::PatternNameAlreadyBound(name) => write!(f, "name '{name}' already bound in this pattern"),
            Warning::NameAlreadyExported(name) => write!(f, "name '{name}' already exported"),
        }
    }
}

impl std::fmt::Display for RuntimeWarning {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RuntimeWarning::NameNotImplicit(name) => write!(f, "name '{name}' exists but is not declared as implicit"),
            RuntimeWarning::InlineHighlightEnumerate => f.write_str("cannot enumerate lines in inline code"),
            RuntimeWarning::InlineHighlightMultiline => f.write_str("inline code cannot be multiple lines"),
            RuntimeWarning::BrokenLink(path) => write!(f, "linked file does not exist at \"{path}\""),
        }
    }
}
