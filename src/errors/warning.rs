#[allow(missing_docs)]
/// Represents a warning which occurs during compilation of a Papyri source
/// file.
pub enum Warning {
    RedundantOptionType,
    NameAlreadyDeclared(std::rc::Rc<str>),
    PatternNameAlreadyBound(std::rc::Rc<str>),
    NameAlreadyExported(std::rc::Rc<str>),
}

#[allow(missing_docs)]
/// Represents a warning which occurs at runtime; there is an associated stack
/// trace.
pub enum RuntimeWarning {
    NameNotImplicit(std::rc::Rc<str>),
    InlineHighlightEnumerate,
    InlineHighlightMultiline,
    NoMatchingBranch,
    HighlightNotEnabled,
    HighlightLanguageUnknown(std::rc::Rc<str>),
    BrokenLink(std::rc::Rc<str>),
}

impl std::fmt::Display for Warning {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Warning::RedundantOptionType => f.write_str("type modifier '?' is redundant here"),
            Warning::NameAlreadyDeclared(name) => write!(f, "name '{name}' already declared"),
            Warning::PatternNameAlreadyBound(name) => write!(f, "name '{name}' already bound in this pattern; did you mean '=${name}'?"),
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
            RuntimeWarning::NoMatchingBranch => f.write_str("no matching branch in @match"),
            RuntimeWarning::HighlightNotEnabled => f.write_str("syntax highlighting is not enabled in this build"),
            RuntimeWarning::HighlightLanguageUnknown(language) => write!(f, "no syntax highlighter found for language \"{language}\""),
            RuntimeWarning::BrokenLink(path) => write!(f, "linked file does not exist at \"{path}\""),
        }
    }
}
