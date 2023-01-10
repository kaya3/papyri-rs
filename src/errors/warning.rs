#[allow(missing_docs)]
#[derive(Debug)]
/// Represents a warning which occurs during compilation of a Papyri source
/// file.
pub enum Warning {
    RedundantOptionType,
    NameAlreadyDeclared(std::rc::Rc<str>),
    PatternNameAlreadyBound(std::rc::Rc<str>),
    NameAlreadyExported(std::rc::Rc<str>),
    InlineHighlightEnumerate,
    InlineHighlightMultiline,
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
            Warning::InlineHighlightEnumerate => f.write_str("cannot enumerate lines in inline code"),
            Warning::InlineHighlightMultiline => f.write_str("inline code cannot be multiple lines"),
            Warning::HighlightNotEnabled => f.write_str("syntax highlighting is not enabled in this build"),
            Warning::HighlightLanguageUnknown(language) => write!(f, "no syntax highlighter found for language \"{language}\""),
            Warning::BrokenLink(path) => write!(f, "linked file does not exist at \"{path}\""),
        }
    }
}
