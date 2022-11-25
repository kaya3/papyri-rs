pub enum Warning {
    AnonymousFunctionInText,
    NameAlreadyDeclared(String),
    NameNotImplicit(String),
    NoMatchingBranch,
    PatternNameAlreadyBound(String),
    NameAlreadyExported(String),
    InlineHighlightEnumerate,
    InlineHighlightMultiline,
}

impl std::fmt::Display for Warning {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Warning::AnonymousFunctionInText => f.write_str("anonymous function not expected here"),
            Warning::NameAlreadyDeclared(name) => write!(f, "name '{name}' already declared"),
            Warning::NameNotImplicit(name) => write!(f, "name '{name}' exists but is not declared as implicit"),
            Warning::NoMatchingBranch => f.write_str("no matching branch in @match"),
            Warning::PatternNameAlreadyBound(name) => write!(f, "name '{name}' already bound in this pattern"),
            Warning::NameAlreadyExported(name) => write!(f, "name '{name}' already exported"),
            Warning::InlineHighlightEnumerate => f.write_str("cannot enumerate lines in inline code"),
            Warning::InlineHighlightMultiline => f.write_str("inline code cannot be multiple lines"),
        }
    }
}
