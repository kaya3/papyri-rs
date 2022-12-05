use crate::compiler::Type;

#[allow(missing_docs)]
/// Represents a type error which occurs during compilation of a Papyri source
/// file.
pub enum TypeError {
    ExpectedWas(Type, Type),
    TooManyPositionalArgs(usize, usize),
    NotEnoughPositionalArgs(usize, usize),
    TagNotAllowed(String),
    InlineParagraphBreak,
    NoContentAllowed,
    SortKeyInvalid(Type),
    SortKeyHeterogeneous,
}

impl std::fmt::Display for TypeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeError::ExpectedWas(expected, was) => write!(f, "expected {expected}, was {was}"),
            TypeError::TooManyPositionalArgs(expected, was) => write!(f, "too many positional arguments (expected {expected}, was {was})"),
            TypeError::NotEnoughPositionalArgs(expected, was) => write!(f, "not enough positional arguments (expected {expected}, was {was})"),
            TypeError::TagNotAllowed(tag_name) => write!(f, "'{tag_name}' tag not allowed here"),
            TypeError::InlineParagraphBreak => f.write_str("paragraph break not allowed in inline content"),
            TypeError::NoContentAllowed => f.write_str("non-content content not allowed here"),
            TypeError::SortKeyInvalid(was) => write!(f, "sort key must be str or int, was {was}"),
            TypeError::SortKeyHeterogeneous => f.write_str("sort key must be homogeneous, not a mix of str and int"),
        }
    }
}
