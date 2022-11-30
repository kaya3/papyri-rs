use crate::compiler::Type;

#[allow(missing_docs)]
/// Represents a type error which occurs during compilation of a Papyri source
/// file.
pub enum TypeError {
    ExpectedWas(Type, Type),
    TooManyPositionalArgs(usize, usize),
    NotEnoughPositionalArgs(usize, usize),
    InlineBlockContent,
    InlineParagraphBreak,
}

impl std::fmt::Display for TypeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeError::ExpectedWas(expected, was) => write!(f, "expected {expected}, was {was}"),
            TypeError::TooManyPositionalArgs(expected, was) => write!(f, "too many positional arguments (expected {expected}, was {was})"),
            TypeError::NotEnoughPositionalArgs(expected, was) => write!(f, "not enough positional arguments (expected {expected}, was {was})"),
            TypeError::InlineBlockContent => f.write_str("block content not allowed inside inline content"),
            TypeError::InlineParagraphBreak => f.write_str("paragraph break not allowed in inline content"),
        }
    }
}
