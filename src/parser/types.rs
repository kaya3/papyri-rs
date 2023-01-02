use crate::errors::{SyntaxError, Warning};
use crate::utils::sourcefile::SourceRange;

use super::base::Parser;
use super::token::TokenKind;

#[derive(Clone, PartialEq, Eq)]
/// Represents a type in the Papyri language.
pub enum Type {
    /// The unit type `none`, inhabited only by `Value::UNIT`.
    Unit,
    
    /// The top type `any`, which all values are assignable to.
    Any,
    
    /// The type `html`, representing any HTML content; `block` and `inline`
    /// are subtypes.
    HTML,
    
    /// The type `block`, representing block-level HTML content, or content
    /// which otherwise need not be wrapped in a block-level element.
    Block,
    
    /// The type `inline`, representing inline HTML content.
    Inline,
    
    /// The type `bool`, representing the Boolean values `True` and `False`.
    Bool,
    
    /// The type `int`, representing signed 64-bit integer values.
    Int,
    
    /// The type `str`, representing string values. HTML text content is not
    /// considered to be a string value.
    Str,
    
    /// The type `function`, representing all function with any signature.
    Function,
    
    /// The type `regex`, representing regular expressions.
    Regex,
    
    /// A type of the form `T dict`, representing a dictionary whose elements
    /// are of type `T`.
    Dict(Box<Type>),
    
    /// A type of the form `T list`, representing a list whose elements are of
    /// type `T`.
    List(Box<Type>),
    
    /// A type of the form `T?`, representing either values of type `T` or the
    /// unit value. Optional types are normalised so that the unit value is not
    /// assignable to `T` itself.
    Optional(Box<Type>),
}

impl Type {
    /// Creates a representation of a `dict` type.
    pub(crate) fn dict(self) -> Type {
        Type::Dict(Box::new(self))
    }
    
    /// Creates a representation of a `list` type.
    pub(crate) fn list(self) -> Type {
        Type::List(Box::new(self))
    }
    
    /// Creates a representation of an optional type. The representation is
    /// normalised by simplifying `T?` to `T` whenever the unit value is
    /// already assignable to `T`.
    pub(crate) fn option(self) -> Type {
        if self.unit_is_assignable() {
            self
        } else {
            Type::Optional(Box::new(self))
        }
    }
    
    /// Converts this type to an optional type, if the given condition is true.
    pub(crate) fn option_if(self, condition: bool) -> Type {
        if condition { self.option() } else { self }
    }
    
    /// Indicates whether this type is either `html`, `block` or `inline`. The
    /// types `any` and `none` are not considered to be HTML types.
    pub(crate) fn is_html(&self) -> bool {
        matches!(self, Type::HTML | Type::Block | Type::Inline)
    }
    
    /// Indicates whether the unit value is assignable to this type.
    pub(crate) fn unit_is_assignable(&self) -> bool {
        matches!(self, Type::Any |
            Type::HTML |
            Type::Block |
            Type::Inline |
            Type::Unit |
            Type::Optional(..))
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Any => f.write_str("any"),
            Type::HTML => f.write_str("html"),
            Type::Block => f.write_str("block"),
            Type::Inline => f.write_str("inline"),
            Type::Unit => f.write_str("none"),
            Type::Bool => f.write_str("bool"),
            Type::Int => f.write_str("int"),
            Type::Str => f.write_str("str"),
            Type::Function => f.write_str("function"),
            Type::Regex => f.write_str("regex"),
            Type::Dict(t) => write!(f, "{t} dict"),
            Type::List(t) => write!(f, "{t} list"),
            Type::Optional(t) => write!(f, "{t}?"),
        }
    }
}

impl std::fmt::Debug for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self, f)
    }
}

impl <'a> Parser<'a> {
    /// Parses a type annotation, not including the initial `:`.
    pub(super) fn parse_type(&mut self) -> (Type, Option<SourceRange>) {
        self.skip_whitespace();
        let Some(begin_token) = self.expect_poll_kind(TokenKind::Name) else {
            return (Type::Any, None);
        };
        let mut t = match self.tok_str(begin_token) {
            "any" => Type::Any,
            "none" => Type::Unit,
            "html" => Type::HTML,
            "block" => Type::Block,
            "inline" => Type::Inline,
            "bool" => Type::Bool,
            "int" => Type::Int,
            "str" => Type::Str,
            "function" => Type::Function,
            "regex" => Type::Regex,
            "list" => Type::Any.list(),
            "dict" => Type::Any.dict(),
            _ => {
                self.syntax_error(SyntaxError::TokenInvalidPrimitiveType, begin_token.range);
                Type::Any
            },
        };
        
        let mut range = begin_token.range;
        while let Some(group_tok) = {
            self.skip_whitespace();
            self.poll_if(|_self, t| matches!(t.kind, TokenKind::Name | TokenKind::QuestionMark))
        } {
            match self.tok_str(group_tok) {
                "list" => t = t.list(),
                "dict" => t = t.dict(),
                "?" => if t.unit_is_assignable() {
                    self.diagnostics.warning(Warning::RedundantOptionType, self.src.clone(), group_tok.range);
                } else {
                    t = Type::Optional(Box::new(t));
                },
                _ => {
                    self.syntax_error(SyntaxError::TokenInvalidGroupType, begin_token.range);
                },
            }
            range.end = group_tok.range.end;
        }
        (t, Some(range))
    }
}
