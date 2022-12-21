use crate::errors::SyntaxError;

use super::ast::*;
use super::queue::Parser;
use super::token::{Token, TokenKind};

impl Token {
    /// Indicates whether this token is a primitive type name.
    fn is_primitive_type(&self) -> bool {
        matches!(self.kind, TokenKind::Name)
            && matches!(self.as_str(), "any" | "none" | "html" | "block" | "inline" | "bool" | "int" | "str" | "function" | "regex")
    }
    
    /// Indicates whether this token is a group type name or modifier.
    fn is_group_type(&self) -> bool {
        matches!(self.as_str(), "dict" | "list" | "?")
    }
}

impl <'a> Parser<'a> {
    /// Parses a type annotation, not including the initial `:`.
    pub(super) fn parse_type(&mut self) -> Option<TypeAnnotation> {
        self.skip_whitespace();
        let begin_token = self.expect_poll_kind(TokenKind::Name)?;
        let mut t = if begin_token.is_primitive_type() {
            TypeAnnotation::Primitive(begin_token.range)
        } else if begin_token.is_group_type() {
            let g = (None, begin_token.range);
            TypeAnnotation::Group(Box::new(g))
        } else {
            self.diagnostics.syntax_error(SyntaxError::TokenInvalidPrimitiveType, &begin_token.range);
            return None;
        };
        
        while let Some(group_kind) = {
            self.skip_whitespace();
            self.poll_if(Token::is_group_type)
        } {
            let g = (Some(t), group_kind.range);
            t = TypeAnnotation::Group(Box::new(g));
        }
        Some(t)
    }
}
