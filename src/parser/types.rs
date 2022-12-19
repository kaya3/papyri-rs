use crate::errors::SyntaxError;

use super::ast::*;
use super::queue::Parser;
use super::token::{Token, TokenKind};

impl Token {
    /// Indicates whether this token is a primitive type name.
    pub fn is_primitive_type(&self) -> bool {
        matches!(self.kind, TokenKind::Name)
            && matches!(self.as_str(), "any" | "none" | "html" | "block" | "inline" | "bool" | "int" | "str" | "function" | "regex")
    }
    
    /// Indicates whether this token is a group type name or modifier.
    pub fn is_group_type(&self) -> bool {
        matches!(self.as_str(), "dict" | "list" | "?")
    }
}

impl <'a> Parser<'a> {
    pub(super) fn parse_type(&mut self) -> Option<TypeAnnotation> {
        self.skip_whitespace();
        let primitive_token = self.expect_poll_kind(TokenKind::Name)?;
        if !primitive_token.is_primitive_type() {
            self.diagnostics.syntax_error(SyntaxError::TokenInvalidPrimitiveType, &primitive_token.range);
            return None;
        };
        
        let mut t = TypeAnnotation::Primitive(primitive_token.range);
        while let Some(group_kind) = {
            self.skip_whitespace();
            self.poll_if(Token::is_group_type)
        } {
            t = TypeAnnotation::Group(Box::new(t), group_kind.range);
        }
        Some(t)
    }
}
