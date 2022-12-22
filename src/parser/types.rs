use crate::errors::SyntaxError;

use super::ast::*;
use super::base::Parser;
use super::token::{Token, TokenKind};

impl <'a> Parser<'a> {
    fn is_primitive_type(&self, tok: &Token) -> bool {
        matches!(tok.kind, TokenKind::Name)
            && matches!(self.src.get_span(tok.range), "any" | "none" | "html" | "block" | "inline" | "bool" | "int" | "str" | "function" | "regex")
    }
    
    /// Indicates whether this token is a group type name or modifier.
    fn is_group_type(&self, tok: &Token) -> bool {
        matches!(self.src.get_span(tok.range), "dict" | "list" | "?")
    }
    
    /// Parses a type annotation, not including the initial `:`.
    pub(super) fn parse_type(&mut self) -> Option<TypeAnnotation> {
        self.skip_whitespace();
        let begin_token = self.expect_poll_kind(TokenKind::Name)?;
        let mut t = if self.is_primitive_type(&begin_token) {
            TypeAnnotation::Primitive(begin_token.range)
        } else if self.is_group_type(&begin_token) {
            let g = (None, begin_token.range);
            TypeAnnotation::Group(Box::new(g))
        } else {
            self.syntax_error(SyntaxError::TokenInvalidPrimitiveType, begin_token.range);
            return None;
        };
        
        while let Some(group_kind) = {
            self.skip_whitespace();
            self.poll_if(|_self, t| _self.is_group_type(t))
        } {
            let g = (Some(t), group_kind.range);
            t = TypeAnnotation::Group(Box::new(g));
        }
        Some(t)
    }
}
