use super::ast::*;
use super::queue::Parser;
use super::token::{Token, TokenKind};

impl <'a> Parser<'a> {
    pub fn parse_type(&mut self) -> Option<TypeAnnotation> {
        self.skip_whitespace();
        let primitive_token = self.expect_poll_kind(TokenKind::Name)?;
        if !primitive_token.is_primitive_type() {
            self.diagnostics.syntax_error(&format!("'{}' is not a type", primitive_token.as_str()), &primitive_token.range);
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
