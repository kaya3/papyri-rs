//! This module contains helper functions for parsing text.

use std::rc::Rc;
use aho_corasick::{AhoCorasick, MatchKind, AhoCorasickBuilder};
use htmlentity::entity;
use once_cell::sync::Lazy;

use crate::errors::SyntaxError;
use super::ast::*;
use super::base::Parser;
use super::token::{Token, TokenKind, QuoteKind, QuoteDir};

static TEXT_SUBS: Lazy<AhoCorasick> = Lazy::new(
    || AhoCorasickBuilder::new()
        .match_kind(MatchKind::LeftmostLongest)
        .build([
            // minus signs
            "-0", "-1", "-2", "-3", "-4",
            "-5", "-6", "-7", "-8", "-9",
            
            "---", // em dash
            "--", // en dash
            "<->", // left-right arrow
            "<-", // left arrow
            "->", // right arrow
            "!=", // not equal
            "<=", // less than or equal
            ">=", // greater than or equal
            "+/-", // plus or minus
            "(c)", // copyright
            "~", // non-breaking space
        ])
);

/// Performs a set of text substitutions, replacing certain substrings which
/// represent dashes, arrows or other symbols with the corresponding Unicode
/// characters.
pub(crate) fn substitutions(s: &str) -> String {
    TEXT_SUBS.replace_all(s, &[
        // minus signs
        "\u{2212}0", "\u{2212}1", "\u{2212}2", "\u{2212}3", "\u{2212}4",
        "\u{2212}5", "\u{2212}6", "\u{2212}7", "\u{2212}8", "\u{2212}9",
        
        "\u{2014}", // em dash
        "\u{2013}", // en dash
        "\u{2194}", // left-right arrow
        "\u{2190}", // left arrow
        "\u{2192}", // right arrow
        "\u{2260}", // not equal
        "\u{2264}", // less than or equal
        "\u{2265}", // greater than or equal
        "\u{00B1}", // plus or minus
        "\u{00A9}", // copyright
        "\u{00A0}", // non-breaking space
    ])
}

impl <'a> Parser<'a> {
    /// Decodes a string containing an HTML entity to the character that entity
    /// represents. If the string is not a valid entity, a syntax error is
    /// reported and a Unicode replacement character is returned.
    pub(super) fn decode_entity(&mut self, tok: Token) -> char {
        let s = self.tok_str(tok);
        let decoded = entity::decode(s);
        if decoded.len() == 1 {
            decoded[0]
        } else {
            self.report(SyntaxError::TokenInvalidEntity, tok.range);
            char::REPLACEMENT_CHARACTER
        }
    }
    
    /// Decodes an escape sequence, such as `\xA0` or `\n`. If the escape sequence
    /// does not define a valid Unicode character, then a syntax error is reported
    /// and a Unicode replacement character is returned.
    pub(super) fn unescape_char(&mut self, tok: Token) -> char {
        let s = self.tok_str(tok);
        match s.chars().nth(1).unwrap() {
            'x' | 'u' | 'U' => {
                let char_code = u32::from_str_radix(&s[2..], 16)
                    .unwrap_or_else(|e| self.ice_at(&format!("failed to parse hex digits: {e}"), tok.range));
                match char::from_u32(char_code) {
                    Some(c) => c,
                    None => {
                        self.report(SyntaxError::TokenInvalidEscape, tok.range);
                        char::REPLACEMENT_CHARACTER
                    },
                }
            },
            'n' => '\n',
            't' => '\t',
            c => c,
        }
    }
    
    /// Parses literal text starting at `first_token`, until just before the
    /// next token without text, or the next `LAngle` token, whichever is
    /// first. Text substitutions are applied here.
    /// 
    /// `first_token` must be a token with text.
    pub(super) fn parse_text(&mut self, first_token: Token) -> AST {
        let mut text = self.token_text(first_token)
            .unwrap_or_else(|| self.ice_at("invalid text token", first_token.range))
            .to_string();
        
        let mut range = first_token.range;
        while let Some(tok) = self.poll_if(|_self, tok| match tok.kind {
            TokenKind::LAngle => false,
            _ => if let Some(t) = _self.token_text(tok) { text += t; true } else { false },
        }) {
            range.end = tok.range.end;
        }
        
        text = substitutions(&text);
        AST::Text(Rc::from(text), range)
    }
    
    /// Returns the representation of this token as normal text, if it has one.
    fn token_text(&self, tok: Token) -> Option<&str> {
        match tok.kind {
            TokenKind::Name |
            TokenKind::Number |
            TokenKind::Boolean(..) |
            TokenKind::LPar |
            TokenKind::RPar |
            TokenKind::LAngle |
            TokenKind::RAngle |
            TokenKind::Ampersand |
            TokenKind::Arrow |
            TokenKind::Asterisk |
            TokenKind::Colon |
            TokenKind::Comma |
            TokenKind::Dot |
            TokenKind::DoubleAsterisk |
            TokenKind::DoubleColon |
            TokenKind::Equals |
            TokenKind::ExclamationMark |
            TokenKind::QuestionMark |
            TokenKind::QuestionMarkDoubleColon |
            TokenKind::Underscore |
            TokenKind::RawText => Some(self.tok_str(tok)),
            
            TokenKind::Ellipsis => Some("\u{2026}"),
            
            TokenKind::Quote(q, d) => Some(match (q, d) {
                // single quotes
                (QuoteKind::Single, QuoteDir::Left) => "\u{2018}",
                (QuoteKind::Single, QuoteDir::Right) => "\u{2019}",
                // double quotes
                (QuoteKind::Double, QuoteDir::Left) => "\u{201C}",
                (QuoteKind::Double, QuoteDir::Right) => "\u{201D}",
            }),
            
            // word joiner
            TokenKind::Bar => Some("\u{2060}"),
            
            _ => None,
        }
    }
}
