//! This module contains helper functions for parsing text.

use std::rc::Rc;
use aho_corasick::{AhoCorasick, MatchKind, AhoCorasickBuilder};
use htmlentity::entity;
use once_cell::sync::Lazy;

use crate::errors::{Diagnostics, ice_at, SyntaxError};
use crate::utils::SourceRange;
use super::ast::*;
use super::queue::Parser;
use super::token::{Token, TokenKind};

/// Decodes a string containing an HTML entity to the character that entity
/// represents. If the string is not a valid entity, a syntax error is reported.
pub(crate) fn decode_entity(range: &SourceRange, diagnostics: &mut Diagnostics) -> String {
    let s = range.as_str();
    let decoded = entity::decode(s).iter().collect();
    if s == decoded { diagnostics.syntax_error(SyntaxError::TokenInvalidEntity, range); }
    decoded
}

/// Decodes an escape sequence, such as `\xA0` or `\n`. If the escape sequence
/// does not define a valid Unicode character, then a syntax error is reported
/// through `diagnostics` and a Unicode replacement character is returned.
pub(crate) fn unescape_char(range: &SourceRange, diagnostics: &mut Diagnostics) -> String {
    let s = range.as_str();
    match s.chars().nth(1).unwrap() {
        'x' | 'u' | 'U' => {
            let Ok(char_code) = u32::from_str_radix(&s[2..], 16) else {
                ice_at("failed to parse hex digits", range);
            };
            match char::from_u32(char_code) {
                Some(c) => c,
                None => {
                    diagnostics.syntax_error(SyntaxError::TokenInvalidEscape, range);
                    char::REPLACEMENT_CHARACTER
                },
            }
        },
        'n' => '\n',
        't' => '\t',
        c => c,
    }.to_string()
}

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
    /// Parses literal text starting at `first_token`, until just before the
    /// next token without text, or the next `LAngle` token, whichever is
    /// first. Text substitutions are applied here.
    /// 
    /// `first_token` must be a token with text.
    pub(super) fn parse_text(&mut self, first_token: Token) -> AST {
        let Some(text) = first_token.text() else {
            ice_at("invalid text token", &first_token.range)
        };
        
        let mut text = text.to_string();
        let mut range = first_token.range;
        while let Some(tok) = self.poll_if(|tok| match tok.kind {
            TokenKind::LAngle => false,
            _ => if let Some(t) = tok.text() { text += t; true } else { false },
        }) {
            range.end = tok.range.end;
        }
        
        text = substitutions(&text);
        AST::Text(Rc::from(text), range)
    }
}
