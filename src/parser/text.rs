use std::rc::Rc;
use aho_corasick::{AhoCorasick, MatchKind, AhoCorasickBuilder};
use htmlentity::entity;
use once_cell::sync::Lazy;

use crate::utils::{ice_at, SourceRange, Diagnostics};
use super::ast::*;
use super::queue::Parser;
use super::token::{Token, TokenKind};

pub fn encode_entity(s: &str, escape_quotes: bool) -> String {
    entity::encode(
        s,
        if escape_quotes { entity::EntitySet::SpecialChars } else { entity::EntitySet::Html },
        entity::EncodeType::NamedOrHex,
    ).into_iter().collect()
}

pub fn decode_entity(range: &SourceRange, diagnostics: &mut Diagnostics) -> String {
    let s = range.as_str();
    let decoded = entity::decode(s).iter().collect();
    if s == decoded { diagnostics.syntax_error("invalid entity", range); }
    decoded
}

pub fn unescape_char(range: &SourceRange, diagnostics: &mut Diagnostics) -> char {
    let s = range.as_str();
    match s.chars().nth(1).unwrap() {
        'x' | 'u' | 'U' => {
            let Ok(char_code) = u32::from_str_radix(&s[2..], 16) else {
                ice_at("failed to parse hex digits", range);
            };
            match char::from_u32(char_code) {
                Some(c) => c,
                None => {
                    // replacement character
                    diagnostics.error("invalid unicode escape", range);
                    '\u{FFFD}'
                },
            }
        },
        'n' => '\n',
        't' => '\t',
        c => c,
    }
}

static TEXT_SUBS: Lazy<AhoCorasick> = Lazy::new(
    || AhoCorasickBuilder::new()
        .match_kind(MatchKind::LeftmostLongest)
        .build(&[
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
pub fn substitutions(s: &str) -> String {
    TEXT_SUBS.replace_all(s, &[
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
    pub fn parse_text(&mut self, first_token: Token) -> AST {
        let Some(text) = first_token.text() else {
            ice_at("invalid text token", &first_token.range)
        };
        
        let mut text = text.to_string();
        let mut range = first_token.range;
        while let Some(tok) = self.poll_if(|tok| match tok.kind {
            // must skip quotes in case we're parsing text inside a string template
            TokenKind::LAngle |
            TokenKind::Quote(..) => false,
            _ => if let Some(t) = tok.text() { text += t; true } else { false },
        }) {
            range.end = tok.range.end;
        }
        
        text = substitutions(&text);
        AST::Text(Rc::from(text), range)
    }
}
