use std::ops::Range;
use once_cell::sync::Lazy;

use crate::errors;
use crate::utils::str_ids;
use super::highlight_papyri::syntax_highlight_papyri;
use super::html::HTML;
use super::tag::Tag;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenKind {
    Plain,
    Comment,
    Decorator,
    Keyword,
    KeywordLiteral,
    KeywordOp,
    Name,
    NameDef,
    Number,
    Op,
    ParenLeft,
    ParenRight,
    ParenUnmatched,
    Punctuation,
    String,
    TypeAnnotation,
    Url(bool),
    Invalid,
}

fn matching_paren(s: &str) -> &'static str {
    match s {
        "(" => ")",
        "{" => "}",
        "[" => "]",
        _ => errors::ice("not a paren"),
    }
}

static URL_REGEX: Lazy<regex::Regex> = Lazy::new(|| {
    regex::Regex::new("https?://[^\\s'\"`]+")
        .expect("Failed to compile URL regex")
});

pub struct LineHighlighter<'a> {
    src: &'a str,
    parts: Vec<HTML>,
    paren_stack: Vec<(&'static str, u32)>,
    paren_count: u32,
    current_range: Range<usize>,
    current_token_kind: Option<TokenKind>,
}

impl <'a> LineHighlighter<'a> {
    pub fn new(src: &str) -> LineHighlighter {
        LineHighlighter {
            src,
            parts: Vec::new(),
            paren_stack: Vec::new(),
            paren_count: 0,
            current_range: 0..0,
            current_token_kind: None,
        }
    }
    
    pub fn push(&mut self, range: Range<usize>, kind: TokenKind) {
        let s = &self.src[range.clone()];
        match kind {
            TokenKind::Comment |
            TokenKind::String => {
                self.push_with_possible_urls(range, kind, TokenKind::Url(kind == TokenKind::Comment));
            },
            TokenKind::ParenLeft => {
                self.paren_count += 1;
                self.paren_stack.push((matching_paren(s), self.paren_count));
                self.push_with_paren_no(range, kind, self.paren_count);
            },
            TokenKind::ParenRight => match self.paren_stack.last() {
                Some(&(rpar, paren_no)) if rpar == s => {
                    self.push_with_paren_no(range, kind, paren_no);
                    self.paren_stack.pop();
                },
                _ => {
                    self.push_other(range, TokenKind::ParenUnmatched);
                },
            },
            _ => {
                self.push_other(range, kind);
            },
        }
    }
    
    fn push_with_possible_urls(&mut self, range: Range<usize>, kind: TokenKind, url_kind: TokenKind) {
        let s = &self.src[range.clone()];
        
        let mut cur = range.start;
        for m in URL_REGEX.find_iter(s) {
            let m_start = range.start + m.start();
            let m_end = range.start + m.end();
            
            self.push_other(cur..m_start, kind);
            self.push_other(m_start..m_end, url_kind);
            self.close_part();
            
            cur = m_end;
        }
        self.push_other(cur..range.end, kind);
    }
    
    fn push_with_paren_no(&mut self, range: Range<usize>, kind: TokenKind, paren_no: u32) {
        self.close_part();
        let s = &self.src[range.clone()];
        self.parts.push(LineHighlighter::make_token(kind, s, paren_no));
        self.current_range = range.end..range.end;
    }
    
    fn push_other(&mut self, range: Range<usize>, kind: TokenKind) {
        if range.is_empty() {
            // do nothing
        } else if matches!(self.current_token_kind, Some(k) if k == kind) {
            self.current_range.end = range.end;
        } else {
            self.close_part();
            self.current_token_kind = Some(kind);
            self.current_range = range;
        }
    }
    
    fn close_part(&mut self) {
        if let Some(kind) = self.current_token_kind {
            let s = &self.src[self.current_range.clone()];
            self.parts.push(LineHighlighter::make_token(kind, s, 0));
            self.current_range.start = self.current_range.end;
            self.current_token_kind = None;
        }
    }
    
    pub fn take_line(&mut self) -> HTML {
        self.close_part();
        let line = HTML::seq(std::mem::take(&mut self.parts));
        self.current_token_kind = None;
        line
    }
    
    fn make_token(kind: TokenKind, s: &str, paren_no: u32) -> HTML {
        let css_class = match kind {
            TokenKind::Plain => return HTML::text(s),
            TokenKind::Url(is_comment) => return LineHighlighter::make_link_token(s, is_comment),
            
            TokenKind::Comment => "comment",
            TokenKind::Decorator => "decorator",
            TokenKind::Invalid => "err",
            TokenKind::Keyword => "keyword",
            TokenKind::KeywordLiteral => "keyword-literal",
            TokenKind::KeywordOp => "keyword-op",
            TokenKind::Name => "name",
            TokenKind::NameDef => "name-def",
            TokenKind::Number => "number",
            TokenKind::Op => "op",
            TokenKind::ParenLeft => "lparen",
            TokenKind::ParenRight => "rparen",
            TokenKind::ParenUnmatched => "unmatched-paren",
            TokenKind::Punctuation => "punctuation",
            TokenKind::String => "string",
            TokenKind::TypeAnnotation => "type-annotation",
        };
        
        let mut tag = Tag::new(str_ids::SPAN, HTML::text(s))
            .str_attr(str_ids::CLASS, css_class);
        if paren_no > 0 {
            tag = tag.str_attr(str_ids::DATA_PAREN_NO, &paren_no.to_string());
        }
        tag.into()
    }
    
    fn make_link_token(s: &str, is_comment: bool) -> HTML {
        let mut tag = Tag::new(str_ids::A, HTML::text(s))
            .str_attr(str_ids::HREF, s);
        if is_comment { tag = tag.str_attr(str_ids::CLASS, "comment"); }
        tag.into()
    }
}

pub fn enumerate_lines(lines: Vec<HTML>, start: i64) -> HTML {
    let mut out = Vec::new();
    for (i, line) in lines.into_iter().enumerate() {
        let tag = Tag::new(str_ids::SPAN, line)
            .str_attr(str_ids::CLASS, "line")
            .str_attr(str_ids::DATA_LINE_NO, &(i as i64 + start).to_string());
        out.push(tag.into());
        out.push(HTML::RawNewline);
    }
    HTML::seq(out)
}

pub fn no_highlighting(src: &str) -> Vec<HTML> {
    src.lines()
        .map(HTML::text)
        .collect()
}

#[cfg(not(feature="syntect"))]
pub fn syntax_highlight(src: &str, language: &str) -> Option<Vec<HTML>> {
    if language == "papyri" {
        Some(syntax_highlight_papyri(src))
    } else {
        None
    }
}

#[cfg(feature="syntect")]
pub fn syntax_highlight(src: &str, language: &str) -> Option<Vec<HTML>> {
    if language == "papyri" {
        Some(syntax_highlight_papyri(src))
    } else {
        syntect_highlighting::highlight(src, language)
    }
}

#[cfg(feature="syntect")]
mod syntect_highlighting {
    use std::str::FromStr;
    use once_cell::sync::Lazy;
    use syntect::parsing::{SyntaxSet, Scope, ScopeStack, ParseState};
    use syntect::easy::ScopeRangeIterator;
    use syntect::highlighting::{ScopeSelector, ScopeSelectors};
    
    use crate::utils::text;
    use super::{HTML, TokenKind, LineHighlighter};
    
    struct SyntectCache {
        syntax_set: SyntaxSet,
        
        comment: ScopeSelector,
        decorator: ScopeSelector,
        invalid: ScopeSelector,
        keyword: ScopeSelectors,
        keyword_literal: ScopeSelector,
        keyword_op: ScopeSelector,
        name: ScopeSelector,
        name_def: ScopeSelector,
        number: ScopeSelector,
        op: ScopeSelector,
        punctuation: ScopeSelector,
        string: ScopeSelector,
        type_annotation: ScopeSelectors,
    }
    
    fn _selector(s: &str) -> ScopeSelector {
        ScopeSelector::from_str(s).expect("failed to parse scope selector")
    }
    fn _multiple_selectors(s: &str) -> ScopeSelectors {
        ScopeSelectors::from_str(s).expect("failed to parse scope selectors")
    }
    
    static CACHE: Lazy<SyntectCache> = Lazy::new(|| SyntectCache {
        syntax_set: SyntaxSet::load_defaults_nonewlines(),
        
        comment: _selector("comment"),
        decorator: _selector("meta.annotation"),
        invalid: _selector("invalid - invalid.deprecated"),
        keyword: _multiple_selectors("keyword, storage.modifier, storage.type, variable.language"),
        keyword_literal: _selector("constant.language"),
        keyword_op: _selector("keyword.operator.word"),
        name: _selector("variable - variable.annotation"),
        name_def: _selector("entity.name"),
        number: _selector("constant.numeric"),
        op: _selector("keyword.operator"),
        punctuation: _selector("punctuation - punctuation.definition.annotation"),
        string: _selector("string"),
        type_annotation: _multiple_selectors("meta.annotation.type"),
    });
    
    fn token_kind(s: &str, stack: &[Scope]) -> TokenKind {
        if text::is_whitespace(s) {
            TokenKind::Plain
        } else if CACHE.invalid.does_match(stack).is_some() {
            TokenKind::Invalid
        } else if CACHE.comment.does_match(stack).is_some() {
            TokenKind::Comment
        } else if matches!(s, "(" | "{" | "[") {
            TokenKind::ParenLeft
        } else if matches!(s, ")" | "}" | "]") {
            TokenKind::ParenRight
        } else if CACHE.string.does_match(stack).is_some() {
            TokenKind::String
        } else if CACHE.number.does_match(stack).is_some() {
            TokenKind::Number
        } else if CACHE.keyword_literal.does_match(stack).is_some() {
            TokenKind::KeywordLiteral
        } else if CACHE.keyword_op.does_match(stack).is_some() {
            TokenKind::KeywordOp
        } else if CACHE.op.does_match(stack).is_some() {
            TokenKind::Op
        } else if text::is_identifier(s) && CACHE.keyword.does_match(stack).is_some() {
            TokenKind::Keyword
        } else if CACHE.punctuation.does_match(stack).is_some() {
            TokenKind::Punctuation
        } else if CACHE.name_def.does_match(stack).is_some() {
            TokenKind::NameDef
        } else if CACHE.name.does_match(stack).is_some() {
            TokenKind::Name
        } else if CACHE.type_annotation.does_match(stack).is_some() {
            TokenKind::TypeAnnotation
        } else if CACHE.decorator.does_match(stack).is_some() {
            TokenKind::Decorator
        } else if text::is_identifier(s) {
            TokenKind::Name
        } else {
            TokenKind::Punctuation
        }
    }
    
    pub fn highlight(src: &str, language: &str) -> Option<Vec<HTML>> {
        let syntax = CACHE.syntax_set.find_syntax_by_token(language)?;
        
        let mut state = ParseState::new(syntax);
        let mut stack = ScopeStack::new();
        let mut line_highlighter = LineHighlighter::new(src);
        let mut out = Vec::new();
        
        for line in src.lines() {
            line_highlighter.src = line;
            match state.parse_line(line, &CACHE.syntax_set) {
                Ok(ops) => {
                    for (range, op) in ScopeRangeIterator::new(&ops, line) {
                        stack.apply(op).unwrap();
                        if range.is_empty() { continue; }
                        
                        let s = &line[range.clone()];
                        let kind = token_kind(s, stack.as_slice());
                        line_highlighter.push(range, kind);
                    }
                    out.push(line_highlighter.take_line());
                },
                Err(_) => {
                    out.push(HTML::text(line));
                },
            }
        }
        
        // TODO: if paren_stack is not empty, need to go back and mark left-parens as unmatched
        
        Some(out)
    }
}
