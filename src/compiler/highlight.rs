use std::rc::Rc;

use crate::utils::str_ids;
use super::html::HTML;
use super::tag::Tag;

pub fn enumerate_lines(lines: Vec<HTML>, start: i64) -> HTML {
    let mut out = Vec::new();
    for (i, line) in lines.into_iter().enumerate() {
        let mut tag = Tag::new(str_ids::SPAN, line.clone());
        tag.add_css_class("line");
        let index = Rc::from((i as i64 + start).to_string());
        tag.attr(str_ids::DATA_LINE_NO, Some(index));
        out.push(HTML::from(tag));
        out.push(HTML::RawNewline);
    }
    HTML::seq(&out)
}

fn no_highlighting(src: &str) -> Vec<HTML> {
    src.lines()
        .map(HTML::text)
        .collect()
}

#[cfg(not(feature="syntect"))]
pub fn syntax_highlight(src: &str, _language: &str) -> Vec<HTML> {
    no_highlighting(src)
}

#[cfg(feature="syntect")]
pub fn syntax_highlight(src: &str, language: &str) -> Vec<HTML> {
    syntect_highlighting::highlight(src, language)
}


#[cfg(feature="syntect")]
mod syntect_highlighting {
    use std::ops::Range;
    use std::rc::Rc;
    use std::str::FromStr;
    use once_cell::sync::Lazy;
    use syntect::parsing::{SyntaxSet, Scope, ScopeStack, ParseState};
    use syntect::easy::ScopeRangeIterator;
    use syntect::highlighting::{ScopeSelector, ScopeSelectors};
    
    use crate::utils::{ice, str_ids, text};
    use super::{HTML, Tag};
    
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    enum TokenKind {
        Comment,
        CommentURL,
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
        URL,
        Whitespace,
        Invalid,
    }
    
    struct SyntectCache {
        syntax_set: SyntaxSet,
        url_matcher: regex_lexer::Lexer<bool>,
        
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
        url_matcher: regex_lexer::LexerBuilder::new()
            .token(".", false)
            .token("https?://[^\\s'\"`]+", true)
            .build()
            .expect("Failed to build regex_lexer"),
        
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
    
    fn matching_paren(s: &str) -> &'static str {
        match s {
            "(" => ")",
            "{" => "}",
            "[" => "]",
            _ => ice("not a paren"),
        }
    }
    
    fn token_kind(s: &str, stack: &[Scope]) -> TokenKind {
        if text::is_whitespace(s) {
            TokenKind::Whitespace
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
        } else if text::is_ascii_alphabetic(s) && CACHE.keyword.does_match(stack).is_some() {
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
        } else if text::is_ascii_alphabetic(s) {
            TokenKind::Name
        } else {
            TokenKind::Punctuation
        }
    }
    
    pub fn highlight(src: &str, language: &str) -> Vec<HTML> {
        let Some(syntax) = CACHE.syntax_set.find_syntax_by_token(language) else {
            return super::no_highlighting(src);
        };
        
        let mut state = ParseState::new(syntax);
        let mut paren_stack = Vec::new();
        let mut paren_count: u32 = 0;
        let mut stack = ScopeStack::new();
        let mut out = Vec::new();
        for line in src.lines() {
            match state.parse_line(line, &CACHE.syntax_set) {
                Ok(ops) => {
                    let mut line_highlighter = LineHighlighter::new(line);
                    for (range, op) in ScopeRangeIterator::new(&ops, line) {
                        stack.apply(op).unwrap();
                        if range.is_empty() { continue; }
                        
                        let s = &line[range.clone()];
                        let kind = token_kind(s, stack.as_slice());
                        match kind {
                            TokenKind::Comment => {
                                line_highlighter.push_with_possible_urls(range, kind, TokenKind::CommentURL);
                            },
                            TokenKind::String => {
                                line_highlighter.push_with_possible_urls(range, kind, TokenKind::URL);
                            },
                            TokenKind::ParenLeft => {
                                paren_count += 1;
                                paren_stack.push((matching_paren(s), paren_count));
                                line_highlighter.push_with_paren_no(range, kind, paren_count);
                            },
                            TokenKind::ParenRight => match paren_stack.last() {
                                Some((rpar, paren_no)) if *rpar == s => {
                                    line_highlighter.push_with_paren_no(range, kind, *paren_no);
                                    paren_stack.pop();
                                },
                                _ => {
                                    line_highlighter.push(range, TokenKind::ParenUnmatched);
                                }
                            },
                            _ => {
                                line_highlighter.push(range, kind);
                            }
                        }
                    }
                    out.push(line_highlighter.close());
                },
                Err(_) => {
                    println!("Failed to parse: {}", line);
                    out.push(HTML::text(line));
                },
            }
        }
        
        // TODO: if paren_stack is not empty, need to go back and mark left-parens as unmatched
        
        out
    }
    
    struct LineHighlighter<'a> {
        line: &'a str,
        parts: Vec<HTML>,
        current_range: Range<usize>,
        current_token_kind: Option<TokenKind>,
    }
    
    impl <'a> LineHighlighter<'a> {
        fn new(line: &str) -> LineHighlighter {
            LineHighlighter {
                line,
                parts: Vec::new(),
                current_range: 0..0,
                current_token_kind: None,
            }
        }
        
        fn push_with_possible_urls(&mut self, range: Range<usize>, kind: TokenKind, url_kind: TokenKind) {
            let s = &self.line[range.clone()];
            for t in CACHE.url_matcher.tokens(s) {
                let span = (range.start + t.span.start)..(range.start + t.span.end);
                if t.kind {
                    self.push(span, url_kind);
                    self.close_part();
                } else {
                    self.push(span, kind);
                }
            }
        }
        
        fn push_with_paren_no(&mut self, range: Range<usize>, kind: TokenKind, paren_no: u32) {
            self.close_part();
            let s = &self.line[range.clone()];
            self.parts.push(make_token(kind, s, paren_no));
            self.current_range = range.end..range.end;
        }
        
        fn push(&mut self, range: Range<usize>, kind: TokenKind) {
            if range.is_empty() { return; }
            if matches!(self.current_token_kind, Some(k) if k == kind) {
                self.current_range.end = range.end;
                return;
            }
            self.close_part();
            self.current_token_kind = Some(kind);
            self.current_range = range;
        }
        
        fn close_part(&mut self) {
            if let Some(kind) = self.current_token_kind {
                let s = &self.line[self.current_range.clone()];
                self.parts.push(make_token(kind, s, 0));
                self.current_range.start = self.current_range.end;
                self.current_token_kind = None;
            }
        }
        
        fn close(mut self) -> HTML {
            self.close_part();
            HTML::seq(&self.parts)
        }
    }
    
    fn make_token(kind: TokenKind, s: &str, paren_no: u32) -> HTML {
        let css_class = match kind {
            TokenKind::Comment => "comment",
            TokenKind::CommentURL => return make_link_token(s, true),
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
            TokenKind::URL => return make_link_token(s, false),
            TokenKind::Whitespace => return if s == " " { HTML::Whitespace } else { HTML::text(s) },
        };
        
        let mut tag = Tag::new(str_ids::SPAN, HTML::text(s));
        tag.add_css_class(css_class);
        if paren_no > 0 {
            tag.attr(str_ids::DATA_PAREN_NO, Some(Rc::from(format!("{}", paren_no))));
        }
        HTML::from(tag)
    }
    
    fn make_link_token(s: &str, is_comment: bool) -> HTML {
        let mut tag = Tag::new(str_ids::A, HTML::text(s));
        tag.attr(str_ids::HREF, Some(Rc::from(s)));
        if is_comment { tag.add_css_class("comment"); }
        HTML::from(tag)
    }
}
