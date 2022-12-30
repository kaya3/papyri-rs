use std::rc::Rc;

use crate::errors::{Diagnostics, ice_at, ReportingLevel};
use crate::parser::{token, tokenize};
use crate::utils::sourcefile::SourceFile;
use super::highlight::{TokenKind, LineHighlighter};
use super::html::HTML;

pub(super) fn syntax_highlight_papyri(src: &str) -> Vec<HTML> {
    let src = Rc::new(SourceFile::new_anonymous_synthetic(src.trim_end()));
    let tokens = tokenize(
        src.clone(),
        false,
        &mut Diagnostics::new(ReportingLevel::IgnoreAll),
    );
    
    let mut next_is_def = false;
    let mut next_is_tag_name = false;
    let mut line = LineHighlighter::new(&src.src);
    let mut out = Vec::new();
    
    for tok in tokens {
        let kind = match tok.kind {
            token::TokenKind::Comment => TokenKind::Comment,
            
            token::TokenKind::RawText |
            token::TokenKind::Whitespace |
            token::TokenKind::Newline => TokenKind::Plain,
            
            token::TokenKind::Boolean(..) |
            token::TokenKind::Escape |
            token::TokenKind::Entity => TokenKind::KeywordLiteral,
            
            token::TokenKind::Number => TokenKind::Number,
            token::TokenKind::VarName => TokenKind::Name,
            token::TokenKind::Verbatim(..) => TokenKind::String,
            
            token::TokenKind::Name => if next_is_def {
                next_is_def = false;
                TokenKind::NameDef
            } else if next_is_tag_name {
                TokenKind::Name
            } else {
                TokenKind::Plain
            },
            
            token::TokenKind::FuncName => TokenKind::Decorator,
            token::TokenKind::Keyword(k) => {
                next_is_def = k == token::Keyword::Fn;
                TokenKind::Keyword
            },
            
            token::TokenKind::LPar |
            token::TokenKind::LSqb |
            token::TokenKind::LBrace => TokenKind::ParenLeft,
            token::TokenKind::RPar |
            token::TokenKind::RSqb |
            token::TokenKind::RBrace => TokenKind::ParenRight,
            
            _ => TokenKind::Op,
        };
        
        next_is_tag_name = tok.kind == token::TokenKind::LAngle;
        
        let mut cur = tok.range.start as usize;
        let mut do_close = false;
        for part in src.get_span(tok.range).split('\n') {
            if do_close {
                if &src.src[cur - 1..cur] != "\n" {
                    ice_at("newline splitting failed", src.as_ref(), tok.range);
                }
                out.push(line.take_line());
            }
            
            let cur_end = cur + part.len();
            line.push(cur..cur_end, kind);
            
            cur = cur_end + 1;
            do_close = true;
        }
    }
    
    out.push(line.take_line());
    out
}
