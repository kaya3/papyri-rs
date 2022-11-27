use crate::errors::{Diagnostics, ice_at, ReportingLevel};
use crate::parser;
use crate::utils::SourceFile;
use super::highlight::{TokenKind, LineHighlighter};
use super::html::HTML;

pub fn syntax_highlight_papyri(src: &str) -> Vec<HTML> {
    let tokens = parser::tokenize(
        SourceFile::synthetic("<string>", src.trim_end()),
        false,
        &mut Diagnostics::new(ReportingLevel::IgnoreAll),
    );
    
    let mut next_is_def = false;
    let mut next_is_tag_name = false;
    let mut line = LineHighlighter::new(src);
    let mut out = Vec::new();
    
    for tok in tokens {
        let kind = match tok.kind {
            parser::TokenKind::Comment => TokenKind::Comment,
            
            parser::TokenKind::RawText |
            parser::TokenKind::Whitespace |
            parser::TokenKind::Newline => TokenKind::Plain,
            
            parser::TokenKind::Boolean |
            parser::TokenKind::Escape |
            parser::TokenKind::Entity => TokenKind::KeywordLiteral,
            
            parser::TokenKind::Number => TokenKind::Number,
            parser::TokenKind::VarName => TokenKind::Name,
            parser::TokenKind::Verbatim => TokenKind::String,
            
            parser::TokenKind::Name => if next_is_def {
                next_is_def = false;
                TokenKind::NameDef
            } else if next_is_tag_name {
                TokenKind::Name
            } else {
                TokenKind::Plain
            },
            
            parser::TokenKind::FuncName => match tok.as_str() {
                "@fn" => { next_is_def = true; TokenKind::Keyword },
                "@match" => TokenKind::Keyword,
                _ => TokenKind::Decorator,
            },
            
            parser::TokenKind::LPar |
            parser::TokenKind::LSqb |
            parser::TokenKind::LBrace => TokenKind::ParenLeft,
            parser::TokenKind::RPar |
            parser::TokenKind::RSqb |
            parser::TokenKind::RBrace => TokenKind::ParenRight,
            
            _ => TokenKind::Op,
        };
        
        next_is_tag_name = tok.kind == parser::TokenKind::LAngle;
        
        let mut cur = tok.range.start as usize;
        let mut do_close = false;
        for part in tok.as_str().split("\n") {
            if do_close {
                if &src[cur - 1..cur] != "\n" {
                    ice_at("newline splitting failed", &tok.range);
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
