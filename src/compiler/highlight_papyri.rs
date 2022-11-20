use crate::parser;
use crate::utils::{Diagnostics, SourceFile, ice_at};
use super::highlight::{TokenKind, LineHighlighter};
use super::html::HTML;

pub fn syntax_highlight_papyri(src: &str) -> Vec<HTML> {
    let tokens = parser::tokenize(
        SourceFile::synthetic("<string>", src.trim_end()),
        false,
        &mut Diagnostics::new(),
    );
    
    let mut next_is_def = false;
    let mut out = Vec::new();
    let mut line = LineHighlighter::new(src);
    
    for tok in tokens {
        let kind = match tok.kind {
            parser::TokenKind::Comment => TokenKind::Comment,
            
            parser::TokenKind::Whitespace |
            parser::TokenKind::Newline => TokenKind::Whitespace,
            
            parser::TokenKind::Escape |
            parser::TokenKind::Entity => TokenKind::KeywordLiteral,
            
            parser::TokenKind::RawText => TokenKind::Punctuation,
            
            parser::TokenKind::Name => if next_is_def {
                next_is_def = false;
                TokenKind::NameDef
            } else {
                TokenKind::Punctuation
            },
            
            parser::TokenKind::FuncName => match tok.as_str() {
                "@fn" => { next_is_def = true; TokenKind::Keyword },
                "@match" => TokenKind::Keyword,
                _ => TokenKind::Decorator,
            },
            
            parser::TokenKind::VarName => TokenKind::Name,
            parser::TokenKind::Number => TokenKind::Number,
            parser::TokenKind::Boolean => TokenKind::KeywordLiteral,
            parser::TokenKind::Verbatim => TokenKind::String,
            
            parser::TokenKind::LPar |
            parser::TokenKind::LSqb |
            parser::TokenKind::LBrace => TokenKind::ParenLeft,
            parser::TokenKind::RPar |
            parser::TokenKind::RSqb |
            parser::TokenKind::RBrace => TokenKind::ParenRight,
            
            _ => TokenKind::Op,
        };
        
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
