use std::rc::Rc;

use once_cell::sync::Lazy;

use crate::utils::{Diagnostics, SourceRange, SourceFile};
use super::token::{Token, TokenKind};

// later patterns take priority
static LEXER: Lazy<regex_lexer::Lexer<TokenKind>> = Lazy::new(
    || regex_lexer::LexerBuilder::new()
        .token(r".", TokenKind::Undetermined)
        .token(r"\s+", TokenKind::Whitespace) // or short Newline
        .token(r"\s*\n\s*\n\s*", TokenKind::Newline)
        .token(r"[\$@<]?[a-zA-Z_][a-zA-Z0-9_]*", TokenKind::Name)
        .token(r"</(?:[a-zA-Z_][a-zA-Z0-9_]*)?>", TokenKind::CloseTag)
        .token(r"-?[0-9]+", TokenKind::Number)
        .token(r"\\(?:u[0-9a-fA-F]{4}|U[0-9a-fA-F]{8}|[^uU])", TokenKind::Escape)
        .token(r"&(?:[a-zA-Z][a-zA-Z0-9]*|#[0-9]+);", TokenKind::Entity)
        .token(r"#[^\n]*(?:\n[^\S\n]*)?", TokenKind::Comment)
        .token(r"<!--", TokenKind::LAngleComment)
        .token(r"/>", TokenKind::RAngleSlash)
        .token(r"-->", TokenKind::RAngleComment)
        .token(r"->", TokenKind::Arrow)
        .token(r"\.\.\.", TokenKind::Ellipsis)
        .token(r"\*\*?", TokenKind::Asterisk)
        .token(r"`+", TokenKind::Verbatim)
        .token("[^#\\sa-zA-Z0-9_\\\\\\-\\(\\)\\[\\]\\{\\}<>/\\.,=:~\\|\\?@\\*\\$`'\"]+", TokenKind::RawText)
        .build()
        .expect("Failed to build regex_lexer")
);

pub fn tokenize(src: Rc<SourceFile>, strip_comments: bool, diagnostics: &mut Diagnostics) -> Vec<Token> {
    let mut cur = 0;
    let mut open_tag = false;
    let mut right_quote = false;
    let mut short_newline = false;
    
    let mut errors = Vec::new();
    let mut tokens = Vec::<Token>::new();
    
    let mut token_stream = LEXER.tokens(&src.src);
    while let Some(t) = token_stream.next() {
        let mut kind = t.kind;
        let mut s = t.text;
        
        match kind {
            TokenKind::Undetermined => {
                kind = match s {
                    "(" => TokenKind::LPar,
                    ")" => TokenKind::RPar,
                    "[" => TokenKind::LSqb,
                    "]" => TokenKind::RSqb,
                    "{" => TokenKind::LBrace,
                    "}" => TokenKind::RBrace,
                    "<" => {
                        open_tag = true;
                        TokenKind::LAngle
                    },
                    ">" => TokenKind::RAngle,
                    "." => TokenKind::Dot,
                    "," => TokenKind::Comma,
                    "=" => TokenKind::Equals,
                    ":" => TokenKind::Colon,
                    "~" => TokenKind::Tilde,
                    "|" => TokenKind::Bar,
                    "?" => TokenKind::QuestionMark,
                    "$" => TokenKind::Dollar,
                    "@" => TokenKind::At,
                    "'" => if right_quote { TokenKind::RQuote } else { TokenKind::LQuote },
                    "\"" => if right_quote { TokenKind::RDblQuote } else { TokenKind::LDblQuote },
                    
                    _ => TokenKind::RawText,
                };
            },
            
            TokenKind::Name => {
                kind = match s {
                    "True" | "False" => TokenKind::Boolean,
                    s if s.starts_with("@") => TokenKind::FuncName,
                    s if s.starts_with("<") => TokenKind::OpenTag,
                    s if s.starts_with("$") => TokenKind::VarName,
                    _ => TokenKind::Name,
                };
            }
            
            TokenKind::Whitespace => {
                if short_newline && s.contains("\n") { kind = TokenKind::Newline; }
            },
            
            TokenKind::Verbatim => {
                let backticks = s.len();
                if let Some(close_tok) = token_stream.find(|t| t.kind == TokenKind::Verbatim && t.text.len() >= backticks) {
                    s = &src.src[cur..close_tok.span.end];
                    if close_tok.text.len() > backticks {
                        errors.push("too many backticks in closing delimiter");
                    } else if backticks < 3 && s.contains("\n") {
                        errors.push("multiline verbatim string must be delimited by at least three backticks");
                    }
                } else {
                    s = &src.src[cur..];
                    errors.push("unexpected end of file in verbatim string");
                }
            },
            TokenKind::LAngleComment => {
                kind = TokenKind::Comment;
                if let Some(close_tok) = token_stream.find(|t| t.kind == TokenKind::RAngleComment) {
                    s = &src.src[cur..close_tok.span.end];
                } else {
                    s = &src.src[cur..];
                }
            },
            TokenKind::RAngleComment => {
                kind = TokenKind::RawText;
            },
            
            TokenKind::CloseTag | TokenKind::RAngleSlash => {
                open_tag = false;
            },
            _ => {},
        }
        
        let end = cur + s.len();
        let tok = Token {
            kind,
            range: SourceRange {
                src: src.clone(),
                start: cur as u32,
                end: end as u32,
            }
        };
        for msg in &errors {
            diagnostics.syntax_error(msg, &tok.range);
        }
        errors.clear();
        
        right_quote = match kind {
            TokenKind::Equals | TokenKind::LBrace | TokenKind::LPar | TokenKind::LSqb | TokenKind::Newline | TokenKind::Whitespace => false,
            TokenKind::RAngle | TokenKind::RAngleSlash => !open_tag,
            _ => true,
        };
        short_newline = kind == TokenKind::Comment;
        cur = end;
        
        if kind == TokenKind::Comment && strip_comments {
            continue;
        } else if let Some(prev_token) = tokens.last() {
            if tok.is_whitespace() && prev_token.is_whitespace() {
                if tok.kind == TokenKind::Newline {
                    tokens.pop();
                } else {
                    continue;
                }
            }
        }
        tokens.push(tok);
    }
    
    tokens
}
