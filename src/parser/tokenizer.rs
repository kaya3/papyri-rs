use std::rc::Rc;
use once_cell::sync::Lazy;

use crate::errors::{Diagnostics, SyntaxError};
use crate::utils::{SourceRange, SourceFile};
use super::token::{Token, TokenKind, QuoteDir, QuoteKind};

// Later patterns take priority
static LEXER: Lazy<regex_lexer::Lexer<TokenKind>> = Lazy::new(
    || regex_lexer::LexerBuilder::new()
        .token(r"<!--|/>|->|\.\.\.|\*\*|.", TokenKind::Undetermined)
        .token(r"\s+", TokenKind::Whitespace) // or Newline
        .token(r"[a-zA-Z_][a-zA-Z0-9_]*", TokenKind::Name)
        .token(r"-?[0-9]+", TokenKind::Number)
        .token(r"</(?:[a-zA-Z_][a-zA-Z0-9_]*)?>", TokenKind::CloseTag)
        .token(r"\\(?:x[0-9a-fA-F]{2}|u[0-9a-fA-F]{4}|U[0-9a-fA-F]{8}|[^xuU])", TokenKind::Escape)
        .token(r"&(?:[a-zA-Z][a-zA-Z0-9]*|#[0-9]+|#x[0-9a-fA-F]+);", TokenKind::Entity)
        .token(r"`+", TokenKind::Verbatim)
        // need to ensure that `<!-- \-->` recognises the end of the comment
        .token(r"\\?-->", TokenKind::RAngleComment)
        .token(r#"[^#\sa-zA-Z0-9_\\\-\(\)\[\]\{\}<>/\.,=:~\|\?@\*\$&`'"]+"#, TokenKind::RawText)
        .build()
        .expect("Failed to build regex_lexer")
);

fn is_paragraph_break(s: &str) -> bool {
    s.chars().filter(|c| *c == '\n').count() >= 2
}

/// Tokenizes the given Papyri source file.
pub fn tokenize(src: Rc<SourceFile>, strip_comments: bool, diagnostics: &mut Diagnostics) -> Vec<Token> {
    // The current index in the source string. Used to generate token spans.
    let mut cur = 0;
    
    // Indicates whether a quote `'` or `"` should become a left or right quote.
    let mut quote_dir = QuoteDir::Left;
    
    // Holds an error to be emitted for the current token; this cannot be sent
    // to `diagnostics` until a span is known.
    let mut error_kind: Option<SyntaxError> = None;
    
    // Collects the output tokens.
    let mut tokens: Vec<Token> = Vec::new();
    
    let mut token_stream = LEXER.tokens(&src.src).peekable();
    while let Some(t) = token_stream.next() {
        let mut kind = t.kind;
        let mut s = t.text;
        
        match kind {
            TokenKind::Undetermined => {
                kind = match s {
                    "<!--" => {
                        if let Some(close_tok) = token_stream.find(|t| t.kind == TokenKind::RAngleComment) {
                            s = &src.src[cur..close_tok.span.end];
                        } else {
                            // no syntax error here; an unmatched `<!--` means
                            // the rest of the file is commented
                            s = &src.src[cur..];
                        }
                        TokenKind::Comment
                    },
                    
                    "#" => {
                        while let Some(t) = token_stream.peek() {
                            if t.kind == TokenKind::Whitespace && t.text.contains('\n') { break; }
                            token_stream.next();
                        }
                        
                        if let Some(close_tok) = token_stream.peek() {
                            if is_paragraph_break(close_tok.text) {
                                s = &src.src[cur..close_tok.span.start];
                            } else {
                                s = &src.src[cur..close_tok.span.end];
                                token_stream.next();
                            }
                        } else {
                            // no syntax error here; the rest of the file is commented
                            s = &src.src[cur..];
                        }
                        TokenKind::Comment
                    },
                    
                    "$" | "@" if matches!(token_stream.peek(), Some(tok) if tok.kind == TokenKind::Name) => {
                        let k = if s == "$" { TokenKind::VarName } else { TokenKind::FuncName };
                        let end = token_stream.next().unwrap().span.end;
                        s = &src.src[cur..end];
                        k
                    },
                    
                    "(" => TokenKind::LPar,
                    ")" => TokenKind::RPar,
                    "[" => TokenKind::LSqb,
                    "]" => TokenKind::RSqb,
                    "{" => TokenKind::LBrace,
                    "}" => TokenKind::RBrace,
                    "<" => TokenKind::LAngle,
                    ">" | "/>" => TokenKind::RAngle,
                    "." => TokenKind::Dot,
                    "..." => TokenKind::Ellipsis,
                    "," => TokenKind::Comma,
                    "=" => TokenKind::Equals,
                    ":" => TokenKind::Colon,
                    "|" => TokenKind::Bar,
                    "?" => TokenKind::QuestionMark,
                    "*" | "**" => TokenKind::Asterisk,
                    "->" => TokenKind::Arrow,
                    "'" => TokenKind::Quote(QuoteKind::Single, quote_dir),
                    "\"" => TokenKind::Quote(QuoteKind::Double, quote_dir),
                    
                    _ => TokenKind::RawText,
                };
            },
            
            TokenKind::Name if matches!(s, "True" | "False") => {
                kind = TokenKind::Boolean;
            },
            
            TokenKind::Whitespace => {
                if is_paragraph_break(s) { kind = TokenKind::Newline; }
            },
            
            TokenKind::Verbatim => {
                let backticks = s.len();
                if let Some(close_tok) = token_stream.find(|t| t.kind == TokenKind::Verbatim && t.text.len() >= backticks) {
                    s = &src.src[cur..close_tok.span.end];
                    if close_tok.text.len() > backticks {
                        error_kind = Some(SyntaxError::TokenVerbatimTooManyBackticks);
                    } else if backticks < 3 && s.contains('\n') {
                        error_kind = Some(SyntaxError::TokenVerbatimMultilineNotEnoughBackticks);
                    }
                } else {
                    s = &src.src[cur..];
                    error_kind = Some(SyntaxError::TokenVerbatimEOF);
                }
            },
            TokenKind::RAngleComment => {
                kind = TokenKind::RawText;
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
        
        if let Some(msg) = error_kind {
            diagnostics.syntax_error(msg, &tok.range);
            error_kind = None;
        }
        
        quote_dir = match kind {
            TokenKind::Equals | TokenKind::LBrace | TokenKind::LPar | TokenKind::LSqb | TokenKind::RAngle | TokenKind::Newline | TokenKind::Whitespace => QuoteDir::Left,
            _ => QuoteDir::Right,
        };
        cur = end;
        
        if kind == TokenKind::Comment && strip_comments {
            continue;
        } else if let Some(prev_token) = tokens.last() {
            // Ensure that there are never two consecutive whitespace tokens in
            // the output. This can happen if a comment token is removed from
            // between two whitespace tokens.
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
