use std::rc::Rc;
use once_cell::sync::Lazy;

use crate::errors::{Diagnostics, SyntaxError};
use crate::utils::{SourceRange, SourceFile};
use super::token::{Token, TokenKind, QuoteDir, QuoteKind};

// Later patterns take priority
static LEXER: Lazy<regex_lexer::Lexer<Option<TokenKind>>> = Lazy::new(
    || regex_lexer::LexerBuilder::new()
        .token(r"<!--|-->|/>|->|\.\.\.|\*\*|.", None)
        .token(r"\s+", Some(TokenKind::Whitespace)) // or Newline
        .token(r"[a-zA-Z_][a-zA-Z0-9_]*", Some(TokenKind::Name))
        .token(r"-?[0-9]+", Some(TokenKind::Number))
        .token(r"</(?:[a-zA-Z_][a-zA-Z0-9_]*)?>", Some(TokenKind::CloseTag))
        .token(r"\\(?:x[0-9a-fA-F]{2}|u[0-9a-fA-F]{4}|U[0-9a-fA-F]{8}|[^xuU\n])", Some(TokenKind::Escape))
        .token(r"&(?:[a-zA-Z][a-zA-Z0-9]*|#[0-9]+|#x[0-9a-fA-F]+);", Some(TokenKind::Entity))
        .token(r"`+", Some(TokenKind::Verbatim))
        .token(r#"[^#\sa-zA-Z0-9_\\\-\(\)\[\]\{\}<>/\.,=:~\|!\?@\*\$&`'"]+"#, Some(TokenKind::RawText))
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
        let mut s = t.text;
        
        let kind = match t.kind {
            None => {
                match s {
                    "$" | "@" if matches!(token_stream.peek(), Some(tok) if tok.kind == Some(TokenKind::Name)) => {
                        let k = if s == "$" { TokenKind::VarName } else { TokenKind::FuncName };
                        let end = token_stream.next().unwrap().span.end;
                        s = &src.src[cur..end];
                        k
                    },
                    
                    "#" => {
                        // this would be much simpler if verbatims could be matched with backreferences
                        while matches!(token_stream.peek(), Some(tok) if tok.kind != Some(TokenKind::Whitespace) || !tok.text.contains('\n')) {
                            token_stream.next();
                        }
                        if matches!(token_stream.peek(), Some(tok) if !is_paragraph_break(tok.text)) {
                            token_stream.next();
                        }
                        s = match token_stream.peek() {
                            Some(tok) => &src.src[cur..tok.span.start],
                            None => &src.src[cur..],
                        };
                        TokenKind::Comment
                    },
                    
                    "<!--" => {
                        // this would be much simpler if verbatims could be matched with backreferences
                        let mut extra_dashes = 0;
                        let close_tok = loop {
                            match token_stream.next() {
                                Some(tok) if (
                                    tok.text == "-->"
                                    || extra_dashes >= 1 && tok.text == "->"
                                    || extra_dashes == 2 && tok.text == ">"
                                ) => break Some(tok),
                                None => break None,
                                Some(tok) => extra_dashes = if tok.text.ends_with("--") { 2 } else if tok.text.ends_with('-') { 1 } else { 0 },
                            }
                        };
                        s = match close_tok {
                            Some(close_tok) => &src.src[cur..close_tok.span.end],
                            None => &src.src[cur..],
                        };
                        TokenKind::Comment
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
                    "!" => TokenKind::ExclamationMark,
                    "?" => TokenKind::QuestionMark,
                    "*" | "**" => TokenKind::Asterisk,
                    "->" => TokenKind::Arrow,
                    "'" => TokenKind::Quote(QuoteKind::Single, quote_dir),
                    "\"" => TokenKind::Quote(QuoteKind::Double, quote_dir),
                    
                    _ => TokenKind::RawText,
                }
            },
            
            Some(TokenKind::Whitespace) => {
                if is_paragraph_break(s) { TokenKind::Newline } else { TokenKind::Whitespace }
            },
            
            Some(TokenKind::Name) => match s {
                "True" | "False" => TokenKind::Boolean,
                _ => TokenKind::Name,
            },
            
            Some(TokenKind::Verbatim) => {
                // if `regex_lexer` supported backreferences, this would be a lot simpler
                let open_backticks = t.text.len();
                let mut extra_backtick = 0;
                let close_tok = loop {
                    match token_stream.next() {
                        Some(tok) if tok.kind == Some(TokenKind::Verbatim) => {
                            if tok.text.len() >= open_backticks {
                                break Some(tok);
                            }
                        },
                        Some(tok) if tok.kind == Some(TokenKind::Escape) && tok.text.ends_with('`') => {
                            if open_backticks == 1 {
                                break Some(tok);
                            } else if matches!(token_stream.peek(), Some(t) if t.kind == Some(TokenKind::Verbatim) && t.text.len() + 1 >= open_backticks) {
                                extra_backtick = 1;
                                break token_stream.next();
                            }
                        },
                        None => break None,
                        
                        _ => {},
                    }
                };
                
                if let Some(close_tok) = close_tok {
                    s = &src.src[cur..close_tok.span.end];
                    if close_tok.kind == Some(TokenKind::Verbatim) && close_tok.text.len() + extra_backtick > open_backticks {
                        error_kind = Some(SyntaxError::TokenVerbatimTooManyBackticks);
                    } else if open_backticks < 3 && s.contains('\n') {
                        error_kind = Some(SyntaxError::TokenVerbatimMultilineNotEnoughBackticks);
                    }
                } else {
                    s = &src.src[cur..];
                    error_kind = Some(SyntaxError::TokenVerbatimEOF);
                }
                TokenKind::Verbatim
            },
            
            Some(kind) => kind,
        };
        
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
