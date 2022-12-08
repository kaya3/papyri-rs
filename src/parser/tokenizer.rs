use std::rc::Rc;

use crate::errors;
use crate::utils::{SourceRange, SourceFile};
use super::token::{Token, TokenKind, QuoteDir, QuoteKind};

/// Tokenizes the given Papyri source file.
pub fn tokenize(src: Rc<SourceFile>, strip_comments: bool, diagnostics: &mut errors::Diagnostics) -> Vec<Token> {
    // The current index in the source string. Used to generate token spans.
    let mut cur = 0;
    
    // Indicates whether a quote `'` or `"` should become a left or right quote.
    let mut quote_dir = QuoteDir::Left;
    
    // Holds an error to be emitted for the current token; this cannot be sent
    // to `diagnostics` until a span is known.
    let mut error_kind: Option<errors::SyntaxError> = None;
    
    // Collects the output tokens.
    let mut tokens: Vec<Token> = Vec::new();
    
    let src_str = src.src.as_ref();
    while cur < src_str.len() {
        let (len, mut kind) = next_token(&src_str[cur..], |e| error_kind = Some(e));
        if len == 0 { errors::ice("next_token returned empty length"); }
        
        if let TokenKind::Quote(_, ref mut dir) = kind {
            *dir = quote_dir;
        }
        
        let end = cur + len;
        let tok = Token {
            kind,
            range: SourceRange {
                src: src.clone(),
                start: cur as u32,
                end: end as u32,
            }
        };
        
        if let Some(msg) = std::mem::take(&mut error_kind) {
            diagnostics.syntax_error(msg, &tok.range);
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

fn next_token(src: &str, mut on_error: impl FnMut(errors::SyntaxError)) -> (usize, TokenKind) {
    let mut chars = src.chars();
    let Some(first_char) = chars.next() else {
        errors::ice("next_token called at end of source")
    };
    
    match first_char {
        '#' => {
            let i = 1 + scan_while(&src[1..], |c| c != '\n');
            let j = i + scan_while(&src[i..], |c| c.is_ascii_whitespace());
            let len = if is_paragraph_break(&src[i..j]) { i } else { j };
            (len, TokenKind::Comment)
        },
        
        c if c.is_ascii_whitespace() => {
            let len = 1 + scan_while(&src[1..], |c: char| c.is_ascii_whitespace());
            let kind = if is_paragraph_break(&src[..len]) { TokenKind::Newline } else { TokenKind::Whitespace };
            (len, kind)
        },
        
        c if is_ident_start(c) => {
            let len = 1 + scan_while(&src[1..], is_ident_cont);
            let kind = match &src[..len] {
                "True" | "False" => TokenKind::Boolean,
                _ => TokenKind::Name,
            };
            (len, kind)
        }
        
        '0'..='9' => {
            (1 + scan_while(&src[1..], |c| c.is_ascii_digit()), TokenKind::Number)
        }
        
        '$' | '@' if matches!(chars.next(), Some(c) if is_ident_start(c)) => {
            let len = 1 + scan_while(&src[1..], is_ident_cont);
            let kind = if first_char == '$' { TokenKind::VarName } else { TokenKind::FuncName };
            (len, kind)
        },
        
        '`' => {
            let open_delim_len = 1 + scan_while(&src[1..], |c| c == '`');
            let len = match src[open_delim_len..].find(&src[..open_delim_len]) {
                Some(content_len) => {
                    let close_delim_start = open_delim_len + content_len;
                    let close_delim_len = open_delim_len + scan_while(&src[close_delim_start + open_delim_len..], |c| c == '`');
                    if close_delim_len > open_delim_len {
                        on_error(errors::SyntaxError::TokenVerbatimTooManyBackticks);
                    } else if open_delim_len < 3 && src[open_delim_len..close_delim_start].contains('\n') {
                        on_error(errors::SyntaxError::TokenVerbatimMultilineNotEnoughBackticks);
                    }
                    close_delim_start + close_delim_len
                },
                None => {
                    on_error(errors::SyntaxError::TokenVerbatimEOF);
                    src.len()
                },
            };
            (len, TokenKind::Verbatim)
        },
        
        '<' => match chars.next() {
            Some('!') if src.starts_with("<!--") => {
                let len = match src[4..].find("-->") {
                    Some(content_len) => content_len + 7,
                    // no syntax error here; comment is allowed to reach to the end of the source
                    None => src.len(),
                };
                (len, TokenKind::Comment)
            },
            Some('/') => {
                if matches!(chars.next(), Some(c) if is_ident_start(c)) {
                    let ident_len = 1 + scan_while(&src[3..], is_ident_cont);
                    if src[2 + ident_len..].starts_with(">") {
                        (3 + ident_len, TokenKind::CloseTag)
                    } else {
                        on_error(errors::SyntaxError::TagCloseMalformed);
                        (2 + ident_len, TokenKind::RawText)
                    }
                } else if src.starts_with("</>") {
                    (3, TokenKind::CloseTag)
                } else {
                    on_error(errors::SyntaxError::TagCloseMalformed);
                    (2, TokenKind::RawText)
                }
            },
            _ => (1, TokenKind::LAngle),
        },
        
        '-' if src.starts_with("->") => (2, TokenKind::Arrow),
        '-' if matches!(chars.next(), Some('0'..='9')) => {
            let len = 2 + scan_while(&src[2..], |c| c.is_ascii_digit());
            (len, TokenKind::Number)
        },
        
        '(' => (1, TokenKind::LPar),
        ')' => (1, TokenKind::RPar),
        '[' => (1, TokenKind::LSqb),
        ']' => (1, TokenKind::RSqb),
        '{' => (1, TokenKind::LBrace),
        '}' => (1, TokenKind::RBrace),
        '>' => (1, TokenKind::RAngle),
        '/' if src.starts_with("/>") => (2, TokenKind::RAngle),
        
        '.' => if src.starts_with("...") { (3, TokenKind::Ellipsis) } else { (1, TokenKind::Dot) },
        ',' => (1, TokenKind::Comma),
        '=' => (1, TokenKind::Equals),
        ':' => if src.starts_with("::") { (2, TokenKind::DoubleColon) } else { (1, TokenKind::Colon) },
        '?' => if src.starts_with("?::") { (3, TokenKind::DoubleColon) } else { (1, TokenKind::QuestionMark) },
        '|' => (1, TokenKind::Bar),
        '!' => (1, TokenKind::ExclamationMark),
        '*' => (if src.starts_with("**") { 2 } else { 1 }, TokenKind::Asterisk),
        '\'' => (1, TokenKind::Quote(QuoteKind::Single, QuoteDir::Left)),
        '"' => (1, TokenKind::Quote(QuoteKind::Double, QuoteDir::Left)),
        
        '\\' => {
            let next_char = chars.next();
            match next_char {
                Some('\n') => {
                    on_error(errors::SyntaxError::TokenInvalidEscape);
                    (1, TokenKind::Escape)
                },
                Some(c) => if matches!(c, 'x' | 'u' | 'U') {
                    // expected token length, including the `\x` | `\u` | `\U`
                    let end = if c == 'x' { 4 } else if c == 'u' { 6 } else { 10 };
                    let len = if src.len() >= end && src[2..end].chars().all(|c| c.is_ascii_hexdigit()) {
                        end
                    } else {
                        on_error(errors::SyntaxError::TokenInvalidEscape);
                        2
                    };
                    (len, TokenKind::Escape)
                } else {
                    (1 + c.len_utf8(), TokenKind::Escape)
                },
                None => (1, TokenKind::RawText),
            }
        },
        
        '&' => {
            let mut len = 1 + scan_while(&src[1..], |c| !c.is_ascii_whitespace() && c != ';');
            if src[len..].starts_with(";") {
                len += 1;
            } else {
                on_error(errors::SyntaxError::TokenEntityMissingSemicolon);
            }
            (len, TokenKind::Entity)
        },
        
        _ => {
            let char_len = first_char.len_utf8();
            let len = char_len + scan_while(&src[char_len..], is_raw_text);
            (len, TokenKind::RawText)
        },
    }
}

fn scan_while(src: &str, f: impl Fn(char) -> bool) -> usize {
    src.find(|c| !f(c))
        .unwrap_or(src.len())
}

fn is_ident_start(c: char) -> bool {
    matches!(c, 'A'..='Z' | 'a'..='z' | '_')
}

fn is_ident_cont(c: char) -> bool {
    matches!(c, 'A'..='Z' | 'a'..='z' | '0'..='9' | '_')
}

fn is_paragraph_break(s: &str) -> bool {
    s.chars().filter(|c| *c == '\n').count() >= 2
}

fn is_raw_text(c: char) -> bool {
    match c {
        '(' | ')' | '[' | ']' | '{' | '}' | '<' | '>' |
        '.' | ',' | '-' | '=' | ':' | '*' | '!' | '?' | '/' |
        '|' | '#' | '@' | '$' | '&' | '\\' | '`' | '\'' | '"' => false,
        _ => !is_ident_cont(c) && !c.is_ascii_whitespace(),
    }
}

#[cfg(test)]
mod test {
    use super::{errors, TokenKind, next_token};
    
    fn no_err(_: errors::SyntaxError) {
        panic!()
    }
    
    #[test]
    fn hash_comment() {
        assert_eq!(next_token("#foo\nbar", no_err), (5, TokenKind::Comment));
        assert_eq!(next_token("#foo\n  bar", no_err), (7, TokenKind::Comment));
        assert_eq!(next_token("#foo\n\nbar", no_err), (4, TokenKind::Comment));
    }
    
    #[test]
    fn html_comment() {
        assert_eq!(next_token("<!-- foo --> bar", no_err), (12, TokenKind::Comment));
        assert_eq!(next_token("<!--\n--> bar", no_err), (8, TokenKind::Comment));
        assert_eq!(next_token("<!-- ```--> bar", no_err), (11, TokenKind::Comment));
        assert_eq!(next_token(r"<!-- \--> bar", no_err), (9, TokenKind::Comment));
        assert_eq!(next_token(r"<!-- foo <!--> bar", no_err), (14, TokenKind::Comment));
        assert_eq!(next_token("<!-- foo bar baz", no_err), (16, TokenKind::Comment));
    }
    
    #[test]
    fn whitespace() {
        assert_eq!(next_token("   bar", no_err), (3, TokenKind::Whitespace));
        assert_eq!(next_token("\n bar", no_err), (2, TokenKind::Whitespace));
        assert_eq!(next_token("\t\t\t\nbar", no_err), (4, TokenKind::Whitespace));
    }
    
    #[test]
    fn paragraph_break() {
        assert_eq!(next_token("\n\nbar", no_err), (2, TokenKind::Newline));
        assert_eq!(next_token("\n    \nbar", no_err), (6, TokenKind::Newline));
        assert_eq!(next_token("\n\n\n\nbar", no_err), (4, TokenKind::Newline));
    }
    
    #[test]
    fn escape() {
        assert_eq!(next_token(r"\n bar", no_err), (2, TokenKind::Escape));
        assert_eq!(next_token(r"\xeF bar", no_err), (4, TokenKind::Escape));
        assert_eq!(next_token(r"\uCdeF bar", no_err), (6, TokenKind::Escape));
        assert_eq!(next_token(r"\U89AbcDEF bar", no_err), (10, TokenKind::Escape));
    }
    
    #[test]
    fn entity() {
        assert_eq!(next_token("&lt; bar", no_err), (4, TokenKind::Entity));
        assert_eq!(next_token("&#97; bar", no_err), (5, TokenKind::Entity));
        assert_eq!(next_token("&#xA0; bar", no_err), (6, TokenKind::Entity));
    }
    
    #[test]
    fn name() {
        assert_eq!(next_token("foo bar", no_err), (3, TokenKind::Name));
        assert_eq!(next_token("_foo bar", no_err), (4, TokenKind::Name));
        assert_eq!(next_token("foo23 bar", no_err), (5, TokenKind::Name));
    }
    
    #[test]
    fn var_name() {
        assert_eq!(next_token("$foo bar", no_err), (4, TokenKind::VarName));
        assert_eq!(next_token("$_foo bar", no_err), (5, TokenKind::VarName));
        assert_eq!(next_token("$foo23 bar", no_err), (6, TokenKind::VarName));
    }
    
    #[test]
    fn func_name() {
        assert_eq!(next_token("@foo bar", no_err), (4, TokenKind::FuncName));
        assert_eq!(next_token("@_foo bar", no_err), (5, TokenKind::FuncName));
        assert_eq!(next_token("@foo23 bar", no_err), (6, TokenKind::FuncName));
    }
    
    #[test]
    fn number() {
        assert_eq!(next_token("0 bar", no_err), (1, TokenKind::Number));
        assert_eq!(next_token("123 bar", no_err), (3, TokenKind::Number));
        assert_eq!(next_token("-123 bar", no_err), (4, TokenKind::Number));
    }
    
    #[test]
    fn boolean() {
        assert_eq!(next_token("True bar", no_err), (4, TokenKind::Boolean));
        assert_eq!(next_token("False bar", no_err), (5, TokenKind::Boolean));
    }
    
    #[test]
    fn string_literal() {
        assert_eq!(next_token("`foo` bar", no_err), (5, TokenKind::Verbatim));
        assert_eq!(next_token("``foo ` bar`` baz", no_err), (13, TokenKind::Verbatim));
        assert_eq!(next_token("```foo\nbar\nbaz``` qux", no_err), (17, TokenKind::Verbatim));
    }
    
    #[test]
    fn parens() {
        assert_eq!(next_token("(bar", no_err), (1, TokenKind::LPar));
        assert_eq!(next_token(")bar", no_err), (1, TokenKind::RPar));
        assert_eq!(next_token("[bar", no_err), (1, TokenKind::LSqb));
        assert_eq!(next_token("]bar", no_err), (1, TokenKind::RSqb));
        assert_eq!(next_token("{bar", no_err), (1, TokenKind::LBrace));
        assert_eq!(next_token("}bar", no_err), (1, TokenKind::RBrace));
    }
    
    #[test]
    fn tags() {
        assert_eq!(next_token("<bar", no_err), (1, TokenKind::LAngle));
        assert_eq!(next_token(">bar", no_err), (1, TokenKind::RAngle));
        assert_eq!(next_token("/>bar", no_err), (2, TokenKind::RAngle));
        assert_eq!(next_token("</>bar", no_err), (3, TokenKind::CloseTag));
        assert_eq!(next_token("</foo>bar", no_err), (6, TokenKind::CloseTag));
    }
    
    #[test]
    fn punctuation() {
        assert_eq!(next_token(". bar", no_err), (1, TokenKind::Dot));
        assert_eq!(next_token("... bar", no_err), (3, TokenKind::Ellipsis));
        assert_eq!(next_token(", bar", no_err), (1, TokenKind::Comma));
        assert_eq!(next_token("= bar", no_err), (1, TokenKind::Equals));
        assert_eq!(next_token(": bar", no_err), (1, TokenKind::Colon));
        assert_eq!(next_token(":: bar", no_err), (2, TokenKind::DoubleColon));
        assert_eq!(next_token("?:: bar", no_err), (3, TokenKind::DoubleColon));
        assert_eq!(next_token("-> bar", no_err), (2, TokenKind::Arrow));
        assert_eq!(next_token("| bar", no_err), (1, TokenKind::Bar));
        assert_eq!(next_token("* bar", no_err), (1, TokenKind::Asterisk));
        assert_eq!(next_token("** bar", no_err), (2, TokenKind::Asterisk));
        assert_eq!(next_token("! bar", no_err), (1, TokenKind::ExclamationMark));
        assert_eq!(next_token("? bar", no_err), (1, TokenKind::QuestionMark));
    }
    
    #[test]
    fn raw_text() {
        assert_eq!(next_token("^~% bar", no_err), (3, TokenKind::RawText));
        assert_eq!(next_token("^~% bar", no_err), (3, TokenKind::RawText));
    }
}
