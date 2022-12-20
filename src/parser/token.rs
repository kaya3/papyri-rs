use crate::errors::ice_at;
use crate::utils::SourceRange;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
#[allow(missing_docs)]
/// A kind of token in the Papyri language grammar.
pub enum TokenKind {
    Comment,
    Whitespace,
    Newline,
    
    Escape,
    Entity,
    RawText,
    
    Name,
    FuncName,
    VarName,
    Number,
    Boolean,
    Verbatim,
    
    LPar,
    RPar,
    LSqb,
    RSqb,
    LBrace,
    RBrace,
    LAngle,
    RAngle,
    CloseTag,
    
    Ampersand,
    Arrow,
    Asterisk,
    Bar,
    Colon,
    Comma,
    Dot,
    DoubleColon,
    Ellipsis,
    Equals,
    ExclamationMark,
    QuestionMark,
    Quote(QuoteKind, QuoteDir),
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
#[allow(missing_docs)]
/// A kind of quote token; either single (`'`) or double (`"`).
pub enum QuoteKind {Single, Double}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
#[allow(missing_docs)]
/// The direction a quote character should face (either left or right).
pub enum QuoteDir {Left, Right}

impl std::fmt::Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            TokenKind::Comment => "comment",
            TokenKind::Whitespace => "whitespace",
            TokenKind::Newline => "paragraph break",
            TokenKind::Escape => "escape sequence",
            TokenKind::Entity => "entity",
            TokenKind::RawText => "text",
            TokenKind::Name => "name",
            TokenKind::FuncName => "function name",
            TokenKind::VarName => "variable name",
            TokenKind::Number => "number literal",
            TokenKind::Boolean => "bool literal",
            TokenKind::Verbatim => "string literal",
            TokenKind::CloseTag => "closing tag",
            TokenKind::LPar => "'('",
            TokenKind::RPar => "')'",
            TokenKind::LSqb => "'['",
            TokenKind::RSqb => "']'",
            TokenKind::LBrace => "'{'",
            TokenKind::RBrace => "'}'",
            TokenKind::LAngle => "'<'",
            TokenKind::RAngle => "'>' or '/>'",
            TokenKind::Ampersand => "'&'",
            TokenKind::Arrow => "'->'",
            TokenKind::Asterisk => "'*' or '**'",
            TokenKind::Bar => "'|'",
            TokenKind::Colon => "':'",
            TokenKind::Comma => "','",
            TokenKind::Dot => "'.'",
            TokenKind::DoubleColon => "'::'",
            TokenKind::Ellipsis => "ellipsis",
            TokenKind::Equals => "'='",
            TokenKind::ExclamationMark => "'!'",
            TokenKind::QuestionMark => "'?'",
            TokenKind::Quote(k, _) => match k {
                QuoteKind::Single => "single-quote",
                QuoteKind::Double => "double-quote",
            },
        })
    }
}

#[derive(Clone)]
/// A concrete token which occurs at some position in a Papyri source file.
pub struct Token {
    /// The kind of token this is.
    pub kind: TokenKind,
    
    /// The source span of this token.
    pub range: SourceRange,
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // some TokenKind descriptions are like `'*' or '**'`, which is not
        // helpful for describing an actual token
        match self.kind {
            TokenKind::RAngle |
            TokenKind::Asterisk |
            TokenKind::DoubleColon => write!(f, "'{}'", self.as_str()),
            _ => std::fmt::Display::fmt(&self.kind, f),
        }
    }
}

impl Token {
    /// Returns this token's source as a borrowed string.
    pub fn as_str(&self) -> &str {
        self.range.as_str()
    }
    
    /// Indicates whether this token is a whitespace or newline token.
    pub fn is_whitespace(&self) -> bool {
        self.kind == TokenKind::Whitespace || self.kind == TokenKind::Newline
    }
    
    /// Indicates whether this token is a keyword function name.
    pub fn is_keyword_func(&self) -> bool {
        matches!(self.kind, TokenKind::FuncName)
            && matches!(self.as_str(), "@export" | "@fn" | "@implicit" | "@let" | "@match")
    }
    
    /// Indicates whether this token is a closing tag for the given tag name.
    /// If no tag name is given, only `</>` matches.
    pub fn is_close_tag(&self, name: &Option<String>) -> bool {
        self.kind == TokenKind::CloseTag && {
            let s = self.as_str();
            s == "</>" || matches!(name, Some(t) if t.eq_ignore_ascii_case(&s[2..s.len() - 1]))
        }
    }
    
    /// Returns the `bool` value of a Boolean literal token.
    pub fn get_bool_value(&self) -> bool {
        if self.kind != TokenKind::Boolean { ice_at("token is not Boolean", &self.range); }
        self.as_str() == "True"
    }
    
    /// Converts a `bool` value to the source of a Boolean literal token. This
    /// is the inverse of `get_bool_value`.
    pub fn bool_to_string(b: bool) -> &'static str {
        if b { "True" } else { "False" }
    }
    
    /// Returns the name from this VarName or FuncName token.
    pub fn get_var_name(&self) -> &str {
        if !matches!(self.kind, TokenKind::VarName | TokenKind::FuncName) { ice_at("token is not VarName or FuncName", &self.range); }
        &self.as_str()[1..]
    }
    
    /// Returns the raw text of this Verbatim token.
    pub fn get_verbatim_text(&self) -> &str {
        if self.kind != TokenKind::Verbatim { ice_at("token is not Verbatim", &self.range); }
        self.as_str().trim_matches('`')
    }
    
    /// Indicates whether this Verbatim token is a multi-line string literal.
    pub fn is_multiline_verbatim(&self) -> bool {
        if self.kind != TokenKind::Verbatim { ice_at("token is not Verbatim", &self.range); }
        self.as_str().starts_with("```")
    }
    
    /// Returns the representation of this token as normal text, if it has one.
    pub fn text(&self) -> Option<&str> {
        match self.kind {
            TokenKind::Name |
            TokenKind::Number |
            TokenKind::Boolean |
            TokenKind::LPar |
            TokenKind::RPar |
            TokenKind::LAngle |
            TokenKind::RAngle |
            TokenKind::Ampersand |
            TokenKind::Arrow |
            TokenKind::Asterisk |
            TokenKind::Colon |
            TokenKind::Comma |
            TokenKind::Dot |
            TokenKind::DoubleColon |
            TokenKind::Equals |
            TokenKind::ExclamationMark |
            TokenKind::QuestionMark |
            TokenKind::RawText => Some(self.as_str()),
            
            TokenKind::Ellipsis => Some("\u{2026}"),
            
            TokenKind::Quote(q, d) => Some(match (q, d) {
                // single quotes
                (QuoteKind::Single, QuoteDir::Left) => "\u{2018}",
                (QuoteKind::Single, QuoteDir::Right) => "\u{2019}",
                // double quotes
                (QuoteKind::Double, QuoteDir::Left) => "\u{201C}",
                (QuoteKind::Double, QuoteDir::Right) => "\u{201D}",
            }),
            
            // word joiner
            TokenKind::Bar => Some("\u{2060}"),
            
            _ => None,
        }
    }
}

impl std::fmt::Debug for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "Token({:?}, {:?}) at {}", self.kind, self.as_str(), self.range.str_start())
    }
}
