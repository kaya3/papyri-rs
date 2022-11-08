use crate::utils::{SourceRange, ice};

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum TokenKind {
    Undetermined,
    
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
    
    OpenTag,
    CloseTag,
    
    LPar,
    RPar,
    LSqb,
    RSqb,
    LBrace,
    RBrace,
    LAngle,
    LAngleComment,
    RAngle,
    RAngleSlash,
    RAngleComment,
    
    Dot,
    Ellipsis,
    Comma,
    Equals,
    Colon,
    Tilde,
    Arrow,
    Bar,
    Asterisk,
    QuestionMark,
    At,
    Dollar,
    LQuote,
    RQuote,
    LDblQuote,
    RDblQuote,
}

pub struct Token {
    pub kind: TokenKind,
    pub range: SourceRange,
}

impl Token {
    pub fn as_str(&self) -> &str {
        self.range.as_str()
    }
    
    pub fn to_string(&self) -> String {
        self.as_str().to_string()
    }
    
    pub fn is_whitespace(&self) -> bool {
        self.kind == TokenKind::Whitespace || self.kind == TokenKind::Newline
    }
    
    pub fn is_primitive_type(&self) -> bool {
        matches!(self.as_str(), "any" | "none" | "html" | "block" | "inline" | "bool" | "int" | "str" | "function")
    }
    
    pub fn is_group_type(&self) -> bool {
        matches!(self.as_str(), "dict" | "list" | "?")
    }
    
    pub fn is_close_tag(&self, name: &str) -> bool {
        self.kind == TokenKind::CloseTag && {
            let s = self.as_str();
            s == "</>" || &s[2..s.len()-1].to_ascii_lowercase() == name
        }
    }
    
    pub fn get_bool_value(&self) -> bool {
        if self.kind != TokenKind::Boolean { ice("token is not Boolean"); }
        self.as_str() == "True"
    }
    
    pub fn bool_to_string(b: bool) -> &'static str {
        if b { "True" } else { "False" }
    }
    
    pub fn get_open_tag_name(&self) -> &str {
        if self.kind != TokenKind::OpenTag { ice("token is not OpenTag"); }
        &self.as_str()[1..]
    }
    
    pub fn get_func_name(&self) -> &str {
        match self.kind {
            TokenKind::FuncName => &self.as_str()[1..],
            _ => ice("token is not At or FuncName"),
        }
    }
    
    pub fn get_var_name(&self) -> &str {
        if self.kind != TokenKind::VarName { ice("token is not VarName"); }
        &self.as_str()[1..]
    }
    
    pub fn get_verbatim_text(&self) -> &str {
        if self.kind != TokenKind::Verbatim { ice("token is not Verbatim"); }
        let s = self.as_str();
        let mut i = 0;
        while s.chars().nth(i).unwrap() == '`' { i += 1; }
        &s[i..s.len()-i]
    }
    
    pub fn is_multiline_verbatim(&self) -> bool {
        if self.kind != TokenKind::Verbatim { ice("token is not Verbatim"); }
        self.as_str().starts_with("```")
    }
    
    pub fn text(&self) -> Option<String> {
        match self.kind {
            TokenKind::Name |
            TokenKind::Boolean |
            TokenKind::LPar |
            TokenKind::RPar |
            TokenKind::LAngle |
            TokenKind::RAngle |
            TokenKind::RAngleSlash |
            TokenKind::Dot |
            TokenKind::Comma |
            TokenKind::Equals |
            TokenKind::Colon |
            TokenKind::Asterisk |
            TokenKind::At |
            TokenKind::Dollar |
            TokenKind::QuestionMark |
            TokenKind::Arrow |
            TokenKind::RawText => Some(self.to_string()),
            
            TokenKind::Number => Some(self.as_str().replace("-", "\u{2212}")),
            TokenKind::Ellipsis => Some("\u{2026}".to_string()),
            TokenKind::LQuote => Some("\u{2018}".to_string()),
            TokenKind::RQuote => Some("\u{2019}".to_string()),
            TokenKind::LDblQuote => Some("\u{201C}".to_string()),
            TokenKind::RDblQuote => Some("\u{201D}".to_string()),
            
            // non-breaking space
            TokenKind::Tilde => Some("\u{00A0}".to_string()),
            // word joiner
            TokenKind::Bar => Some("\u{2060}".to_string()),
            
            _ => None,
        }
    }
}

impl std::fmt::Debug for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "Token({:?}, {:?}) at {}", self.kind, self.as_str(), self.range.str_start())
    }
}
