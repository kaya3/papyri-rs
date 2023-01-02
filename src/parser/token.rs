//! This module contains declarations for tokens in the Papyri language grammar.

use crate::utils::sourcefile::SourceRange;

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
    Keyword(Keyword),
    FuncName,
    VarName,
    Number,
    Boolean(bool),
    Verbatim(VerbatimKind),
    
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
    DoubleAsterisk,
    DoubleColon,
    Ellipsis,
    Equals,
    ExclamationMark,
    QuestionMark,
    QuestionMarkDoubleColon,
    Quote(QuoteKind, QuoteDir),
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
#[allow(missing_docs)]
pub enum VerbatimKind {Simple, Multiline}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
#[allow(missing_docs)]
pub enum Keyword {Export, Fn, Implicit, Let, Match}

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
            TokenKind::Keyword(..) => "keyword",
            TokenKind::FuncName => "function name",
            TokenKind::VarName => "variable name",
            TokenKind::Number => "int literal",
            TokenKind::Boolean(..) => "bool literal",
            TokenKind::Verbatim(..) => "string literal",
            TokenKind::CloseTag => "closing tag",
            TokenKind::LPar => "'('",
            TokenKind::RPar => "')'",
            TokenKind::LSqb => "'['",
            TokenKind::RSqb => "']'",
            TokenKind::LBrace => "'{'",
            TokenKind::RBrace => "'}'",
            TokenKind::LAngle => "'<'",
            TokenKind::RAngle => "'>'",
            TokenKind::Ampersand => "'&'",
            TokenKind::Arrow => "'->'",
            TokenKind::Asterisk => "'*'",
            TokenKind::Bar => "'|'",
            TokenKind::Colon => "':'",
            TokenKind::Comma => "','",
            TokenKind::Dot => "'.'",
            TokenKind::DoubleAsterisk => "'**'",
            TokenKind::DoubleColon => "'::'",
            TokenKind::Ellipsis => "ellipsis",
            TokenKind::Equals => "'='",
            TokenKind::ExclamationMark => "'!'",
            TokenKind::QuestionMark => "'?'",
            TokenKind::QuestionMarkDoubleColon => "'?::'",
            TokenKind::Quote(k, _) => match k {
                QuoteKind::Single => "single-quote",
                QuoteKind::Double => "double-quote",
            },
        })
    }
}

#[derive(Debug, Clone, Copy)]
/// A concrete token which occurs at some position in a Papyri source file.
pub struct Token {
    /// The kind of token this is.
    pub kind: TokenKind,
    
    /// The source span of this token.
    pub range: SourceRange,
}

impl Token {
    /// Indicates whether this token is a whitespace or newline token.
    pub(crate) fn is_whitespace(&self) -> bool {
        self.kind == TokenKind::Whitespace || self.kind == TokenKind::Newline
    }
    
    /// Converts a `bool` value to the source of a Boolean literal token. This
    /// is the inverse of `get_bool_value`.
    pub(crate) fn bool_to_string(b: bool) -> &'static str {
        if b { "True" } else { "False" }
    }
    
    /// Returns the raw text of a Verbatim token.
    pub(crate) fn get_verbatim_text(s: &str) -> &str {
        s.trim_matches('`')
    }
}
