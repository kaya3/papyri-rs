use crate::parser::{TokenKind, Token};

#[allow(missing_docs)]
/// Represents a syntax error in a Papyri source file.
pub enum SyntaxError {
    TokenExpected(TokenKind),
    TokenExpectedDoctype,
    TokenExpectedWas(TokenKind, Token),
    TokenExpectedWasEOF(TokenKind),
    TokenUnexpected(Token),
    TokenUnmatched(Token),
    TokenInvalidNumber(std::num::ParseIntError),
    TokenInvalidEntity,
    TokenInvalidEscape,
    TokenInvalidPrimitiveType,
    TokenVerbatimMultilineNotEnoughBackticks,
    TokenVerbatimTooManyBackticks,
    TokenVerbatimEOF,
    
    ExpectedValue,
    UnexpectedEOF,
    
    TagUnmatchedOpen,
    TagUnmatchedClose,
    TagDuplicateAttr(String),
    
    SpreadNotAllowed,
    SpreadPositionalNotAllowed,
    SpreadNamedNotAllowed,
    
    ParamDuplicateName(String),
    ParamPositionalAfterNamed,
    ParamRequiredAfterOptional,
    ParamDefaultImplicit,
    ParamPositionalImplicit,
    ParamSpreadDefault,
    ParamSpreadImplicit,
    ParamMultipleSpread,
    ParamAfterSpread,
    ParamPositionalSpreadNoUnderscore,
    ParamNamedSpreadUnderscore,
    ParamContentSpread,
    ParamContentDefault,
    
    ArgDuplicateName(String),
    ArgNamedNotAllowed,
    ArgPositionalAfterNamed,
    ArgSpreadNamed,
    ArgNamedUnderscore,
    
    LetInMissingArgs,
    LetInPositionalArg,
    LetInLiteral,
    
    PatternBareName,
    PatternMultipleSpreads,
    PatternNamedUnderscore,
    PatternNamedAfterSpread,
    PatternDuplicateName(String),
    PatternIncorrectCloseTag,
    PatternCannotMatchHTML,
    PatternAttrAccess,
}

impl std::fmt::Display for SyntaxError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SyntaxError::TokenExpected(kind) => write!(f, "expected {kind}"),
            SyntaxError::TokenExpectedDoctype => f.write_str("expected 'DOCTYPE'"),
            SyntaxError::TokenExpectedWas(kind, token) => write!(f, "expected {kind}, was {token}"),
            SyntaxError::TokenExpectedWasEOF(kind) => write!(f, "expected {kind}, was end of course"),
            SyntaxError::TokenUnexpected(token) => write!(f, "unexpected {token}"),
            SyntaxError::TokenUnmatched(token) => write!(f, "unmatched {token}"),
            SyntaxError::TokenInvalidNumber(e) => write!(f, "invalid number ({e})"),
            SyntaxError::TokenInvalidEntity => f.write_str("invalid entity"),
            SyntaxError::TokenInvalidEscape => f.write_str("invalid Unicode escape"),
            SyntaxError::TokenInvalidPrimitiveType => f.write_str("not a primitive type name"),
            SyntaxError::TokenVerbatimMultilineNotEnoughBackticks => f.write_str("multiline string literal must be delimited by at least three backticks"),
            SyntaxError::TokenVerbatimTooManyBackticks => f.write_str("too many backticks in string literal closing delimiter"),
            SyntaxError::TokenVerbatimEOF => f.write_str("unexpected end of source in string literal"),
            SyntaxError::ExpectedValue => f.write_str("expected value"),
            SyntaxError::UnexpectedEOF => f.write_str("unexpected end of source"),
            SyntaxError::TagUnmatchedOpen => f.write_str("unmatched opening tag"),
            SyntaxError::TagUnmatchedClose => f.write_str("unmatched closing tag"),
            SyntaxError::TagDuplicateAttr(name) => write!(f, "duplicate attribute name '{name}'"),
            SyntaxError::SpreadNotAllowed => f.write_str("spread not allowed here"),
            SyntaxError::SpreadPositionalNotAllowed => f.write_str("positional spread not allowed here"),
            SyntaxError::SpreadNamedNotAllowed => f.write_str("named spread not allowed here"),
            SyntaxError::ParamDuplicateName(name) => write!(f, "duplicate parameter name '{name}'"),
            SyntaxError::ParamPositionalAfterNamed => f.write_str("positional parameter cannot occur after named parameter"),
            SyntaxError::ParamRequiredAfterOptional => f.write_str("required parameter cannot occur after optional parameter"),
            SyntaxError::ParamDefaultImplicit => f.write_str("implicit parameter cannot have default value"),
            SyntaxError::ParamPositionalImplicit => f.write_str("positional parameter cannot be implicit"),
            SyntaxError::ParamSpreadDefault => f.write_str("spread parameter cannot have default value"),
            SyntaxError::ParamSpreadImplicit => f.write_str("spread parameter cannot be implicit"),
            SyntaxError::ParamMultipleSpread => f.write_str("cannot have multiple spread parameters"),
            SyntaxError::ParamAfterSpread => f.write_str("parameter cannot occur after spread"),
            SyntaxError::ParamPositionalSpreadNoUnderscore => f.write_str("positional spread parameter must begin with underscore"),
            SyntaxError::ParamNamedSpreadUnderscore => f.write_str("named spread parameter must not begin with underscore"),
            SyntaxError::ParamContentSpread => f.write_str("content parameter cannot be spread"),
            SyntaxError::ParamContentDefault => f.write_str("content parameter cannot have default value"),
            SyntaxError::ArgDuplicateName(name) => write!(f, "duplicate named argument '{name}'"),
            SyntaxError::ArgNamedNotAllowed => f.write_str("named argument not allowed here"),
            SyntaxError::ArgPositionalAfterNamed => f.write_str("positional argument cannot occur after named argument"),
            SyntaxError::ArgSpreadNamed => f.write_str("named argument cannot be spread"),
            SyntaxError::ArgNamedUnderscore => f.write_str("named argument cannot begin with underscore"),
            SyntaxError::LetInMissingArgs => f.write_str("missing variable declarations for let expression"),
            SyntaxError::LetInPositionalArg => f.write_str("positional argument not allowed in let expression"),
            SyntaxError::LetInLiteral => f.write_str("let expression with literal has no effect; did you mean '...'?"),
            SyntaxError::PatternBareName => f.write_str("bare name not allowed in match pattern; use $ for variable name or backticks for string literal"),
            SyntaxError::PatternMultipleSpreads => f.write_str("match pattern cannot have multiple spreads"),
            SyntaxError::PatternNamedUnderscore => f.write_str("named pattern cannot begin with underscore"),
            SyntaxError::PatternNamedAfterSpread => f.write_str("named pattern cannot occur after spread"),
            SyntaxError::PatternDuplicateName(name) => write!(f, "duplicate named pattern '{name}'"),
            SyntaxError::PatternIncorrectCloseTag => f.write_str("incorrect closing tag in match pattern; use </> for unnamed tag"),
            SyntaxError::PatternCannotMatchHTML => f.write_str("this pattern cannot match HTML content"),
            SyntaxError::PatternAttrAccess => f.write_str("variable pattern must be a simple name, not attribute access"),
        }
    }
}
