use std::rc::Rc;
use indexmap::IndexMap;

use crate::utils::{SourceRange, NameID};
use super::token::Token;

#[derive(Debug)]
pub struct TagAttribute {
    pub range: SourceRange,
    pub name_id: NameID,
    pub question_mark: bool,
    pub value: Option<AST>,
}

#[derive(Debug)]
pub enum TagAttrOrSpread {
    Attr(TagAttribute),
    Spread(AST),
}

#[derive(Debug)]
pub enum TagName {
    Fixed(NameID),
    Variable(VarName),
}

#[derive(Debug)]
pub struct Tag {
    pub range: SourceRange,
    pub name: TagName,
    pub attrs: Box<[TagAttrOrSpread]>,
    pub children: Box<[AST]>,
}

#[derive(Debug)]
pub enum TypeAnnotation {
    Primitive(SourceRange),
    Group(Box<TypeAnnotation>, SourceRange),
}

impl TypeAnnotation {
    pub fn range(&self) -> SourceRange {
        match self {
            TypeAnnotation::Primitive(range) => range.clone(),
            TypeAnnotation::Group(child, kind) => child.range().to_end(kind.end),
        }
    }
}

#[derive(Debug)]
pub struct Param {
    pub range: SourceRange,
    pub name_id: NameID,
    pub question_mark: bool,
    pub is_implicit: bool,
    pub type_annotation: Option<TypeAnnotation>,
    pub default_value: Option<Box<AST>>,
}

#[derive(Debug)]
pub struct Signature {
    pub range: SourceRange,
    pub positional_params: Box<[Param]>,
    pub spread_param: Option<Box<Param>>,
    pub named_params: Box<[Param]>,
    pub spread_named_param: Option<Box<Param>>,
    pub content_param: Param,
}

#[derive(Debug)]
pub struct FuncDef {
    pub range: SourceRange,
    pub name_id: NameID,
    pub signature: Signature,
    pub body: Rc<AST>,
}

#[derive(Debug)]
pub struct Arg {
    pub range: SourceRange,
    pub name_id: NameID,
    pub spread_kind: SpreadKind,
    pub value: AST,
}

impl Arg {
    pub fn is_positional(&self) -> bool {
        self.name_id.is_anonymous() && self.spread_kind != SpreadKind::Named
    }
    
    pub fn is_spread(&self) -> bool {
        self.spread_kind != SpreadKind::NoSpread
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SpreadKind {
    NoSpread,
    Positional,
    Named,
}

#[derive(Debug)]
pub struct FuncCall {
    pub range: SourceRange,
    pub name_id: NameID,
    pub args: Box<[Arg]>,
    pub content: AST
}

#[derive(Debug)]
pub enum TemplatePart {
    Literal(SourceRange),
    VarName(VarName),
    Escape(SourceRange),
    Entity(SourceRange),
    Whitespace
}

#[derive(Debug)]
pub struct Match {
    pub range: SourceRange,
    pub value: AST,
    pub branches: Box<[MatchBranch]>,
}

#[derive(Debug)]
pub struct MatchBranch {
    pub pattern: MatchPattern,
    pub then: AST,
}

#[derive(Debug)]
pub enum MatchPattern {
    Ignore(SourceRange),
    SpreadIgnore(SourceRange),
    EmptyHTML(SourceRange),
    Literal(Token),
    LiteralName(SourceRange, NameID),
    VarName(VarName),
    SpreadVarName(VarName),
    Regex(SourceRange, regex::Regex, Box<[VarName]>),
    Typed(SourceRange, Box<MatchPattern>, TypeAnnotation),
    TypeOf(SourceRange, Box<MatchPattern>, VarName),
    ExactList(SourceRange, Box<[MatchPattern]>),
    SpreadList(SourceRange, Box<[MatchPattern]>, usize),
    Tag(SourceRange, Box<TagMatchPattern>),
}

#[derive(Debug)]
pub struct TagMatchPattern {
    pub name: MatchPattern,
    pub attrs: IndexMap<NameID, MatchPattern>,
    pub spread: Option<MatchPattern>,
    pub content: MatchPattern,
}

impl MatchPattern {
    pub fn range(&self) -> &SourceRange {
        match self {
            MatchPattern::Ignore(range) |
            MatchPattern::SpreadIgnore(range) |
            MatchPattern::Typed(range, ..) |
            MatchPattern::TypeOf(range, ..) |
            MatchPattern::ExactList(range, ..) |
            MatchPattern::SpreadList(range, ..) |
            MatchPattern::Tag(range, ..) |
            MatchPattern::Regex(range, ..) |
            MatchPattern::EmptyHTML(range) |
            MatchPattern::LiteralName(range, ..) |
            MatchPattern::Literal(Token {range, ..}) |
            MatchPattern::VarName(VarName {range, ..}) |
            MatchPattern::SpreadVarName(VarName {range, ..}) => range,
        }
    }
    
    pub fn is_spread(&self) -> bool {
        match self {
            MatchPattern::SpreadIgnore(..) | MatchPattern::SpreadVarName(..) => true,
            MatchPattern::Typed(_, child, _) => child.is_spread(),
            _ => false,
        }
    }
}

#[derive(Debug)]
pub struct VarName {
    pub name_id: NameID,
    pub range: SourceRange,
}

#[derive(Debug)]
pub enum AST {
    LiteralValue(Token),
    Verbatim(Token),
    
    FuncCall(Box<FuncCall>),
    FuncDef(Box<FuncDef>),
    Match(Box<Match>),
    
    Group(Box<[AST]>, SourceRange),
    List(Box<[(AST, bool)]>, SourceRange),
    Template(Box<[TemplatePart]>, SourceRange),
    Tag(Box<Tag>),
    VarName(VarName),
    
    Text(Rc<str>, SourceRange),
    Entity(SourceRange),
    Escape(SourceRange),
    Whitespace(SourceRange),
    ParagraphBreak(SourceRange),
}

impl AST {
    pub fn seq_range(open: Token, children: &[AST], close: Option<Token>) -> SourceRange {
        let end = match (children.last(), close) {
            (_, Some(close)) => close.range.end,
            (Some(child), _) => child.range().end,
            _ => open.range.end,
        };
        open.range.to_end(end)
    }
    
    pub fn range(&self) -> &SourceRange {
        match self {
            AST::FuncCall(call) => &call.range,
            AST::FuncDef(def) => &def.range,
            AST::Match(m) => &m.range,
            AST::Tag(tag) => &tag.range,
            
            AST::LiteralValue(Token {range, ..}) |
            AST::Verbatim(Token {range, ..}) |
            AST::VarName(VarName {range, ..}) |
            AST::Group(.., range) |
            AST::List(.., range) |
            AST::Template(.., range) |
            AST::Text(.., range) |
            AST::Entity(range) |
            AST::Escape(range) |
            AST::Whitespace(range) |
            AST::ParagraphBreak(range) => range,
        }
    }
    
    pub fn is_whitespace(&self) -> bool {
        matches!(self, AST::Whitespace(..) | AST::ParagraphBreak(..))
    }
}
