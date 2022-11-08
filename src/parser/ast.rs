use std::rc::Rc;

use crate::utils::{SourceRange, NameID};
use super::token::Token;

#[derive(Debug)]
pub struct TagAttribute {
    pub name_id: NameID,
    pub question_mark: bool,
    pub value: Option<AST>,
}

#[derive(Debug)]
pub struct Tag {
    pub range: SourceRange,
    pub name_id: NameID,
    pub attrs: Box<[TagAttribute]>,
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

#[derive(Debug, PartialEq, Eq)]
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
pub struct GroupOrList {
    pub range: SourceRange,
    pub children: Box<[AST]>,
}

#[derive(Debug)]
pub enum AST {
    LiteralValue(Token),
    Verbatim(Token),
    
    FuncCall(Box<FuncCall>),
    FuncDef(Box<FuncDef>),
    Group(Box<GroupOrList>),
    List(Box<GroupOrList>),
    Tag(Box<Tag>),
    VarName(NameID, SourceRange),
    
    Text(Rc<str>, SourceRange),
    Entity(SourceRange),
    Escape(SourceRange),
    Whitespace(SourceRange),
    ParagraphBreak(SourceRange),
}

impl AST {
    pub fn seq(open: Token, children: Vec<AST>, close: Option<Token>) -> Box<GroupOrList> {
        let end = close.map(|t| t.range.end)
            .or_else(|| children.last().map(|child| child.range().end))
            .unwrap_or(open.range.end);
        Box::new(GroupOrList {
            range: open.range.to_end(end),
            children: children.into_boxed_slice(),
        })
    }
    
    pub fn range(&self) -> &SourceRange {
        match self {
            AST::FuncCall(call) => &call.range,
            AST::FuncDef(def) => &def.range,
            AST::Group(group) => &group.range,
            AST::List(list) => &list.range,
            AST::Tag(tag) => &tag.range,
            
            AST::LiteralValue(Token {range, ..}) |
            AST::Verbatim(Token {range, ..}) |
            AST::VarName(_, range) |
            AST::Text(_, range) |
            AST::Entity(range) |
            AST::Escape(range) |
            AST::Whitespace(range) |
            AST::ParagraphBreak(range) => range,
        }
    }
    
    pub fn is_whitespace(&self) -> bool {
        matches!(self, AST::Whitespace(_) | AST::ParagraphBreak(_))
    }
}
