//! This module contains type declarations for AST nodes.

use std::rc::Rc;
use indexmap::IndexMap;

use crate::utils::{SourceRange, NameID};
use super::token::Token;

#[derive(Debug)]
/// An AST node for a single HTML tag attribute.
pub struct TagAttribute {
    /// The source span of this AST node.
    pub range: SourceRange,
    
    /// The ID of the interned name of this attribute.
    pub name_id: NameID,
    
    /// If `true`, the attribute is ignored if its value is `none`; e.g.
    /// `attr?=$value`.
    pub question_mark: bool,
    
    /// The attribute's value; it is `None` for a Boolean attribute, as in
    /// `<input disabled>`.
    pub value: Option<AST>,
}

#[derive(Debug)]
/// An attribute of an `ast::Tag` node.
pub enum TagAttrOrSpread {
    /// A literal attribute with a name, e.g. `name="value"`.
    Attr(TagAttribute),
    
    /// A spread of named attributes, e.g. `**$attrs`.
    Spread(AST),
}

#[derive(Debug)]
/// The name of an `ast::Tag` node.
pub enum TagName {
    /// A literal tag name, e.g. `<span>`.
    Literal(NameID),
    
    /// A variable tag name, e.g. `<$t>`.
    Name(Name),
}

#[derive(Debug)]
/// An AST node representing an HTML tag.
pub struct Tag {
    /// The source span of this AST node.
    pub range: SourceRange,
    
    /// The tag name.
    pub name: TagName,
    
    /// The tag's attributes.
    pub attrs: Box<[TagAttrOrSpread]>,
    
    /// The tag's contents.
    pub children: Box<[AST]>,
}

#[derive(Debug)]
/// An AST node representing a type annotation.
pub enum TypeAnnotation {
    /// A primitive type name.
    Primitive(SourceRange),
    
    /// A group type; either `T list`, `T dict`, or `T?`.
    Group(Box<TypeAnnotation>, SourceRange),
}

impl TypeAnnotation {
    /// Returns the source span of this type annotation.
    pub fn range(&self) -> SourceRange {
        match self {
            TypeAnnotation::Primitive(range) => range.clone(),
            TypeAnnotation::Group(child, kind) => child.range().to_end(kind.end),
        }
    }
    
    /// Returns the end position of the source span of this type annotation.
    pub fn range_end(&self) -> u32 {
        match self {
            TypeAnnotation::Primitive(range) |
            TypeAnnotation::Group(_, range) => range.end,
        }
    }
}

#[derive(Debug)]
/// A parameter declaration for an AST function signature node.
pub struct Param {
    /// The source span for this parameter declaration.
    pub range: SourceRange,
    
    /// The ID of the interned parameter name.
    pub name_id: NameID,
    
    /// If `true`, the parameter is optional with a default `none`.
    pub question_mark: bool,
    
    /// If `true`, the parameter is implicit, meaning that its default value
    /// will be taken from a variable of the same name at the call-site.
    pub is_implicit: bool,
    
    /// The type annotation for this parameter, if it has one.
    pub type_annotation: Option<TypeAnnotation>,
    
    /// The default value for this parameter, if it has one.
    pub default_value: Option<Box<AST>>,
}

#[derive(Debug)]
/// A function signature for an AST function definition.
pub struct Signature {
    /// The source span of this function signature.
    pub range: SourceRange,
    
    /// The positional parameters in this function signature. Parameters are
    /// positional if their names begin with underscores.
    pub positional_params: Box<[Param]>,
    
    /// The positional spread parameter in this function signature, if it has
    /// one.
    pub spread_param: Option<Box<Param>>,
    
    /// The named parameters in this function signature.
    pub named_params: Box<[Param]>,
    
    /// The named spread parameter in this function signature, if it has one.
    pub spread_named_param: Option<Box<Param>>,
    
    /// The content parameter in this function signature.
    pub content_param: Param,
}

#[derive(Debug)]
/// An AST node for a function definition.
pub struct FuncDef {
    /// The source span of this function definition.
    pub range: SourceRange,
    
    /// The ID of the interned function name; it may be anonymous.
    pub name_id: NameID,
    
    /// The signature of this function definition.
    pub signature: Signature,
    
    /// The function body.
    pub body: Rc<AST>,
}

#[derive(Debug)]
/// A "let in" AST node.
pub struct LetIn {
    /// The source span of this "let in" expression.
    pub range: SourceRange,
    
    /// If true, this expression is `@implicit`, otherwise it is `@let`.
    pub is_implicit: bool,
    
    /// The variables declared in this "let in" expression.
    pub vars: Box<[(NameID, AST)]>,
    
    /// The expression body.
    pub child: AST,
}

#[derive(Debug)]
/// An `@export` declaration.
pub enum Export {
    /// An `@export(name1=..., name2=...).` declaration.
    Names(SourceRange, Box<[(NameID, AST)]>),
    
    /// An `@export @let ...` declaration.
    LetIn(SourceRange, LetIn),
    
    /// An `@export @fn ...` declaration.
    FuncDef(SourceRange, FuncDef),
}

impl Export {
    fn range(&self) -> &SourceRange {
        match self {
            Export::Names(range, ..) |
            Export::LetIn(range, ..) |
            Export::FuncDef(range, ..) => range,
        }
    }
}

#[derive(Debug)]
/// An argument in an AST function call.
pub struct Arg {
    /// The source span of this argument.
    pub range: SourceRange,
    
    /// The ID of the argument name, if it is a named argument; it is anonymous
    /// if this argument is positional or a spread.
    pub name_id: NameID,
    
    /// Indicates whether this argument is a simple argument, a positional
    /// spread argument, or a named spread argument.
    pub spread_kind: SpreadKind,
    
    /// The argument's value.
    pub value: AST,
}

impl Arg {
    /// Indicates whether this argument is positional, including a positional
    /// spread argument.
    pub fn is_positional(&self) -> bool {
        self.name_id.is_anonymous() && self.spread_kind != SpreadKind::Named
    }
    
    /// Indicates whether this argument is a positional spread or named spread
    /// argument.
    pub fn is_spread(&self) -> bool {
        self.spread_kind != SpreadKind::NoSpread
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[allow(missing_docs)]
/// Indicates whether an argument or parameter is spread, and if so whether it
/// is a positional spread `*v` or named spread `**kw`.
pub enum SpreadKind {NoSpread, Positional, Named}

#[derive(Debug)]
/// An AST node for a function call.
pub struct FuncCall {
    /// The source span of this AST node.
    pub range: SourceRange,
    
    /// The function name which this node represents a call to.
    pub func: Name,
    
    /// The arguments of this function call.
    pub args: Box<[Arg]>,
    
    /// The "contents" argument of this function call.
    pub content: AST
}

#[derive(Debug)]
/// A part of an AST string template.
pub enum TemplatePart {
    /// A literal text part, represented by a source range.
    Literal(SourceRange),
    
    /// A literal text part, represented by a string.
    LiteralStr(Box<str>),
    
    /// A name expression.
    Name(Name),
    
    /// Any sequence of consecutive whitespace characters.
    Whitespace,
}

#[derive(Debug)]
/// An AST node for a `@match` expression.
pub struct Match {
    /// The source span of this `@match` expression.
    pub range: SourceRange,
    
    /// The value to be matched against.
    pub value: AST,
    
    /// The branches of this `@match` expression; each has a pattern and a
    /// handler to be evaluated if the pattern is matched.
    pub branches: Box<[(MatchPattern, AST)]>,
}

#[derive(Debug)]
/// An AST node for a pattern in a `@match` expression.
pub enum MatchPattern {
    /// A pattern which matches any value, unconditionally.
    Ignore(SourceRange),
    
    /// A pattern which only matches a unit value.
    LiteralNone(SourceRange),
    
    /// A pattern which matches a literal bool, int or str value.
    Literal(Token),
    
    /// A pattern which matches a literal str, whose value is interned.
    LiteralName(SourceRange, NameID),
    
    /// A pattern which matches unconditionally, and binds it to a variable.
    VarName(SimpleName),
    
    /// A pattern which matches if the value is equal to the value of an
    /// expression.
    EqualsValue(SourceRange, AST),
    
    /// A pattern which matches a string according to a regular expression,
    /// binding its capturing groups to variables. Any capturing groups which
    /// do not match will be bound to a unit value.
    Regex(SourceRange, regex::Regex, Box<[SimpleName]>),
    
    /// A pattern which matches if the child pattern matches and the value has
    /// the correct type. The value may be coerced in order to match.
    Typed(SourceRange, Box<MatchPattern>, TypeAnnotation),
    
    /// A pattern which matches if the child pattern matches, and also binds
    /// the value's type (as a string) to a variable.
    TypeOf(SourceRange, Box<MatchPattern>, SimpleName),
    
    /// A pattern which matches a list, if the list's length equals the number
    /// of child patterns and each list element matches the corresponding child
    /// pattern.
    ExactList(SourceRange, Box<[MatchPattern]>),
    
    /// A pattern which matches a list, with a positional spread pattern which
    /// matches any excess elements. The third component of this variant is the
    /// index of the spread pattern in the boxed slice of child patterns.
    SpreadList(SourceRange, Box<[MatchPattern]>, usize),
    
    /// A pattern which matches a dictionary, with an optional named spread
    /// pattern which matches any excess elements.
    Dict(SourceRange, Box<DictMatchPattern>),
    
    /// A pattern which matches an HTML tag.
    Tag(SourceRange, Box<TagMatchPattern>),
    
    /// A pattern which matches an exact sequence of HTML content.
    ExactHTMLSeq(SourceRange, Box<[MatchPattern]>),
    
    /// A pattern which matches a sequence of HTML content, with a positional
    /// spread pattern which matches any excess content.
    SpreadHTMLSeq(SourceRange, Box<[MatchPattern]>, usize),
}

#[derive(Debug)]
/// An AST node for a dictionary pattern.
pub struct DictMatchPattern {
    /// A collection of (name, pattern) pairs for matching individual tag
    /// attributes.
    pub attrs: IndexMap<NameID, MatchPattern>,
    
    /// An optional pattern for matching the remaining tag attributes, as a
    /// dictionary.
    pub spread: Option<MatchPattern>,
}

#[derive(Debug)]
/// An AST node for an HTML tag pattern.
pub struct TagMatchPattern {
    /// A pattern for matching an HTML tag's name.
    pub name: MatchPattern,
    
    /// A pattern for matching an HTML tag's attributes, as a dictionary.
    pub attrs: MatchPattern,
    
    /// A pattern for matching an HTML tag's contents, as an HTML sequence.
    pub content: MatchPattern,
}

impl MatchPattern {
    /// Returns the source span corresponding to this match pattern.
    pub fn range(&self) -> &SourceRange {
        match self {
            MatchPattern::Ignore(range) |
            MatchPattern::EqualsValue(range, ..) |
            MatchPattern::Typed(range, ..) |
            MatchPattern::TypeOf(range, ..) |
            MatchPattern::ExactList(range, ..) |
            MatchPattern::SpreadList(range, ..) |
            MatchPattern::Dict(range, ..) |
            MatchPattern::Tag(range, ..) |
            MatchPattern::ExactHTMLSeq(range, ..) |
            MatchPattern::SpreadHTMLSeq(range, ..) |
            MatchPattern::Regex(range, ..) |
            MatchPattern::LiteralNone(range) |
            MatchPattern::LiteralName(range, ..) |
            MatchPattern::Literal(Token {range, ..}) |
            MatchPattern::VarName(SimpleName {range, ..}) => range,
        }
    }
    
    /// Indicates whether this pattern can possibly match any HTML content.
    pub fn can_match_html(&self) -> bool {
        match self {
            MatchPattern::Ignore(..) |
            MatchPattern::LiteralNone(..) |
            MatchPattern::VarName(..) |
            MatchPattern::Tag(..) |
            MatchPattern::ExactHTMLSeq(..) |
            MatchPattern::SpreadHTMLSeq(..) => true,
            
            MatchPattern::Typed(_, child, _) |
            MatchPattern::TypeOf(_, child, _) => child.can_match_html(),
            
            _ => false,
        }
    }
}

#[derive(Debug)]
#[allow(missing_docs)]
/// A name expression, which is either a simple name or an attribute access.
pub enum Name {
    SimpleName(SimpleName),
    AttrName(Box<AttrName>),
}

impl Name {
    /// Returns the source span of this name expression.
    pub fn range(&self) -> &SourceRange {
        match self {
            Name::SimpleName(name) => &name.range,
            Name::AttrName(attr) => &attr.range,
        }
    }
    
    /// Indicates whether any attribute access in this name expression is
    /// coalescing, i.e. whether a missing attribute may resolve to the unit
    /// value rather than raising an error.
    pub fn is_coalescing(&self) -> bool {
        let mut name = self;
        while let Name::AttrName(attr) = name {
            if attr.is_coalescing { return true; }
            name = &attr.subject;
        }
        false
    }
}

impl AttrName {
    /// Returns the simple name at the root of this name expression.
    pub fn get_root(self) -> SimpleName {
        let mut attr = self;
        loop {
            match attr.subject {
                Name::SimpleName(name) => break name,
                Name::AttrName(a) => attr = *a,
            }
        }
    }
}

#[derive(Debug)]
/// An occurrence of a simple variable name in a Papyri source file.
pub struct SimpleName {
    /// The ID of the interned variable name.
    pub name_id: NameID,
    
    /// The source span at which this variable name occurs.
    pub range: SourceRange,
}

#[derive(Debug)]
/// An attribute access expression, e.g. `$foo::bar`.
pub struct AttrName {
    /// The left-hand-side of this attribute access expression.
    pub subject: Name,
    
    /// If `true`, this expression coalesces a unit value on the left-hand-side.
    pub is_coalescing: bool,
    
    /// The attribute name.
    pub attr_name_id: NameID,
    
    /// The source span of this attribute access expression.
    pub range: SourceRange,
}

#[derive(Debug)]
/// An AST node. This may represent either flow content or a value.
pub enum AST {
    /// A literal bool, int, str or unit, not including a string literal.
    LiteralValue(Token),
    
    /// A string literal.
    Verbatim(Token),
    
    /// An `@export` declaration.
    Export(Box<Export>),
    
    /// A function call, not including `@fn` or `@match` (which are keywords, not
    /// functions).
    FuncCall(Box<FuncCall>),
    
    /// A function definition, using the `@fn` keyword.
    FuncDef(Box<FuncDef>),
    
    /// A "let in" expression.
    LetIn(Box<LetIn>),
    
    /// A `@match` expression.
    Match(Box<Match>),
    
    /// A group of HTML content, delimited by braces.
    Group(Box<[AST]>, SourceRange),
    
    /// A list, delimited by square brackets.
    List(Box<[(AST, bool)]>, SourceRange),
    
    /// A string template, delimited by quotes.
    Template(Box<[TemplatePart]>, SourceRange),
    
    /// An HTML tag.
    Tag(Box<Tag>),
    
    /// A name expression.
    Name(Name),
    
    /// Literal text. Text substitutions have already been applied.
    Text(Rc<str>, SourceRange),
    
    /// Literal whitespace.
    Whitespace(SourceRange),
    
    /// A literal paragraph break, represented in the source as a blank line.
    ParagraphBreak(SourceRange),
}

impl AST {
    /// Returns the source span corresponding to this AST node.
    pub fn range(&self) -> &SourceRange {
        match self {
            AST::FuncCall(call) => &call.range,
            AST::FuncDef(def) => &def.range,
            AST::LetIn(l) => &l.range,
            AST::Match(m) => &m.range,
            AST::Tag(tag) => &tag.range,
            AST::Export(e) => e.range(),
            AST::Name(name) => name.range(),
            
            AST::LiteralValue(Token {range, ..}) |
            AST::Verbatim(Token {range, ..}) |
            AST::Group(.., range) |
            AST::List(.., range) |
            AST::Template(.., range) |
            AST::Text(.., range) |
            AST::Whitespace(range) |
            AST::ParagraphBreak(range) => range,
        }
    }
    
    /// Indicates whether this AST node is whitespace or a paragraph break.
    pub fn is_whitespace(&self) -> bool {
        matches!(self, AST::Whitespace(..) | AST::ParagraphBreak(..))
    }
}
