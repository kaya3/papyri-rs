//! This module contains type declarations for AST nodes.

use std::rc::Rc;

use crate::utils::{NameID, NameIDSet, NameIDMap};
use crate::utils::sourcefile::SourceRange;
use super::types::Type;

#[derive(Debug)]
/// An AST node for a single HTML tag attribute.
pub struct TagAttribute {
    /// The source span of this AST node.
    pub(crate) range: SourceRange,
    
    /// The ID of the interned name of this attribute.
    pub(crate) name_id: NameID,
    
    /// If `true`, the attribute is ignored if its value is `none`; e.g.
    /// `attr?=$value`.
    pub(crate) question_mark: bool,
    
    /// The attribute's value; it is `None` for a Boolean attribute, as in
    /// `<input disabled>`.
    pub(crate) value: Option<Expr>,
}

#[derive(Debug)]
/// An attribute of an `ast::Tag` node.
pub enum TagAttrOrSpread {
    /// A literal attribute with a name, e.g. `name="value"`.
    Attr(TagAttribute),
    
    /// A spread of named attributes, e.g. `**$attrs`.
    Spread(Expr),
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
    pub(crate) range: SourceRange,
    
    /// The tag name.
    pub(crate) name: TagName,
    
    /// The tag's attributes.
    pub(crate) attrs: Box<[TagAttrOrSpread]>,
    
    /// The tag's contents.
    pub(crate) children: Box<[AST]>,
}

#[derive(Debug)]
/// A parameter declaration for an AST function signature node.
pub struct Param {
    /// The source span for this parameter declaration.
    pub(crate) range: SourceRange,
    
    /// The ID of the interned parameter name.
    pub(crate) name_id: NameID,
    
    /// If `true`, the parameter is optional with a default `none`.
    pub(crate) question_mark: bool,
    
    /// If `true`, the parameter is implicit, meaning that its default value
    /// will be taken from a variable of the same name at the call-site.
    pub(crate) is_implicit: bool,
    
    /// The type annotation for this parameter, or `Type::Any` if it does not
    /// have one.
    pub(crate) type_annotation: Type,
    
    /// The default value for this parameter, if it has one.
    pub(crate) default_value: Option<Box<Expr>>,
}

#[derive(Debug)]
/// A function signature for an AST function definition.
pub struct Signature {
    /// The source span of this function signature.
    pub(crate) range: SourceRange,
    
    /// The parameters in this function signature. All positional parameters
    /// occur before all named parameters.
    pub(crate) params: Box<[Param]>,
    
    pub(crate) positional_count: u16,
    pub(crate) positional_spread: bool,
    pub(crate) named_count: u16,
    pub(crate) named_spread: bool,
    
    /// If true, the final parameter is the contents parameter. Otherwise, the
    /// contents parameter is anonymous and of type `none`.
    pub(crate) has_content: bool,
}

#[derive(Debug)]
/// An AST node for a function definition.
pub struct FuncDef {
    /// The source span of this function definition.
    pub(crate) range: SourceRange,
    
    /// The ID of the interned function name; it may be anonymous.
    pub(crate) name_id: NameID,
    
    /// The signature of this function definition.
    pub(crate) signature: Signature,
    
    /// The function body.
    pub(crate) body: Rc<Expr>,
}

#[derive(Debug)]
/// A "let in" AST node.
pub struct LetIn {
    /// The source span of this "let in" expression.
    pub(crate) range: SourceRange,
    
    /// If true, this expression is `@implicit`, otherwise it is `@let`.
    pub(crate) is_implicit: bool,
    
    /// The variables declared in this "let in" expression.
    pub(crate) vars: Box<[(NameID, Expr)]>,
    
    /// The expression body.
    pub(crate) child: Expr,
}

#[derive(Debug)]
/// An `@export` declaration.
pub enum Export {
    /// An `@export(name1=..., name2=...).` declaration.
    Names(SourceRange, Box<[(NameID, Expr)]>),
    
    /// An `@export @let ...` declaration.
    LetIn(SourceRange, LetIn),
    
    /// An `@export @fn ...` declaration.
    FuncDef(SourceRange, FuncDef),
}

impl Export {
    pub(super) fn range(&self) -> SourceRange {
        match self {
            Export::Names(range, ..) |
            Export::LetIn(range, ..) |
            Export::FuncDef(range, ..) => *range,
        }
    }
}

#[derive(Debug)]
/// An argument in an AST function call.
pub struct Arg {
    /// The source span of this argument.
    pub(crate) range: SourceRange,
    
    /// The ID of the argument name, if it is a named argument; it is anonymous
    /// if this argument is positional or a spread.
    pub(crate) name_id: NameID,
    
    /// Indicates whether this argument is a simple argument, a positional
    /// spread argument, or a named spread argument.
    pub(super) spread_kind: SpreadKind,
    
    /// The argument's value.
    pub(crate) value: Expr,
}

impl Arg {
    /// Indicates whether this argument is positional, including a positional
    /// spread argument.
    pub(crate) fn is_positional(&self) -> bool {
        self.name_id.is_anonymous() && self.spread_kind != SpreadKind::Named
    }
    
    /// Indicates whether this argument is a positional spread or named spread
    /// argument.
    pub(crate) fn is_spread(&self) -> bool {
        self.spread_kind != SpreadKind::NoSpread
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[allow(missing_docs)]
/// Indicates whether an argument or parameter is spread, and if so whether it
/// is a positional spread `*v` or named spread `**kw`.
pub(super) enum SpreadKind {NoSpread, Positional, Named}

#[derive(Debug)]
/// An AST node for a function call.
pub struct FuncCall {
    /// The source span of this AST node.
    pub(crate) range: SourceRange,
    
    /// The function name which this node represents a call to.
    pub(crate) func: Name,
    
    /// The arguments of this function call.
    pub(crate) args: Box<[Arg]>,
    
    /// The "contents" argument of this function call.
    pub(crate) content: Expr,
}

#[derive(Debug)]
/// A part of an AST string template.
pub enum TemplatePart {
    /// A literal text part, represented by a source range.
    Literal(SourceRange),
    
    /// A literal character, either from an escape sequence or an HTML entity.
    LiteralChar(char),
    
    /// A name expression.
    Name(Name),
    
    /// Any sequence of consecutive whitespace characters.
    Whitespace,
}

impl TemplatePart {
    pub(super) fn is_literal(&self) -> bool {
        matches!(self, TemplatePart::Literal(..) | TemplatePart::LiteralChar(..))
    }
}

#[derive(Debug)]
/// An AST node for a `@match` expression.
pub struct Match {
    /// The source span of this `@match` expression.
    pub(crate) range: SourceRange,
    
    /// The value to be matched against.
    pub(crate) value: Expr,
    
    /// The branches of this `@match` expression; each has a pattern and a
    /// handler to be evaluated if the pattern is matched.
    pub(crate) branches: Box<[(MatchPattern, Expr)]>,
}

#[derive(Debug)]
/// An AST node for a pattern in a `@match` expression.
pub enum MatchPattern {
    /// A pattern which matches any value, unconditionally.
    Ignore(SourceRange),
    
    /// A pattern which only matches a unit value.
    LiteralNone(SourceRange),
    
    /// A pattern which matches a literal str, whose value is interned.
    LiteralName(SourceRange, NameID),
    
    /// A pattern which matches unconditionally, and binds it to a variable.
    VarName(SimpleName),
    
    /// A pattern which matches if the value is equal to the value of an
    /// expression.
    EqualsValue(Expr),
    
    /// An `&` pattern, which matches if both child patterns match. This is
    /// also used for patterns with type annotations.
    And(Box<(MatchPattern, MatchPattern)>),
    
    /// An `|` pattern, which matches if either of the child patterns match.
    /// Any names bound only by one branch will be bound to a unit value when
    /// the other branch is matched.
    Or(Box<(MatchPattern, MatchPattern)>, Box<[NameID]>),
    
    /// A pattern which matches a string according to a regular expression,
    /// binding its capturing groups to variables. Any capturing groups which
    /// do not match will be bound to a unit value.
    Regex(SourceRange, Box<RegexMatchPattern>),
    
    /// A pattern which matches if the value has the correct type.
    Typed(SourceRange, Type),
    
    /// A pattern which binds the value's type (as a string) to a variable.
    TypeOf(SimpleName),
    
    /// A pattern which matches a list, if the list's length equals the number
    /// of child patterns and each list element matches the corresponding child
    /// pattern.
    ExactList(SourceRange, Box<[MatchPattern]>),
    
    /// A pattern which matches a list, with a positional spread pattern which
    /// matches any excess elements. The third component of this variant is the
    /// index of the spread pattern in the boxed slice of child patterns.
    SpreadList(SourceRange, Box<[MatchPattern]>, u32),
    
    /// A pattern which matches a dictionary, with an optional named spread
    /// pattern which matches any excess elements.
    Dict(SourceRange, Box<DictMatchPattern>),
    
    /// A pattern which matches an HTML tag.
    Tag(SourceRange, Box<TagMatchPattern>),
    
    /// A pattern which matches an exact sequence of HTML content.
    ExactHTMLSeq(SourceRange, Box<[MatchPattern]>),
    
    /// A pattern which matches a sequence of HTML content, with a positional
    /// spread pattern which matches any excess content.
    SpreadHTMLSeq(SourceRange, Box<[MatchPattern]>, u32),
}

#[derive(Debug)]
/// An AST node for a regular expression pattern.
pub struct RegexMatchPattern {
    /// The regular expression which this pattern matches.
    pub(crate) regex: regex::Regex,
    
    /// The names bound by this regular expression pattern.
    pub(crate) names: Box<[SimpleName]>,
}

#[derive(Debug)]
/// An AST node for a dictionary pattern.
pub struct DictMatchPattern {
    /// A collection of (name, pattern) pairs for matching individual tag
    /// attributes.
    pub(crate) attrs: NameIDMap<MatchPattern>,
    
    /// An optional pattern for matching the remaining tag attributes, as a
    /// dictionary.
    pub(crate) spread: Option<MatchPattern>,
}

#[derive(Debug)]
/// An AST node for an HTML tag pattern.
pub struct TagMatchPattern {
    /// A pattern for matching an HTML tag's name.
    pub(crate) name: MatchPattern,
    
    /// A pattern for matching an HTML tag's attributes, as a dictionary.
    pub(crate) attrs: MatchPattern,
    
    /// A pattern for matching an HTML tag's contents, as an HTML sequence.
    pub(crate) content: MatchPattern,
}

impl MatchPattern {
    /// Returns the source span corresponding to this match pattern.
    pub(crate) fn range(&self) -> SourceRange {
        match self {
            MatchPattern::Ignore(range) |
            MatchPattern::ExactList(range, ..) |
            MatchPattern::SpreadList(range, ..) |
            MatchPattern::Dict(range, ..) |
            MatchPattern::Tag(range, ..) |
            MatchPattern::ExactHTMLSeq(range, ..) |
            MatchPattern::SpreadHTMLSeq(range, ..) |
            MatchPattern::Regex(range, ..) |
            MatchPattern::LiteralNone(range) |
            MatchPattern::LiteralName(range, ..) |
            MatchPattern::Typed(range, ..) |
            MatchPattern::TypeOf(SimpleName {range, ..}) |
            MatchPattern::VarName(SimpleName {range, ..}) => *range,
            MatchPattern::EqualsValue(node) => node.range(),
            MatchPattern::And(pair, ..) |
            MatchPattern::Or(pair, ..) => pair.0.range().to_end(pair.1.range().end),
        }
    }
    
    pub(super) fn get_name_ids(&self, out: &mut NameIDSet) {
        match self {
            MatchPattern::VarName(name) |
            MatchPattern::TypeOf(name) => {
                out.insert(name.name_id);
            },
            MatchPattern::And(pair) => {
                pair.0.get_name_ids(out);
                pair.1.get_name_ids(out);
            },
            MatchPattern::Or(.., name_ids) => {
                out.extend(name_ids.iter());
            },
            MatchPattern::Regex(_, r) => {
                out.extend(r.names.iter().map(|name| name.name_id));
            },
            MatchPattern::Tag(_, tag_pattern) => {
                tag_pattern.name.get_name_ids(out);
                tag_pattern.attrs.get_name_ids(out);
                tag_pattern.content.get_name_ids(out);
            },
            MatchPattern::ExactList(_, children) |
            MatchPattern::SpreadList(_, children, _) |
            MatchPattern::ExactHTMLSeq(_, children) |
            MatchPattern::SpreadHTMLSeq(_, children, _) => {
                for child in children.iter() {
                    child.get_name_ids(out);
                }
            },
            MatchPattern::Dict(_, dict_pattern) => {
                for (_, child) in dict_pattern.attrs.iter() {
                    child.get_name_ids(out);
                }
                if let Some(spread) = &dict_pattern.spread {
                    spread.get_name_ids(out);
                }
            },
            _ => {},
        }
    }
    
    /// Indicates whether this pattern is allowed in a position where only HTML
    /// content is expected.
    pub(super) fn can_match_html(&self) -> bool {
        match self {
            MatchPattern::And(pair) |
            MatchPattern::Or(pair, _) => pair.0.can_match_html() && pair.1.can_match_html(),
            
            MatchPattern::EqualsValue(Expr::Bool(..) | Expr::Int(..) | Expr::List(..)) |
            MatchPattern::ExactList(..) |
            MatchPattern::SpreadList(..) |
            MatchPattern::Dict(..) => false,
            
            _ => true,
        }
    }
}

#[derive(Debug)]
#[allow(missing_docs)]
/// A name expression, which is either a simple name or an attribute access.
pub enum Name {
    Simple(SimpleName),
    Attr(Box<AttrName>),
    Index(Box<IndexName>),
}

impl Name {
    /// Returns the source span of this name expression.
    pub(crate) fn range(&self) -> SourceRange {
        match self {
            Name::Simple(name) => name.range,
            Name::Attr(attr) => attr.range,
            Name::Index(index) => index.range,
        }
    }
    
    /// Indicates whether any part of this name expression is coalescing, i.e.
    /// whether a missing attribute may resolve to the unit value rather than
    /// raising an error.
    pub(crate) fn is_coalescing(&self) -> bool {
        let mut name = self;
        loop {
            match name {
                Name::Simple(_) => return false,
                Name::Attr(attr) => if attr.is_coalescing { return true; } else { name = &attr.subject; },
                Name::Index(index) => if index.is_coalescing { return true; } else { name = &index.subject; },
            }
        }
    }
}

#[derive(Debug)]
/// An occurrence of a simple variable name in a Papyri source file.
pub struct SimpleName {
    /// The ID of the interned variable name.
    pub(crate) name_id: NameID,
    
    /// The source span at which this variable name occurs.
    pub(crate) range: SourceRange,
}

#[derive(Debug)]
/// An attribute access expression, e.g. `$foo::bar`.
pub struct AttrName {
    /// The left-hand-side of this attribute access expression.
    pub(crate) subject: Name,
    
    /// If `true`, this expression coalesces a unit value on the left-hand-side.
    pub(crate) is_coalescing: bool,
    
    /// The attribute name.
    pub(crate) attr_name_id: NameID,
    
    /// The source span of this attribute access expression.
    pub(crate) range: SourceRange,
}

#[derive(Debug)]
/// An indexed access expression, e.g. `$foo::5`.
pub struct IndexName {
    /// The left-hand-side of this indexed access expression.
    pub(crate) subject: Name,
    
    /// If `true`, this expression coalesces a unit value on the left-hand-side.
    pub(crate) is_coalescing: bool,
    
    /// The index.
    pub(crate) index: i64,
    
    /// The source span of this indexed access expression.
    pub(crate) range: SourceRange,
}

#[derive(Debug)]
/// An AST node for an expression.
pub enum Expr {
    /// A unit literal (either `.` or `{}`).
    Unit(SourceRange),
    
    /// A bool literal.
    Bool(bool, SourceRange),
    
    /// A number literal.
    Int(i64, SourceRange),
    
    /// A bare literal string.
    BareString(SourceRange),
    
    /// A string literal.
    Verbatim(SourceRange),
    
    /// A function call.
    FuncCall(Box<FuncCall>),
    
    /// A function definition, using the `@fn` keyword.
    FuncDef(Box<FuncDef>),
    
    /// A `@let` or `@implicit` expression.
    LetIn(Box<LetIn>),
    
    /// A `@match` expression.
    Match(Box<Match>),
    
    /// A group of flow content, delimited by braces.
    Group(Box<[AST]>, SourceRange),
    
    /// A list, delimited by square brackets.
    List(Box<[(Expr, bool)]>, SourceRange),
    
    /// A string template, delimited by quotes.
    Template(Box<[TemplatePart]>, SourceRange),
    
    /// An HTML tag.
    Tag(Box<Tag>),
    
    /// A name expression.
    Name(Name),
}

impl Expr {
    /// Returns the source span corresponding to this AST node.
    pub(crate) fn range(&self) -> SourceRange {
        match self {
            Expr::FuncCall(call) => call.range,
            Expr::FuncDef(def) => def.range,
            Expr::LetIn(l) => l.range,
            Expr::Match(m) => m.range,
            Expr::Tag(tag) => tag.range,
            Expr::Name(name) => name.range(),
            
            Expr::Unit(range) |
            Expr::Bool(.., range) |
            Expr::Int(.., range) |
            Expr::BareString(range) |
            Expr::Verbatim(range, ..) |
            Expr::Group(.., range) |
            Expr::List(.., range) |
            Expr::Template(.., range) => *range,
        }
    }
    
    pub(crate) fn is_literal(&self) -> bool {
        matches!(self, Expr::Unit(..) | Expr::Bool(..) | Expr::Int(..) | Expr::BareString(..) | Expr::Verbatim(..))
    }
}

#[derive(Debug)]
/// An AST node representing flow content.
pub enum AST {
    /// An `@export` declaration.
    Export(Box<Export>),
    
    /// An expression.
    Expr(Expr),
    
    /// A function definition, using the `@fn` keyword.
    FuncDef(Box<FuncDef>),
    
    /// A code fence. The boolean value indicates whether it is multiline.
    CodeFence(SourceRange, bool),
    
    /// Literal text. Text substitutions have already been applied.
    Text(Rc<str>, SourceRange),
    
    /// A literal character, either from an escape sequence or an HTML entity.
    Char(char, SourceRange),
    
    /// Literal whitespace.
    Whitespace(SourceRange),
    
    /// A literal paragraph break, represented in the source as a blank line.
    ParagraphBreak(SourceRange),
}

impl AST {
    /// Returns the source span corresponding to this AST node.
    pub(crate) fn range(&self) -> SourceRange {
        match self {
            AST::Expr(expr) => expr.range(),
            AST::FuncDef(def) => def.range,
            AST::Export(export) => export.range(),
            
            AST::CodeFence(range, ..) |
            AST::Text(.., range) |
            AST::Char(.., range) |
            AST::Whitespace(range) |
            AST::ParagraphBreak(range) => *range,
        }
    }
    
    /// Indicates whether this AST node is whitespace or a paragraph break.
    pub(crate) fn is_whitespace(&self) -> bool {
        match self {
            AST::Whitespace(..) |
            AST::ParagraphBreak(..) => true,
            AST::Char(c, ..) => c.is_ascii_whitespace(),
            _ => false,
        }
    }
}
