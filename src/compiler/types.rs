use crate::errors::{Diagnostics, ice_at, TypeError};
use crate::utils::{SourceRange, str_ids};
use crate::parser::{ast, Token};
use super::compiler::Compiler;
use super::html::HTML;
use super::value::{Value, ValueMap};

#[derive(Clone, PartialEq, Eq)]
/// Represents a type in the Papyri language.
pub enum Type {
    /// The unit type `none`, inhabited only by `Value::UNIT`.
    Unit,
    
    /// The top type `any`, which all values are assignable to.
    AnyValue,
    
    /// The type `html`, representing any HTML content; `block` and `inline`
    /// are subtypes.
    AnyHTML,
    
    /// The type `block`, representing block-level HTML content, or content
    /// which otherwise need not be wrapped in a block-level element.
    Block,
    
    /// The type `inline`, representing inline HTML content.
    Inline,
    
    /// The type `bool`, representing the Boolean values `True` and `False`.
    Bool,
    
    /// The type `int`, representing signed 64-bit integer values.
    Int,
    
    /// The type `str`, representing string values. HTML text content is not
    /// considered to be a string value.
    Str,
    
    /// The type `function`, representing all function with any signature.
    Function,
    
    /// A type of the form `T dict`, representing a dictionary whose elements
    /// are of type `T`.
    Dict(Box<Type>),
    
    /// A type of the form `T list`, representing a list whose elements are of
    /// type `T`.
    List(Box<Type>),
    
    /// A type of the form `T?`, representing either values of type `T` or the
    /// unit value. This is normalised so that the child type is not itself
    /// optional, and not `any` or `none`.
    Optional(Box<Type>),
}

impl Type {
    /// Creates a representation of a `dict` type.
    pub fn dict(t: Type) -> Type {
        Type::Dict(Box::new(t))
    }
    
    /// Creates a representation of a `list` type.
    pub fn list(t: Type) -> Type {
        Type::List(Box::new(t))
    }
    
    /// Creates a representation of an optional type. The representation is
    /// normalised by simplifying `T??` to `T?`, `any?` to `any`, and `none?`
    /// to `none`.
    pub fn optional(t: Type) -> Type {
        if matches!(t, Type::Optional(..) | Type::AnyValue | Type::Unit) {
            t
        } else {
            Type::Optional(Box::new(t))
        }
    }
    
    /// Indicates whether this type is either `html`, `block` or `inline`. The
    /// top type `any` is not considered to be an HTML type.
    pub fn is_html(&self) -> bool {
        matches!(self, Type::AnyHTML | Type::Block | Type::Inline)
    }
    
    /// Returns the type for list or dictionary components, if this type is
    /// used as a type hint for a list or dictionary value. The returned type
    /// is meaningless if a list or dictionary could never be coerced to this
    /// type.
    pub fn component_type(&self) -> &Type {
        match self {
            Type::Dict(t) | Type::List(t) => t,
            Type::Block | Type::AnyHTML => &Type::AnyHTML,
            _ => &Type::AnyValue,
        }
    }
    
    /// Returns the strongest type representing all values of both `self` and
    /// `other`.
    pub fn least_upper_bound(&self, other: &Type) -> Type {
        match (self, other) {
            (t1, t2) if t1 == t2 => t1.clone(),
            (Type::AnyValue, t) | (t, Type::AnyValue) => t.clone(),
            (t, Type::Unit) | (Type::Unit, t) => Type::optional(t.clone()),
            
            (t1, t2) if t1.is_html() && t2.is_html() => Type::AnyHTML,
            (Type::AnyHTML, _) | (_, Type::AnyHTML) => Type::AnyHTML,
            
            (Type::List(t1), Type::List(t2)) => Type::list(t1.least_upper_bound(t2)),
            (Type::Dict(t1), Type::Dict(t2)) => Type::dict(t1.least_upper_bound(t2)),
            (Type::Optional(t1), Type::Optional(t2)) => Type::optional(t1.least_upper_bound(t2)),
            (Type::Optional(t1), t2) | (t2, Type::Optional(t1)) => Type::optional(t1.least_upper_bound(t2)),
            
            (Type::Dict(_) | Type::List(_), Type::Block) | (Type::Block, Type::Dict(_) | Type::List(_)) => Type::Block,
            (Type::Dict(_) | Type::List(_) | Type::Block, _) | (_, Type::Dict(_) | Type::List(_) | Type::Block) => Type::AnyHTML,
            _ => Type::Inline,
        }
    }
    
    /// Compiles an AST type annotation into the Papyri type it represents.
    pub fn compile(type_annotation: &ast::TypeAnnotation) -> Type {
        match type_annotation {
            ast::TypeAnnotation::Primitive(range) => match range.as_str() {
                "any" => Type::AnyValue,
                "html" => Type::AnyHTML,
                "block" => Type::Block,
                "inline" => Type::Inline,
                "none" => Type::Unit,
                "bool" => Type::Bool,
                "int" => Type::Int,
                "str" => Type::Str,
                "function" => Type::Function,
                _ => ice_at("not a primitive type", range),
            },
            ast::TypeAnnotation::Group(child, range) => {
                let child = Type::compile(child);
                match range.as_str() {
                    "dict" => Type::dict(child),
                    "list" => Type::list(child),
                    "?" => Type::optional(child),
                    _ => ice_at("not a group type", range),
                }
            },
        }
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::AnyValue => f.write_str("any"),
            Type::AnyHTML => f.write_str("html"),
            Type::Block => f.write_str("block"),
            Type::Inline => f.write_str("inline"),
            Type::Unit => f.write_str("none"),
            Type::Bool => f.write_str("bool"),
            Type::Int => f.write_str("int"),
            Type::Str => f.write_str("str"),
            Type::Function => f.write_str("function"),
            Type::Dict(t) => write!(f, "{t} dict"),
            Type::List(t) => write!(f, "{t} list"),
            Type::Optional(t) => write!(f, "{t}?"),
        }
    }
}

impl std::fmt::Debug for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self, f)
    }
}

impl Diagnostics {
    pub fn err_expected_type(&mut self, expected: &Type, was: &Type, range: &SourceRange) {
        self.type_error(TypeError::ExpectedWas(expected.clone(), was.clone()), range);
    }
}

impl <'a> Compiler<'a> {
    pub fn coerce(&mut self, value: Value, expected: &Type, range: &SourceRange) -> Option<Value> {
        self._coerce(value, expected, Some(range))
    }
    
    pub fn try_coerce(&mut self, value: Value, expected: &Type) -> Option<Value> {
        self._coerce(value, expected, None)
    }
    
    fn _coerce(&mut self, value: Value, mut expected: &Type, range: Option<&SourceRange>) -> Option<Value> {
        if let Type::Optional(t) = expected {
            if value.is_unit() { return Some(Value::UNIT); }
            expected = t;
        }
        
        match (expected, &value) {
            (Type::Unit, v) if v.is_unit() => return Some(Value::UNIT),
            
            (Type::AnyValue, _) |
            (Type::Bool, Value::Bool(..)) |
            (Type::Int, Value::Int(..)) |
            (Type::Str, Value::Str(..)) |
            (Type::AnyHTML, Value::HTML(..)) |
            (Type::Function, Value::Func(..)) => return Some(value),
            
            (Type::Str, Value::Bool(b)) => return Some(Token::bool_to_string(*b).into()),
            (Type::Str, Value::Int(i)) => return Some(i.to_string().into()),
            
            (Type::Dict(t), Value::Dict(vs)) => {
                let mut coerced_vs = ValueMap::new();
                let mut any_errors = false;
                for (k, v) in vs.iter() {
                    if let Some(coerced_v) = self._coerce(v.clone(), t, range) {
                        coerced_vs.insert(k.clone(), coerced_v);
                    } else {
                        any_errors = true;
                        break;
                    }
                }
                if !any_errors { return Some(Value::dict(coerced_vs)); }
            },
            (Type::List(t), Value::List(vs)) => {
                let vs = vs.as_ref();
                let mut coerced_vs = Vec::new();
                let mut any_errors = false;
                for v in vs.iter() {
                    if let Some(coerced_v) = self._coerce(v.clone(), t, range) {
                        coerced_vs.push(coerced_v);
                    } else {
                        any_errors = true;
                        break;
                    }
                }
                if !any_errors { return Some(Value::list(coerced_vs)); }
            },
            
            (_, Value::Func(..)) |
            (Type::Inline, Value::Dict(..) | Value::List(..)) => {},
            
            (Type::AnyHTML, _) => return Some(self.compile_value(value).into()),
            (Type::Block, _) => {
                let mut r = self.compile_value(value);
                if !r.is_block() {
                    r = HTML::tag(str_ids::P, r);
                }
                return Some(Value::HTML(r));
            },
            (Type::Inline, _) => {
                let v = self.compile_value(value);
                return if v.is_block() {
                    if let Some(range) = range {
                        self.diagnostics.err_expected_type(&Type::Inline, &Type::Block, range);
                    }
                    None
                } else {
                    Some(Value::HTML(v))
                }
            },
            
            _ => {},
        }
        
        if let Some(range) = range {
            self.diagnostics.err_expected_type(expected, &value.get_type(), &range);
        }
        None
    }
}
