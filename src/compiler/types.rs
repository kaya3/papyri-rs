use crate::errors::{self, TypeError};
use crate::utils::{SourceRange, str_ids};
use crate::parser::{ast, Token};
use super::compiler::Compiler;
use super::html::HTML;
use super::value::Value;

#[derive(Clone, PartialEq, Eq)]
/// Represents a type in the Papyri language.
pub enum Type {
    /// The unit type `none`, inhabited only by `Value::UNIT`.
    Unit,
    
    /// The top type `any`, which all values are assignable to.
    Any,
    
    /// The type `html`, representing any HTML content; `block` and `inline`
    /// are subtypes.
    HTML,
    
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
    /// unit value. Optional types are normalised so that the unit value is not
    /// assignable to `T` itself.
    Optional(Box<Type>),
}

impl Type {
    /// Creates a representation of a `dict` type.
    pub fn dict(self: Type) -> Type {
        Type::Dict(Box::new(self))
    }
    
    /// Creates a representation of a `list` type.
    pub fn list(self) -> Type {
        Type::List(Box::new(self))
    }
    
    /// Creates a representation of an optional type. The representation is
    /// normalised by simplifying `T?` to `T` whenever the unit value is
    /// already assignable to `T`.
    pub fn option(self) -> Type {
        if self.unit_is_assignable() {
            self
        } else {
            Type::Optional(Box::new(self))
        }
    }
    
    /// Converts this type to an optional type, if the given condition is true.
    pub fn option_if(self, condition: bool) -> Type {
        if condition { self.option() } else { self }
    }
    
    /// Indicates whether this type is either `html`, `block` or `inline`. The
    /// types `any` and `none` are not considered to be HTML types.
    pub fn is_html(&self) -> bool {
        matches!(self, Type::HTML | Type::Block | Type::Inline)
    }
    
    /// Indicates whether the unit value is assignable to this type.
    pub fn unit_is_assignable(&self) -> bool {
        matches!(self, Type::Any |
            Type::HTML |
            Type::Block |
            Type::Inline |
            Type::Unit |
            Type::Optional(..))
    }
    
    /// Returns the type for list or dictionary components, if this type is
    /// used as a type hint for a list or dictionary value. The returned type
    /// is meaningless if a list or dictionary could never be coerced to this
    /// type.
    pub fn component_type(&self) -> &Type {
        match self {
            Type::Dict(t) | Type::List(t) => t,
            Type::Optional(t) => t.component_type(),
            Type::Block | Type::HTML => &Type::HTML,
            _ => &Type::Any,
        }
    }
    
    /// Returns the strongest type representing all values of both `self` and
    /// `other`.
    pub fn least_upper_bound(self, other: Type) -> Type {
        match (self, other) {
            (t1, t2) if t1 == t2 => t1,
            (Type::Any, t) | (t, Type::Any) => t,
            (t, Type::Unit) | (Type::Unit, t) => t.option(),
            
            (t1, t2) if t1.is_html() && t2.is_html() => Type::HTML,
            
            (Type::List(t1), Type::List(t2)) => t1.least_upper_bound(*t2).list(),
            (Type::Dict(t1), Type::Dict(t2)) => t1.least_upper_bound(*t2).dict(),
            (Type::Optional(t1), Type::Optional(t2)) => t1.least_upper_bound(*t2).option(),
            (Type::Optional(t1), t2) | (t2, Type::Optional(t1)) => t1.least_upper_bound(t2).option(),
            
            _ => Type::Any,
        }
    }
    
    /// Compiles an AST type annotation into the Papyri type it represents.
    pub fn compile(type_annotation: &ast::TypeAnnotation) -> Type {
        match type_annotation {
            ast::TypeAnnotation::Primitive(range) => match range.as_str() {
                "any" => Type::Any,
                "html" => Type::HTML,
                "block" => Type::Block,
                "inline" => Type::Inline,
                "none" => Type::Unit,
                "bool" => Type::Bool,
                "int" => Type::Int,
                "str" => Type::Str,
                "function" => Type::Function,
                _ => errors::ice_at("not a primitive type", range),
            },
            ast::TypeAnnotation::Group(child, range) => {
                let child = Type::compile(child);
                match range.as_str() {
                    "dict" => child.dict(),
                    "list" => child.list(),
                    "?" => child.option(),
                    _ => errors::ice_at("not a group type", range),
                }
            },
        }
    }
    
    /// Determines whether the given value is assignable to this type. Values
    /// which are not assignable may still be coercible.
    pub fn check_value(&self, value: Value) -> bool {
        match (self, value) {
            (t, v) if v.is_unit() => t.unit_is_assignable(),
            
            (Type::Any, _) |
            (Type::Bool, Value::Bool(_)) |
            (Type::Int, Value::Int(_)) |
            (Type::Str, Value::Str(_)) |
            (Type::Function, Value::Func(_)) |
            (Type::HTML, Value::HTML(_)) |
            (Type::HTML, Value::Str(_)) |
            (Type::Inline, Value::Str(_)) => true,
            
            (Type::Block, Value::HTML(h)) => h.is_block(),
            (Type::Inline, Value::HTML(h)) => h.is_inline(),
            
            (Type::Dict(t), Value::Dict(vs)) => t.check_all(vs.values()),
            (Type::List(t), Value::List(vs)) => t.check_all(vs.as_ref().iter()),
            (Type::Optional(t), v) => t.check_value(v),
            
            _ => false,
        }
    }
    
    fn check_all<'a, T: Iterator<Item=&'a Value>>(&self, vs: T) -> bool {
        vs.cloned()
            .all(|v| self.check_value(v))
    }
    
    fn coerce_value(&self, value: Value, value_to_html: &impl Fn(Value) -> HTML) -> Result<Value, TypeError> {
        let mut expected = self;
        if let Type::Optional(t) = expected {
            if value.is_unit() { return Ok(Value::UNIT); }
            expected = t;
        }
        
        match (expected, &value) {
            (Type::Unit, v) if v.is_unit() => return Ok(Value::UNIT),
            
            (Type::Any, _) |
            (Type::Bool, Value::Bool(..)) |
            (Type::Int, Value::Int(..)) |
            (Type::Str, Value::Str(..)) |
            (Type::HTML, Value::Str(..)) |
            (Type::Inline, Value::Str(..)) |
            (Type::HTML, Value::HTML(..)) |
            (Type::Function, Value::Func(..)) => return Ok(value),
            
            (Type::Str, &Value::Bool(b)) => return Ok(Token::bool_to_string(b).into()),
            (Type::Str, &Value::Int(i)) => return Ok(i.to_string().into()),
            
            (Type::Dict(t), Value::Dict(vs)) => {
                let mut coerced_vs = vs.as_ref().clone();
                t.coerce_all(coerced_vs.values_mut(), value_to_html)?;
                return Ok(coerced_vs.into());
            },
            (Type::List(t), Value::List(vs)) => {
                let mut coerced_vs = vs.as_ref().to_vec();
                t.coerce_all(coerced_vs.iter_mut(), value_to_html)?;
                return Ok(coerced_vs.into());
            },
            
            (_, Value::Func(..)) |
            (Type::Inline, Value::Dict(..) | Value::List(..)) => {},
            
            (Type::HTML, _) => {
                return Ok(value_to_html(value).into());
            },
            (Type::Block, _) => {
                let mut r = value_to_html(value);
                if r.is_inline() {
                    r = HTML::tag(str_ids::P, r);
                }
                return Ok(r.into());
            },
            (Type::Inline, _) => {
                let v = value_to_html(value.clone());
                if !v.is_block() {
                    return Ok(v.into());
                }
            },
            
            _ => {},
        }
        
        Err(TypeError::ExpectedWas(expected.clone(), value.get_type()))
    }
    
    fn coerce_all<'a, T: Iterator<Item=&'a mut Value>>(&self, vs: T, value_to_html: &impl Fn(Value) -> HTML) -> Result<(), TypeError> {
        for v in vs {
            let old_v = std::mem::take(v);
            *v = self.coerce_value(old_v, value_to_html)?;
        }
        Ok(())
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Any => f.write_str("any"),
            Type::HTML => f.write_str("html"),
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

impl <'a> Compiler<'a> {
    pub fn coerce(&mut self, value: Value, expected: &Type, range: &SourceRange) -> Option<Value> {
        expected.coerce_value(value, &|v| self.compile_value(v))
            .map_err(|e| self.type_error(e, range))
            .ok()
    }
    
    pub fn try_coerce(&mut self, value: Value, expected: &Type) -> Option<Value> {
        expected.coerce_value(value, &|v| self.compile_value(v))
            .ok()
    }
}

#[cfg(test)]
mod test {
    use super::{Type, Value, HTML};
    
    fn value_to_html(value: Value) -> HTML {
        format!("coerced type '{}' to HTML", value.get_type()).into()
    }
    
    #[test]
    fn basic_types() {
        assert_eq!(Value::UNIT.get_type(), Type::Unit);
        assert_eq!(Value::Int(23).get_type(), Type::Int);
        assert_eq!(Value::Str("foo".into()).get_type(), Type::Str);
        assert_eq!(Value::Bool(true).get_type(), Type::Bool);
    }
    
    #[test]
    fn unit_assignable() {
        assert!(Type::Unit.unit_is_assignable());
        assert!(!Type::Int.unit_is_assignable());
        assert!(!Type::Str.unit_is_assignable());
        assert!(Type::Int.option().unit_is_assignable());
    }
    
    #[test]
    fn assignable() {
        assert!(Type::Int.check_value(Value::Int(23)));
        assert!(!Type::Int.check_value("foo".into()));
        assert!(Type::Str.check_value("foo".into()));
        assert!(!Type::Str.check_value(Value::Int(23)));
    }
    
    #[test]
    fn least_upper_bound() {
        assert_eq!(Type::Int.least_upper_bound(Type::Int), Type::Int);
        assert_eq!(Type::Int.least_upper_bound(Type::Unit), Type::Int.option());
        assert_eq!(Type::Int.least_upper_bound(Type::Str), Type::Any);
    }
    
    #[test]
    fn int_list_type() {
        let list_value: Value = vec![Value::Int(23), Value::Int(42)].into();
        assert_eq!(list_value.get_type(), Type::Int.list());
    }
    
    #[test]
    fn mixed_list_type() {
        let list_value: Value = vec![Value::Int(23), Value::Bool(true)].into();
        assert_eq!(list_value.get_type(), Type::Any.list());
    }
    
    #[test]
    fn coerce_basic() {
        assert_eq!(
            Type::Str.coerce_value(Value::Bool(true), &value_to_html).unwrap(),
            "True".into(),
        );
        assert_eq!(
            Type::Str.coerce_value(Value::Int(42), &value_to_html).unwrap(),
            "42".into(),
        );
    }
    
    #[test]
    fn coerce_optional() {
        assert_eq!(
            Type::Str.option().coerce_value("foo".into(), &value_to_html).unwrap(),
            "foo".into(),
        );
    }
    
    #[test]
    fn coerce_optional_list() {
        let t = Type::Int.list().option();
        let v: Value = vec![Value::Int(23), Value::Int(42)].into();
        assert_eq!(
            t.coerce_value(v.clone(), &value_to_html).unwrap(),
            v,
        );
    }
}
