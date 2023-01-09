use std::rc::Rc;

use crate::errors;
use crate::parser::Type;
use crate::utils::NameIDMap;
use super::func::Func;
use super::html::HTML;
use super::regex_value::RcRegex;
use super::value::{Value, Dict, Int, RcStr, RcDict, List};

pub(super) trait TryConvert where Self: Sized {
    fn as_type() -> Type;
    fn try_convert(value: Value) -> Result<Self, ()>;
}
impl Value {
    pub(super) fn try_convert<T: TryConvert>(self) -> Result<T, errors::TypeError> {
        T::try_convert(self.clone())
            .map_err(|_| errors::TypeError::ExpectedWas(T::as_type(), self.get_type()))
    }
    pub(super) fn expect_convert<T: TryConvert>(self) -> T {
        self.try_convert()
            .unwrap_or_else(|t| errors::ice(&format!("Failed to convert: {t}")))
    }
}

impl TryConvert for Value {
    fn as_type() -> Type {
        Type::Any
    }
    fn try_convert(value: Value) -> Result<Value, ()> {
        Ok(value)
    }
}

impl From<()> for Value {
    fn from(_: ()) -> Value {
        Value::UNIT
    }
}

impl From<bool> for Value {
    fn from(b: bool) -> Value {
        Value::Bool(b)
    }
}
impl TryConvert for bool {
    fn as_type() -> Type {
        Type::Bool
    }
    fn try_convert(value: Value) -> Result<bool, ()> {
        match value {
            Value::Bool(b) => Ok(b),
            _ => Err(()),
        }
    }
}

impl From<Int> for Value {
    fn from(i: Int) -> Value {
        Value::Int(i)
    }
}
impl TryConvert for Int {
    fn as_type() -> Type {
        Type::Int
    }
    fn try_convert(value: Value) -> Result<Int, ()> {
        match value {
            Value::Int(i) => Ok(i),
            _ => Err(()),
        }
    }
}

impl From<&str> for Value {
    fn from(s: &str) -> Value {
        Value::Str(Rc::from(s))
    }
}
impl From<String> for Value {
    fn from(s: String) -> Value {
        Value::Str(Rc::from(s))
    }
}
impl From<RcStr> for Value {
    fn from(s: RcStr) -> Value {
        Value::Str(s)
    }
}
impl TryConvert for RcStr {
    fn as_type() -> Type {
        Type::Str
    }
    fn try_convert(value: Value) -> Result<RcStr, ()> {
        match value {
            Value::Str(s) => Ok(s),
            _ => Err(()),
        }
    }
}

impl From<HTML> for Value {
    fn from(html: HTML) -> Value {
        match html {
            HTML::Text(t) => Value::Str(t),
            HTML::Whitespace => " ".into(),
            HTML::RawNewline => "\n".into(),
            html => Value::HTML(html),
        }
    }
}
impl From<&HTML> for Value {
    fn from(html: &HTML) -> Value {
        html.clone().into()
    }
}
impl TryConvert for HTML {
    fn as_type() -> Type {
        Type::Html
    }
    fn try_convert(value: Value) -> Result<HTML, ()> {
        match value {
            Value::Str(s) => Ok(s.into()),
            Value::HTML(h) => Ok(h),
            _ => Err(()),
        }
    }
}

impl From<Func> for Value {
    fn from(f: Func) -> Value {
        Value::Func(f)
    }
}
impl TryConvert for Func {
    fn as_type() -> Type {
        Type::Function
    }
    fn try_convert(value: Value) -> Result<Func, ()> {
        match value {
            Value::Func(f) => Ok(f),
            _ => Err(()),
        }
    }
}

impl From<RcRegex> for Value {
    fn from(r: RcRegex) -> Self {
        Value::Regex(r)
    }
}
impl TryConvert for RcRegex {
    fn as_type() -> Type {
        Type::Regex
    }
    fn try_convert(value: Value) -> Result<RcRegex, ()> {
        match value {
            Value::Regex(r) => Ok(r),
            _ => Err(()),
        }
    }
}

impl From<Vec<Value>> for Value {
    fn from(vs: Vec<Value>) -> Value {
        Value::List(vs.into())
    }
}
impl <const N: usize> From<[Value; N]> for Value {
    fn from(vs: [Value; N]) -> Value {
        Value::List(vs.as_slice().into())
    }
}
impl From<List> for Value {
    fn from(vs: List) -> Value {
        Value::List(vs)
    }
}
impl TryConvert for List {
    fn as_type() -> Type {
        Type::Any.list()
    }
    fn try_convert(value: Value) -> Result<List, ()> {
        match value {
            Value::List(vs) => Ok(vs),
            _ => Err(()),
        }
    }
}

impl From<Dict> for Value {
    fn from(vs: Dict) -> Value {
        Value::Dict(Rc::new(vs))
    }
}
impl From<RcDict> for Value {
    fn from(vs: RcDict) -> Value {
        Value::Dict(vs)
    }
}
impl TryConvert for RcDict {
    fn as_type() -> Type {
        Type::Any.dict()
    }
    fn try_convert(value: Value) -> Result<RcDict, ()> {
        match value {
            Value::Dict(vs) => Ok(vs),
            _ => Err(()),
        }
    }
}

impl <T: TryConvert> TryConvert for Vec<T> {
    fn as_type() -> Type {
        T::as_type().list()
    }
    fn try_convert(value: Value) -> Result<Vec<T>, ()> {
        List::try_convert(value)?
            .as_ref()
            .iter()
            .cloned()
            .map(T::try_convert)
            .collect()
    }
}
impl <T: TryConvert> TryConvert for NameIDMap<T> {
    fn as_type() -> Type {
        T::as_type().dict()
    }
    fn try_convert(value: Value) -> Result<NameIDMap<T>, ()> {
        let vs: RcDict = TryConvert::try_convert(value)?;
        let mut out = NameIDMap::default();
        for (&k, v) in vs.iter() {
            out.insert(k, T::try_convert(v.clone())?);
        }
        Ok(out)
    }
}

impl <T: TryConvert> TryConvert for Option<T> {
    fn as_type() -> Type {
        T::as_type().option()
    }
    fn try_convert(value: Value) -> Result<Option<T>, ()> {
        if value.is_unit() {
            Ok(None)
        } else {
            T::try_convert(value).map(Option::Some)
        }
    }
}
