use std::collections::HashMap;
use std::rc::Rc;
use maplit::{hashmap, convert_args};

use crate::utils::{ice_at, str_ids, text, NameID, SourceRange};
use super::frame::ActiveFrame;
use super::func::{FuncSignature, FuncParam};
use super::highlight::{syntax_highlight, enumerate_lines};
use super::html::HTML;
use super::compiler::Compiler;
use super::types::Type;
use super::value::{Value, ValueMap};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NativeFunc {
    Export,
    FixIndentation,
    Implicit,
    Import,
    Include,
    Let,
    Map,
    SyntaxHighlight,
}

pub fn get_natives_frame() -> ActiveFrame {
    ActiveFrame::new(None, convert_args!(hashmap!(
        str_ids::EXPORT => NativeFunc::Export,
        str_ids::FIX_INDENTATION => NativeFunc::FixIndentation,
        str_ids::IMPLICIT => NativeFunc::Implicit,
        str_ids::IMPORT => NativeFunc::Import,
        str_ids::INCLUDE => NativeFunc::Include,
        str_ids::LET => NativeFunc::Let,
        str_ids::MAP => NativeFunc::Map,
        str_ids::SYNTAX_HIGHLIGHT => NativeFunc::SyntaxHighlight,
    )))
}

impl NativeFunc {
    pub fn name_id(&self) -> NameID {
        match self {
            NativeFunc::Export => str_ids::EXPORT,
            NativeFunc::FixIndentation => str_ids::FIX_INDENTATION,
            NativeFunc::Implicit => str_ids::IMPLICIT,
            NativeFunc::Import => str_ids::IMPORT,
            NativeFunc::Include => str_ids::INCLUDE,
            NativeFunc::Let => str_ids::LET,
            NativeFunc::Map => str_ids::MAP,
            NativeFunc::SyntaxHighlight => str_ids::SYNTAX_HIGHLIGHT,
        }
    }
    
    pub fn signature(&self) -> FuncSignature {
        match self {
            NativeFunc::Export | NativeFunc::Implicit | NativeFunc::Let => FuncSignature {
                positional_params: Box::new([]),
                spread_param: None,
                named_params: HashMap::new(),
                spread_named_param: Some(FuncParam::new(str_ids::ARGS, Type::dict(Type::AnyValue))),
                content_param: FuncParam::new(str_ids::CONTENT, Type::Unit),
            },
            
            NativeFunc::FixIndentation => FuncSignature {
                positional_params: Box::new([]),
                spread_param: None,
                named_params: HashMap::new(),
                spread_named_param: None,
                content_param: FuncParam::new(str_ids::CONTENT, Type::Str),
            },
            
            NativeFunc::Import | NativeFunc::Include => FuncSignature {
                positional_params: Box::new([]),
                spread_param: None,
                named_params: HashMap::new(),
                spread_named_param: None,
                content_param: FuncParam::new(str_ids::PATH, Type::Str),
            },
            
            NativeFunc::Map => FuncSignature {
                positional_params: Box::new([
                    FuncParam::new(str_ids::_CALLBACK, Type::Function),
                ]),
                spread_param: None,
                named_params: HashMap::new(),
                spread_named_param: None,
                content_param: FuncParam::new(str_ids::CONTENT, Type::list(Type::AnyHTML)),
            },
            
            NativeFunc::SyntaxHighlight => FuncSignature {
                positional_params: Box::new([]),
                spread_param: None,
                named_params: hashmap!(
                    str_ids::LANGUAGE => FuncParam::new(str_ids::LANGUAGE, Type::optional(Type::Str)).implicit().with_default(Value::Unit),
                    str_ids::CODE_BLOCK => FuncParam::new(str_ids::CODE_BLOCK, Type::Bool).with_default(Value::Bool(false)),
                    str_ids::FIRST_LINE_NO => FuncParam::new(str_ids::FIRST_LINE_NO, Type::optional(Type::Int)).with_default(Value::Unit),
                ),
                spread_named_param: None,
                content_param: FuncParam::new(str_ids::CONTENT, Type::Str),
            },
        }
    }
}

impl From<NativeFunc> for Value {
    fn from(f: NativeFunc) -> Value {
        Value::NativeFunc(f)
    }
}

impl <'a> Compiler<'a> {
    pub fn evaluate_native_func(&mut self, f: NativeFunc, bindings: ValueMap, call_range: &SourceRange) -> Option<Value> {
        match f {
            NativeFunc::Export => {
                let Some(Value::Dict(args)) = bindings.get(&str_ids::ARGS) else {
                    ice_at("failed to unpack", call_range);
                };
                
                for (k, v) in args.iter() {
                    if self.exports.insert(k.clone(), v.clone()).is_some() {
                        self.diagnostics.warning(&format!("'{}' already exported", self.get_name(*k)), call_range);
                    }
                }
            },
            
            NativeFunc::FixIndentation => {
                let Some(Value::Str(content)) = bindings.get(&str_ids::CONTENT) else {
                    ice_at("failed to unpack", call_range);
                };
                let s = Rc::from(text::fix_indentation(content));
                return Some(Value::Str(s));
            },
            
            NativeFunc::Import | NativeFunc::Include => {
                let Some(Value::Str(path_str)) = bindings.get(&str_ids::PATH) else {
                    ice_at("failed to unpack", call_range);
                };
                
                let path = std::path::PathBuf::from(format!("{}.papyri", *path_str));
                let module = match self.loader.load_cached(&path, self.diagnostics) {
                    Ok(module) => module,
                    Err(msg) => {
                        self.diagnostics.error(&format!("{} when loading module \"{}\"", msg, *path_str), call_range);
                        return None;
                    },
                };
                return Some(if f == NativeFunc::Import {
                    Value::Dict(module.exports.clone())
                } else {
                    self.set_vars(module.exports.as_ref(), false, call_range);
                    Value::HTML(module.out.clone())
                });
            },
            
            NativeFunc::Implicit | NativeFunc::Let => {
                let Some(Value::Dict(args)) = bindings.get(&str_ids::ARGS) else {
                    ice_at("failed to unpack", call_range);
                };
                self.set_vars(args, f == NativeFunc::Implicit, call_range);
            },
            
            NativeFunc::Map => {
                let (Some(Value::Func(callback)), Some(Value::List(content))) = (
                    bindings.get(&str_ids::_CALLBACK),
                    bindings.get(&str_ids::CONTENT),
                ) else {
                    ice_at("failed to unpack", call_range);
                };
                
                let mut out = Vec::new();
                for v in content.iter() {
                    let bindings = callback.signature().bind_synthetic_call(self, false, v.clone(), call_range)?;
                    let r = self.evaluate_func_call_with_bindings(
                        callback.clone(),
                        bindings,
                        &Type::AnyHTML,
                    )?;
                    out.push(self.compile_value(r));
                }
                return Some(Value::HTML(HTML::seq(&out)));
            },
            
            NativeFunc::SyntaxHighlight => {
                let (Some(language), Some(Value::Bool(block)), Some(first_line_no), Some(Value::Str(src))) = (
                    bindings.get(&str_ids::LANGUAGE),
                    bindings.get(&str_ids::CODE_BLOCK),
                    bindings.get(&str_ids::FIRST_LINE_NO),
                    bindings.get(&str_ids::CONTENT),
                ) else {
                    ice_at("failed to unpack", call_range);
                };
                
                let Value::Str(language) = language else {
                    return Some(Value::HTML(HTML::text(src)));
                };
                
                let first_line_no = if let Value::Int(i) = first_line_no {
                    if !block { self.diagnostics.warning("cannot enumerate lines in inline syntax highlight", call_range); }
                    *i
                } else { 1 };
                
                let r = syntax_highlight(src.as_ref(), language.as_ref());
                return Some(Value::HTML(if *block {
                    enumerate_lines(r, first_line_no)
                } else {
                    if r.len() > 1 { self.diagnostics.warning("multi-line string in inline syntax highlight", call_range); }
                    HTML::seq(&r)
                }));
            }
        }
        
        Some(Value::Unit)
    }
}
