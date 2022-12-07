use std::rc::Rc;
use indexmap::IndexMap;

use crate::errors::{ice, ice_at, RuntimeError, Warning, RuntimeWarning, ModuleError, TypeError};
use crate::utils::{str_ids, text, NameID, SourceRange, relpath, SliceRef};
use super::frame::ActiveFrame;
use super::func::{FuncSignature, FuncParam, Func};
use super::highlight::{syntax_highlight, enumerate_lines, no_highlighting};
use super::html::HTML;
use super::compiler::Compiler;
use super::tag::Tag;
use super::types::Type;
use super::value::{Value, ValueMap};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NativeFunc {
    Code,
    Export,
    Filter,
    Import,
    Include,
    Join,
    ListFiles,
    Map,
    Raise,
    Sorted,
    UniqueID,
    WriteFile,
}

pub fn get_natives_frame() -> ActiveFrame {
    let content_str = Rc::new(FuncSignature {
        positional_params: Box::new([]),
        spread_param: None,
        named_params: IndexMap::new(),
        spread_named_param: None,
        content_param: FuncParam::new(str_ids::PARAM, Type::Str),
    });
    
    let code = Rc::new(FuncSignature {
        positional_params: Box::new([]),
        spread_param: None,
        named_params: IndexMap::from([
            (str_ids::LANGUAGE, FuncParam::new(str_ids::LANGUAGE, Type::optional(Type::Str)).implicit().with_default(Value::UNIT)),
            (str_ids::CODE_BLOCK, FuncParam::new(str_ids::CODE_BLOCK, Type::Bool).with_default(Value::Bool(false))),
            (str_ids::FIRST_LINE_NO, FuncParam::new(str_ids::FIRST_LINE_NO, Type::Int).with_default(Value::Int(1))),
        ]),
        spread_named_param: None,
        content_param: FuncParam::new(str_ids::PARAM, Type::Str),
    });
    
    let export = Rc::new(FuncSignature {
        positional_params: Box::new([]),
        spread_param: None,
        named_params: IndexMap::new(),
        spread_named_param: Some(FuncParam::new(str_ids::PARAM, Type::dict(Type::AnyValue))),
        content_param: FuncParam::new(str_ids::ANONYMOUS, Type::Unit),
    });
    
    let filter = Rc::new(FuncSignature {
        positional_params: Box::new([
            FuncParam::new(str_ids::_0, Type::optional(Type::Function)).with_default(Value::UNIT),
        ]),
        spread_param: None,
        named_params: IndexMap::new(),
        spread_named_param: None,
        content_param: FuncParam::new(str_ids::PARAM, Type::list(Type::AnyValue)),
    });
    
    let join = Rc::new(FuncSignature {
        positional_params: Box::new([
            FuncParam::new(str_ids::_0, Type::AnyValue).with_default(Value::UNIT),
        ]),
        spread_param: None,
        named_params: IndexMap::new(),
        spread_named_param: None,
        content_param: FuncParam::new(str_ids::PARAM, Type::list(Type::AnyHTML)),
    });
    
    let map = Rc::new(FuncSignature {
        positional_params: Box::new([
            FuncParam::new(str_ids::_0, Type::Function),
        ]),
        spread_param: None,
        named_params: IndexMap::new(),
        spread_named_param: None,
        content_param: FuncParam::new(str_ids::PARAM, Type::list(Type::AnyValue)),
    });
    
    let sorted = Rc::new(FuncSignature {
        positional_params: Box::new([]),
        spread_param: None,
        named_params: IndexMap::from([
            (str_ids::KEY, FuncParam::new(str_ids::KEY, Type::optional(Type::Function)).with_default(Value::UNIT)),
            (str_ids::REVERSE, FuncParam::new(str_ids::REVERSE, Type::Bool).with_default(Value::Bool(false))),
        ]),
        spread_named_param: None,
        content_param: FuncParam::new(str_ids::PARAM, Type::list(Type::AnyValue)),
    });
    
    let unique_id = Rc::new(FuncSignature {
        positional_params: Box::new([]),
        spread_param: None,
        named_params: IndexMap::from([
            (str_ids::MAX_LENGTH, FuncParam::new(str_ids::MAX_LENGTH, Type::Int).with_default(Value::Int(128))),
        ]),
        spread_named_param: None,
        content_param: FuncParam::new(str_ids::PARAM, Type::Str),
    });
    
    let write_file = Rc::new(FuncSignature {
        positional_params: Box::new([
            FuncParam::new(str_ids::_0, Type::Str),
        ]),
        spread_param: None,
        named_params: IndexMap::new(),
        spread_named_param: None,
        content_param: FuncParam::new(str_ids::PARAM, Type::AnyHTML),
    });
    
    let bindings: ValueMap = [
        (NativeFunc::Code, code),
        (NativeFunc::Export, export),
        (NativeFunc::Filter, filter),
        (NativeFunc::Import, content_str.clone()),
        (NativeFunc::Include, content_str.clone()),
        (NativeFunc::Join, join),
        (NativeFunc::ListFiles, content_str.clone()),
        (NativeFunc::Map, map),
        (NativeFunc::Raise, content_str),
        (NativeFunc::Sorted, sorted),
        (NativeFunc::UniqueID, unique_id),
        (NativeFunc::WriteFile, write_file),
    ].into_iter().map(|(f, sig)| {
        (f.name_id(), Value::Func(Func::Native(f, sig)))
    }).collect();
    
    ActiveFrame::new(None, bindings, None)
}

impl NativeFunc {
    pub fn name_id(&self) -> NameID {
        match self {
            NativeFunc::Code => str_ids::CODE,
            NativeFunc::Export => str_ids::EXPORT,
            NativeFunc::Filter => str_ids::FILTER,
            NativeFunc::Import => str_ids::IMPORT,
            NativeFunc::Include => str_ids::INCLUDE,
            NativeFunc::Join => str_ids::JOIN,
            NativeFunc::ListFiles => str_ids::LIST_FILES,
            NativeFunc::Map => str_ids::MAP,
            NativeFunc::Raise => str_ids::RAISE,
            NativeFunc::Sorted => str_ids::SORTED,
            NativeFunc::UniqueID => str_ids::UNIQUE_ID,
            NativeFunc::WriteFile => str_ids::WRITE_FILE,
        }
    }
}

impl <'a> Compiler<'a> {
    pub fn evaluate_native_func(&mut self, f: NativeFunc, bindings: ValueMap, call_range: &SourceRange) -> Option<Value> {
        let mut bindings = Bindings(bindings);
        match f {
            NativeFunc::Code => {
                let language = bindings.take_str_option(str_ids::LANGUAGE);
                let is_block = bindings.take_bool(str_ids::CODE_BLOCK);
                let first_line_no = bindings.take_int(str_ids::FIRST_LINE_NO);
                let src = bindings.take_str(str_ids::PARAM);
                
                let with_hint = is_block.then(|| text::get_source_language_hint(src.as_ref()))
                    .flatten();
                
                let (language, src) = if let Some((l, s)) = with_hint {
                    (Some(l), s)
                } else {
                    (language.as_ref().map(|l| l.as_ref()), src.as_ref())
                };
                
                self.native_code_impl(language, is_block, first_line_no, src, call_range)
            },
            
            NativeFunc::Export => {
                let args = bindings.take_dict(str_ids::PARAM);
                
                for (&k, v) in args.iter() {
                    if self.exports.insert(k, v.clone()).is_some() {
                        let name = self.get_name(k).to_string();
                        self.warning(Warning::NameAlreadyExported(name), call_range);
                    }
                }
                Some(Value::UNIT)
            },
            
            NativeFunc::Import | NativeFunc::Include | NativeFunc::ListFiles => {
                let path_str = bindings.take_str(str_ids::PARAM);
                
                // compute path relative to current source file
                let mut path = call_range.src.path.to_path_buf();
                path.pop();
                if f == NativeFunc::ListFiles || path_str.ends_with(".papyri") {
                    path.push(path_str.as_ref());
                } else {
                    path.push(format!("{}.papyri", path_str));
                }
                
                if f == NativeFunc::ListFiles {
                    let base_path = path_str.trim_end_matches('/');
                    let paths: Vec<_> = relpath::find_papyri_source_files_in_dir(
                            &path,
                            |p, e| self.module_error(p, ModuleError::IOError(e), call_range),
                        )?
                        .into_iter()
                        .map(|p| format!("{base_path}/{}", p.to_string_lossy())
                            .strip_suffix(".papyri")
                            .unwrap_or_else(|| ice_at("Failed to strip .papyri extension", call_range))
                            .into()
                        )
                        .collect();
                    
                    return Some(paths.into());
                }
                
                let (module_out, module_exports) = self.ctx.load_cached(&path)
                    .map_err(|e| self.module_error(&path, e, call_range))
                    .ok()?;
                
                Some(if f == NativeFunc::Import {
                    Value::Dict(module_exports)
                } else {
                    self.set_vars(module_exports.as_ref(), false, call_range);
                    module_out.into()
                })
            },
            
            NativeFunc::Filter => {
                let callback = bindings.take_function_option(str_ids::_0);
                let content = bindings.take_list(str_ids::PARAM);
                
                let out = match callback {
                    Some(f) => {
                        let mut out = Vec::new();
                        for v in content.as_ref() {
                            match self.eval_callback(f.clone(), v.clone(), call_range)? {
                                Value::Bool(true) => out.push(v.clone()),
                                Value::Bool(false) => {},
                                _ => {
                                    self.err_expected_type(Type::Bool, v.get_type(), call_range);
                                    return None;
                                },
                            }
                        }
                        out
                    },
                    None => {
                        content.as_ref()
                            .iter()
                            .filter(|v| !v.is_unit())
                            .cloned()
                            .collect()
                    },
                };
                
                Some(out.into())
            },
            
            NativeFunc::Join => {
                let sep = self.compile_value(bindings.take(str_ids::_0));
                let content = bindings.take_list(str_ids::PARAM);
                
                let mut out = Vec::new();
                for (i, v) in content.as_ref().iter().cloned().enumerate() {
                    if i > 0 && !sep.is_empty() { out.push(sep.clone()); }
                    out.push(self.compile_value(v));
                }
                Some(HTML::seq(out).into())
            },
            
            NativeFunc::Map => {
                let callback = bindings.take_function(str_ids::_0);
                let content = bindings.take_list(str_ids::PARAM);
                
                let mut out = Vec::new();
                for v in content.as_ref() {
                    let r = self.eval_callback(callback.clone(), v.clone(), call_range)?;
                    out.push(r);
                }
                Some(out.into())
            },
            
            NativeFunc::Raise => {
                let msg = bindings.take_str(str_ids::PARAM);
                self.runtime_error(RuntimeError::Raised(msg), call_range);
                None
            },
            
            NativeFunc::Sorted => {
                let key_func = bindings.take_function_option(str_ids::KEY);
                let reverse = bindings.take_bool(str_ids::REVERSE);
                let content = bindings.take_list(str_ids::PARAM);
                
                if content.len() == 0 {
                    return Some(Value::List(content));
                }
                
                let mut decorated = Vec::new();
                let mut key_type = Type::Unit;
                for v in content.as_ref().iter().cloned() {
                    let k = key_func.clone()
                        .map_or_else(
                            || Some(v.clone()),
                            |f| self.eval_callback(f, v.clone(), call_range),
                        )?;
                    
                    if !matches!(k, Value::Int(_) | Value::Str(_)) {
                        self.type_error(TypeError::SortKeyInvalid(k.get_type()), call_range);
                        return None;
                    } else if matches!((&key_type, &k), (Type::Int, Value::Str(_)) | (Type::Str, Value::Int(_))) {
                        self.type_error(TypeError::SortKeyHeterogeneous, call_range);
                        return None;
                    } else if matches!(key_type, Type::Unit) {
                        key_type = k.get_type();
                    }
                    
                    decorated.push((k, v));
                }
                
                match decorated.first().unwrap().0 {
                    Value::Int(_) => decorated.sort_by_key(|p| match p.0 {
                        Value::Int(k) => k,
                        _ => ice_at("failed to unwrap int sort key", call_range),
                    }),
                    Value::Str(_) => decorated.sort_by_key(|p| match p.0.clone() {
                        Value::Str(k) => k,
                        _ => ice_at("failed to unwrap str sort key", call_range),
                    }),
                    _ => ice_at("failed to unwrap sort key", call_range),
                }
                
                let mut undecorated: Vec<Value> = decorated.into_iter()
                    .map(|p| p.1)
                    .collect();
                
                if reverse { undecorated.reverse(); }
                Some(undecorated.into())
            },
            
            NativeFunc::UniqueID => {
                let max_len = bindings.take_int(str_ids::MAX_LENGTH);
                let id_base = bindings.take_str(str_ids::PARAM);
                
                if max_len <= 0 {
                    let e = RuntimeError::ParamMustBePositive(self.get_name(str_ids::MAX_LENGTH).to_string(), max_len);
                    self.runtime_error(e, call_range);
                    return None;
                }
                
                let id = self.ctx.unique_ids.get_unique_id(id_base.as_ref(), max_len as usize);
                Some(Value::Str(id))
            },
            
            NativeFunc::WriteFile => {
                let path = bindings.take_str(str_ids::_0);
                let content = self.compile_value(bindings.take(str_ids::PARAM));
                
                self.ctx.push_out_file(path, content)
                    .map_err(|e| self.runtime_error(e, call_range))
                    .ok()?;
                
                Some(Value::UNIT)
            },
        }
    }
    
    fn native_code_impl(&mut self, language: Option<&str>, is_block: bool, first_line_no: i64, src: &str, call_range: &SourceRange) -> Option<Value> {
        let mut tag = Tag::new(str_ids::CODE, HTML::Empty);
        
        let src = text::fix_indentation(src);
        let lines = language
            .map(|language| {
                if let Some(lines) = syntax_highlight(&src, language) {
                    tag.attributes.insert(str_ids::CLASS, Some(format!("syntax-highlight lang-{language}").into()));
                    Some(lines)
                } else {
                    let w = if cfg!(feature = "syntect") {
                        RuntimeWarning::HighlightLanguageUnknown(language.to_string())
                    } else {
                        RuntimeWarning::HighlightNotEnabled
                    };
                    self.runtime_warning(w, call_range);
                    None
                }
            })
            .flatten()
            .unwrap_or_else(|| no_highlighting(&src));
        
        tag.content = if is_block {
            enumerate_lines(lines, first_line_no)
        } else {
            if first_line_no != 1 {
                self.runtime_warning(RuntimeWarning::InlineHighlightEnumerate, call_range);
            }
            if lines.len() > 1 {
                self.runtime_warning(RuntimeWarning::InlineHighlightMultiline, call_range);
            }
            HTML::seq(lines)
        };
        
        Some(tag.into())
    }
    
    fn eval_callback(&mut self, callback: Func, arg: Value, call_range: &SourceRange) -> Option<Value> {
        let bindings = callback.signature()
            .bind_synthetic_call(self, false, arg, call_range)?;
        self.evaluate_func_call_with_bindings(
            callback,
            bindings,
            &Type::AnyValue,
            call_range,
        )
    }
}

struct Bindings(ValueMap);

impl Bindings {
    fn take(&mut self, key: NameID) -> Value {
        self.0.get_mut(&key)
            .map(std::mem::take)
            .unwrap_or_else(|| ice("failed to unpack"))
    }
    
    fn take_bool(&mut self, key: NameID) -> bool {
        match self.take(key) {
            Value::Bool(b) => b,
            _ => ice("failed to unpack"),
        }
    }
    
    fn take_int(&mut self, key: NameID) -> i64 {
        match self.take(key) {
            Value::Int(i) => i,
            _ => ice("failed to unpack"),
        }
    }
    
    fn take_str(&mut self, key: NameID) -> Rc<str> {
        match self.take(key) {
            Value::Str(s) => s,
            _ => ice("failed to unpack"),
        }
    }
    
    fn take_list(&mut self, key: NameID) -> SliceRef<Value> {
        match self.take(key) {
            Value::List(l) => l,
            _ => ice("failed to unpack"),
        }
    }
    
    fn take_dict(&mut self, key: NameID) -> Rc<ValueMap> {
        match self.take(key) {
            Value::Dict(d) => d,
            _ => ice("failed to unpack"),
        }
    }
    
    fn take_function(&mut self, key: NameID) -> Func {
        match self.take(key) {
            Value::Func(f) => f,
            _ => ice("failed to unpack"),
        }
    }
    
    fn take_str_option(&mut self, key: NameID) -> Option<Rc<str>> {
        match self.take(key) {
            Value::Str(s) => Some(s),
            v if v.is_unit() => None,
            _ => ice("failed to unpack"),
        }
    }
    
    fn take_function_option(&mut self, key: NameID) -> Option<Func> {
        match self.take(key) {
            Value::Func(f) => Some(f),
            v if v.is_unit() => None,
            _ => ice("failed to unpack"),
        }
    }
}
