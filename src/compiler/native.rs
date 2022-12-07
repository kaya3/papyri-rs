use std::rc::Rc;

use crate::errors::{ice, ice_at, RuntimeError, Warning, RuntimeWarning, ModuleError, TypeError};
use crate::utils::{str_ids, text, NameID, SourceRange, relpath, SliceRef};
use super::compiler::Compiler;
use super::frame::ActiveFrame;
use super::func::Func;
use super::html::HTML;
use super::signature::{FuncParam, FuncSignature};
use super::tag::Tag;
use super::types::Type;
use super::value::{Value, ValueMap};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NativeFunc {
    Add,
    Bind,
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

pub struct NativeDefs {
    pub add: Func,
    pub bind: Func,
    pub code: Func,
    pub export: Func,
    pub filter: Func,
    pub import: Func,
    pub include: Func,
    pub join: Func,
    pub list_files: Func,
    pub map: Func,
    pub raise: Func,
    pub sorted: Func,
    pub unique_id: Func,
    pub write_file: Func,
}

impl NativeDefs {
    pub fn build() -> NativeDefs {
        let add = FuncSignature::new()
            .pos_spread(FuncParam::new(str_ids::_ARGS, Type::list(Type::Int)))
            .build();
        
        let bind = FuncSignature::new()
            .positional(FuncParam::new(str_ids::_0, Type::Function))
            .pos_spread(FuncParam::new(str_ids::_ARGS, Type::list(Type::AnyValue)))
            .named_spread(FuncParam::new(str_ids::KWARGS, Type::dict(Type::AnyValue)))
            .content(FuncParam::new(str_ids::PARAM, Type::AnyValue))
            .build();
        
        let content_str = FuncSignature::new()
            .content(FuncParam::new(str_ids::PARAM, Type::Str))
            .build();
        
        let code = FuncSignature::new()
            .named(FuncParam::new(str_ids::LANGUAGE, Type::optional(Type::Str)).implicit().unit_default())
            .named(FuncParam::new(str_ids::CODE_BLOCK, Type::Bool).with_default(false))
            .named(FuncParam::new(str_ids::FIRST_LINE_NO, Type::Int).with_default(1))
            .content(FuncParam::new(str_ids::PARAM, Type::Str))
            .build();
        
        let export = FuncSignature::new()
            .named_spread(FuncParam::new(str_ids::KWARGS, Type::dict(Type::AnyValue)))
            .build();
        
        let filter = FuncSignature::new()
            .positional(FuncParam::new(str_ids::_0, Type::optional(Type::Function)).unit_default())
            .content(FuncParam::new(str_ids::PARAM, Type::list(Type::AnyValue)))
            .build();
        
        let join = FuncSignature::new()
            .positional(FuncParam::new(str_ids::_0, Type::AnyValue).unit_default())
            .content(FuncParam::new(str_ids::PARAM, Type::list(Type::AnyHTML)))
            .build();
        
        let map = FuncSignature::new()
            .positional(FuncParam::new(str_ids::_0, Type::Function))
            .content(FuncParam::new(str_ids::PARAM, Type::list(Type::AnyValue)))
            .build();
        
        let sorted = FuncSignature::new()
            .named(FuncParam::new(str_ids::KEY, Type::optional(Type::Function)).unit_default())
            .named(FuncParam::new(str_ids::REVERSE, Type::Bool).with_default(false))
            .content(FuncParam::new(str_ids::PARAM, Type::list(Type::AnyValue)))
            .build();
        
        let unique_id = FuncSignature::new()
            .named(FuncParam::new(str_ids::MAX_LENGTH, Type::Int).with_default(128))
            .content(FuncParam::new(str_ids::PARAM, Type::Str))
            .build();
        
        let write_file = FuncSignature::new()
            .positional(FuncParam::new(str_ids::_0, Type::Str))
            .content(FuncParam::new(str_ids::PARAM, Type::AnyHTML))
            .build();
        
        NativeDefs {
            add: Func::Native(NativeFunc::Add, add),
            bind: Func::Native(NativeFunc::Bind, bind),
            code: Func::Native(NativeFunc::Code, code),
            export: Func::Native(NativeFunc::Export, export),
            filter: Func::Native(NativeFunc::Filter, filter),
            import: Func::Native(NativeFunc::Import, content_str.clone()),
            include: Func::Native(NativeFunc::Include, content_str.clone()),
            join: Func::Native(NativeFunc::Join, join),
            list_files: Func::Native(NativeFunc::ListFiles, content_str.clone()),
            map: Func::Native(NativeFunc::Map, map),
            raise: Func::Native(NativeFunc::Raise, content_str),
            sorted: Func::Native(NativeFunc::Sorted, sorted),
            unique_id: Func::Native(NativeFunc::UniqueID, unique_id),
            write_file: Func::Native(NativeFunc::WriteFile, write_file),
        }
    }
    
    pub fn to_frame(&self) -> ActiveFrame {
        let bindings = [
            &self.add,
            &self.bind,
            &self.code,
            &self.export,
            &self.filter,
            &self.import,
            &self.include,
            &self.join,
            &self.list_files,
            &self.map,
            &self.raise,
            &self.sorted,
            &self.unique_id,
            &self.write_file,
        ].into_iter()
            .cloned()
            .map(|f| (f.name_id(), Value::Func(f)))
            .collect();
        
        ActiveFrame::new(None, bindings, None)
    }
}

impl NativeFunc {
    pub fn name_id(&self) -> NameID {
        match self {
            NativeFunc::Add => str_ids::ADD,
            NativeFunc::Bind => str_ids::BIND,
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
            NativeFunc::Add => {
                let args = bindings.take_list(str_ids::_ARGS);
                
                let mut total = 0;
                for arg in args.as_ref().iter() {
                    match arg {
                        Value::Int(i) => total += i,
                        _ => ice_at("failed to coerce", call_range),
                    }
                }
                Some(total.into())
            },
            
            NativeFunc::Bind => {
                let f = bindings.take_function(str_ids::_0);
                let pos_args = bindings.take_list(str_ids::_ARGS);
                let named_args = bindings.take_dict(str_ids::KWARGS);
                let content_arg = bindings.take(str_ids::PARAM);
                
                f.bind_partial(self, pos_args.as_ref(), named_args.as_ref(), content_arg, call_range)
                    .map(Value::from)
            },
            
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
                let args = bindings.take_dict(str_ids::KWARGS);
                
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
                    
                    Some(paths.into())
                } else {
                    // import or include
                    let (module_out, module_exports) = self.ctx.load_cached(&path)
                        .map_err(|e| self.module_error(&path, e, call_range))
                        .ok()?;
                    
                    Some(if f == NativeFunc::Import {
                        Value::Dict(module_exports)
                    } else {
                        self.set_vars(module_exports.as_ref(), false, call_range);
                        module_out.into()
                    })
                }
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
        use super::highlight::{syntax_highlight, enumerate_lines, no_highlighting};
        
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
        let bindings = callback.bind_synthetic_call(self, false, arg, call_range)?;
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
