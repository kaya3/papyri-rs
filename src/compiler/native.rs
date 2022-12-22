use std::rc::Rc;

use crate::errors::{ice, ice_at, RuntimeError, RuntimeWarning, ModuleError, TypeError};
use crate::utils::{str_ids, text, NameID, relpath, SliceRef};
use crate::utils::sourcefile::SourceRange;
use super::base::Compiler;
use super::frame::ActiveFrame;
use super::func::Func;
use super::html::HTML;
use super::regex_value::RegexValue;
use super::signature::{FuncParam, FuncSignature};
use super::tag::Tag;
use super::types::Type;
use super::value::{Value, ValueMap};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NativeFunc {
    Add,
    Bind,
    Code,
    EscapeHTML,
    Filter,
    Import,
    Include,
    Join,
    ListFiles,
    Map,
    Raise,
    RegexCompile,
    RegexFind,
    RegexFindAll,
    Slice,
    Sorted,
    UniqueID,
    WriteFile,
}

pub(super) struct NativeDefs {
    pub(super) add: Func,
    pub(super) bind: Func,
    pub(super) code: Func,
    pub(super) escape_html: Func,
    pub(super) filter: Func,
    pub(super) import: Func,
    pub(super) include: Func,
    pub(super) join: Func,
    pub(super) list_files: Func,
    pub(super) map: Func,
    pub(super) raise: Func,
    pub(super) regex_compile: Func,
    pub(super) regex_find: Func,
    pub(super) regex_find_all: Func,
    pub(super) slice: Func,
    pub(super) sorted: Func,
    pub(super) unique_id: Func,
    pub(super) write_file: Func,
}

impl NativeDefs {
    pub(super) fn build() -> NativeDefs {
        let add = FuncSignature::builder()
            .pos_spread(FuncParam::new(str_ids::_0, Type::Int.list()))
            .build();
        
        let bind = FuncSignature::builder()
            .positional(FuncParam::new(str_ids::_0, Type::Function))
            .pos_spread(FuncParam::new(str_ids::_1, Type::Any.list()))
            .named_spread(FuncParam::new(str_ids::KWARGS, Type::Any.dict()))
            .content(FuncParam::new(str_ids::PARAM, Type::Any))
            .build();
        
        let content_str = FuncSignature::builder()
            .content(FuncParam::new(str_ids::PARAM, Type::Str))
            .build();
        
        let code = FuncSignature::builder()
            .named(FuncParam::new(str_ids::LANGUAGE, Type::Str.option()).implicit().unit_default())
            .named(FuncParam::new(str_ids::CODE_BLOCK, Type::Bool).with_default(false))
            .named(FuncParam::new(str_ids::FIRST_LINE_NO, Type::Int).with_default(1))
            .content(FuncParam::new(str_ids::PARAM, Type::Str))
            .build();
        
        let escape_html = FuncSignature::builder()
            .content(FuncParam::new(str_ids::PARAM, Type::HTML))
            .build();
        
        let filter = FuncSignature::builder()
            .positional(FuncParam::new(str_ids::_0, Type::Function.option()).unit_default())
            .content(FuncParam::new(str_ids::PARAM, Type::Any.list()))
            .build();
        
        let join = FuncSignature::builder()
            .positional(FuncParam::new(str_ids::_0, Type::Any).unit_default())
            .content(FuncParam::new(str_ids::PARAM, Type::HTML.list()))
            .build();
        
        let map = FuncSignature::builder()
            .positional(FuncParam::new(str_ids::_0, Type::Function))
            .content(FuncParam::new(str_ids::PARAM, Type::Any.list()))
            .build();
        
        let regex_find = FuncSignature::builder()
            .positional(FuncParam::new(str_ids::_0, Type::Regex))
            .content(FuncParam::new(str_ids::PARAM, Type::Str))
            .build();
        
        let slice = FuncSignature::builder()
            .positional(FuncParam::new(str_ids::_0, Type::Int))
            .positional(FuncParam::new(str_ids::_1, Type::Int.option()).unit_default())
            .content(FuncParam::new(str_ids::PARAM, Type::Any.list()))
            .build();
        
        let sorted = FuncSignature::builder()
            .named(FuncParam::new(str_ids::KEY, Type::Function.option()).unit_default())
            .named(FuncParam::new(str_ids::REVERSED, Type::Bool).with_default(false))
            .content(FuncParam::new(str_ids::PARAM, Type::Any.list()))
            .build();
        
        let unique_id = FuncSignature::builder()
            .named(FuncParam::new(str_ids::MAX_LENGTH, Type::Int).with_default(128))
            .content(FuncParam::new(str_ids::PARAM, Type::Str))
            .build();
        
        let write_file = FuncSignature::builder()
            .positional(FuncParam::new(str_ids::_0, Type::Str))
            .content(FuncParam::new(str_ids::PARAM, Type::HTML))
            .build();
        
        NativeDefs {
            add: Func::Native(NativeFunc::Add, add),
            bind: Func::Native(NativeFunc::Bind, bind),
            code: Func::Native(NativeFunc::Code, code),
            escape_html: Func::Native(NativeFunc::EscapeHTML, escape_html),
            filter: Func::Native(NativeFunc::Filter, filter),
            import: Func::Native(NativeFunc::Import, content_str.clone()),
            include: Func::Native(NativeFunc::Include, content_str.clone()),
            join: Func::Native(NativeFunc::Join, join),
            list_files: Func::Native(NativeFunc::ListFiles, content_str.clone()),
            map: Func::Native(NativeFunc::Map, map),
            raise: Func::Native(NativeFunc::Raise, content_str.clone()),
            regex_compile: Func::Native(NativeFunc::RegexCompile, content_str),
            regex_find: Func::Native(NativeFunc::RegexFind, regex_find.clone()),
            regex_find_all: Func::Native(NativeFunc::RegexFindAll, regex_find),
            slice: Func::Native(NativeFunc::Slice, slice),
            sorted: Func::Native(NativeFunc::Sorted, sorted),
            unique_id: Func::Native(NativeFunc::UniqueID, unique_id),
            write_file: Func::Native(NativeFunc::WriteFile, write_file),
        }
    }
    
    pub(super) fn to_frame(&self) -> ActiveFrame {
        impl Func {
            fn entry(&self) -> (NameID, Value) {
                (self.name_id(), self.clone().into())
            }
        }
        
        fn dict_entry<T>(name_id: NameID, map: T) -> (NameID, Value)
        where ValueMap: From<T> {
            (name_id, ValueMap::from(map).into())
        }
        
        let bindings = ValueMap::from([
            dict_entry(str_ids::INT, [
                self.add.entry(),
            ]),
            
            dict_entry(str_ids::STR, [
                self.unique_id.entry(),
            ]),
            
            dict_entry(str_ids::HTML, [
                self.escape_html.entry(),
            ]),
            
            dict_entry(str_ids::LIST, [
                self.filter.entry(),
                self.join.entry(),
                self.map.entry(),
                self.slice.entry(),
                self.sorted.entry(),
            ]),
            
            dict_entry(str_ids::FUNCTION, [
                self.bind.entry(),
            ]),
            
            dict_entry(str_ids::REGEX, [
                self.regex_compile.entry(),
                self.regex_find.entry(),
                self.regex_find_all.entry(),
            ]),
            
            self.code.entry(),
            self.import.entry(),
            self.include.entry(),
            self.list_files.entry(),
            self.raise.entry(),
            self.write_file.entry(),
        ]);
        
        ActiveFrame::new(None, bindings, None)
    }
}

impl NativeFunc {
    pub(super) fn name_id(&self) -> NameID {
        match self {
            NativeFunc::Add => str_ids::ADD,
            NativeFunc::Bind => str_ids::BIND,
            NativeFunc::Code => str_ids::CODE,
            NativeFunc::EscapeHTML => str_ids::ESCAPE_HTML,
            NativeFunc::Filter => str_ids::FILTER,
            NativeFunc::Import => str_ids::IMPORT,
            NativeFunc::Include => str_ids::INCLUDE,
            NativeFunc::Join => str_ids::JOIN,
            NativeFunc::ListFiles => str_ids::LIST_FILES,
            NativeFunc::Map => str_ids::MAP,
            NativeFunc::Raise => str_ids::RAISE,
            NativeFunc::RegexCompile => str_ids::COMPILE,
            NativeFunc::RegexFind => str_ids::FIND,
            NativeFunc::RegexFindAll => str_ids::FIND_ALL,
            NativeFunc::Slice => str_ids::SLICE,
            NativeFunc::Sorted => str_ids::SORTED,
            NativeFunc::UniqueID => str_ids::UNIQUE_ID,
            NativeFunc::WriteFile => str_ids::WRITE_FILE,
        }
    }
}

impl <'a> Compiler<'a> {
    pub(super) fn evaluate_native_func(&mut self, f: NativeFunc, bindings: ValueMap, call_range: SourceRange) -> Option<Value> {
        let mut bindings = Bindings(bindings);
        match f {
            NativeFunc::Add => {
                let args = bindings.take_list(str_ids::_0);
                
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
                let pos_args = bindings.take_list(str_ids::_1);
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
            
            NativeFunc::EscapeHTML => {
                let content = bindings.take(str_ids::PARAM);
                
                let mut s = Vec::new();
                self.ctx.render(&self.compile_value(content), true, &mut s).unwrap();
                Some(String::from_utf8(s).unwrap().into())
            },
            
            NativeFunc::Import | NativeFunc::Include | NativeFunc::ListFiles => {
                let path_str = bindings.take_str(str_ids::PARAM);
                
                // compute path relative to current source file
                let mut path = self.ctx.source_files
                    .get(call_range.src_id)
                    .path
                    .to_path_buf();
                
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
            
            NativeFunc::RegexCompile => {
                let regex_str = bindings.take_str(str_ids::PARAM);
                self.compile_regex(regex_str.as_ref(), call_range)
                    .map(Value::from)
            },
            
            NativeFunc::RegexFind => {
                let regex = bindings.take_regex(str_ids::_0);
                let s = bindings.take_str(str_ids::PARAM);
                Some(regex.find(s.as_ref()))
            },
            
            NativeFunc::RegexFindAll => {
                let regex = bindings.take_regex(str_ids::_0);
                let s = bindings.take_str(str_ids::PARAM);
                Some(regex.find_all(s.as_ref()))
            },
            
            NativeFunc::Slice => {
                let content = bindings.take_list(str_ids::PARAM);
                let n = content.len() as i64;
                
                let start = bindings.take_int(str_ids::_0);
                let end = bindings.take_int_option(str_ids::_1).unwrap_or(n);
                
                let a = if start < 0 { 0.max(start + n) } else { start.min(n) };
                let b = if end < 0 { 0.max(end + n) } else { end.min(n) };
                
                let r = content.slice(a.min(b) as usize, b as usize);
                Some(r.into())
            },
            
            NativeFunc::Sorted => {
                let key_func = bindings.take_function_option(str_ids::KEY);
                let reversed = bindings.take_bool(str_ids::REVERSED);
                let content = bindings.take_list(str_ids::PARAM);
                
                if content.is_empty() {
                    return Some(content.into());
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
                
                // gives correct stability behaviour
                if reversed { decorated.reverse(); }
                
                match decorated.first().unwrap().0 {
                    Value::Int(_) => decorated.sort_by_key(|p| match p.0 {
                        Value::Int(k) => k,
                        _ => ice_at("failed to unwrap int sort key", call_range),
                    }),
                    Value::Str(_) => decorated.sort_by_key(|p| match &p.0 {
                        Value::Str(k) => k.clone(),
                        _ => ice_at("failed to unwrap str sort key", call_range),
                    }),
                    _ => ice_at("failed to unwrap sort key", call_range),
                }
                
                let mut undecorated: Vec<Value> = decorated.into_iter()
                    .map(|p| p.1)
                    .collect();
                
                if reversed { undecorated.reverse(); }
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
    
    fn native_code_impl(&mut self, language: Option<&str>, is_block: bool, first_line_no: i64, src: &str, call_range: SourceRange) -> Option<Value> {
        use super::highlight::{syntax_highlight, enumerate_lines, no_highlighting};
        
        let mut tag = Tag::new(str_ids::CODE, HTML::Empty);
        
        let src = text::fix_indentation(src);
        let lines = language
            .and_then(|language| {
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
    
    fn eval_callback(&mut self, callback: Func, arg: Value, call_range: SourceRange) -> Option<Value> {
        let bindings = callback.bind_synthetic_call(self, false, arg, call_range)?;
        self.evaluate_func_call_with_bindings(
            callback,
            bindings,
            &Type::Any,
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
    
    fn take_regex(&mut self, key: NameID) -> Rc<RegexValue> {
        match self.take(key) {
            Value::Regex(r) => r,
            _ => ice("failed to unpack"),
        }
    }
    
    fn take_int_option(&mut self, key: NameID) -> Option<i64> {
        match self.take(key) {
            Value::Int(i) => Some(i),
            v if v.is_unit() => None,
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
