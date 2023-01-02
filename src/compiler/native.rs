use std::rc::Rc;

use crate::errors::{RuntimeError, RuntimeWarning, ModuleError, TypeError};
use crate::utils::{str_ids, text, relpath, SliceRef};
use crate::utils::sourcefile::{SourceRange, SourceFileID};
use crate::parser::Type;
use super::base::Compiler;
use super::func::Func;
use super::html::HTML;
use super::regex_value::RegexValue;
use super::tag::Tag;
use super::value::{Value, ValueMap};
use super::value_convert::TryConvert;

type Int = i64;
type Str = Rc<str>;
type Regex = Rc<RegexValue>;
type List = SliceRef<Value>;
type Dict = Rc<ValueMap>;

crate::native_defs! {
    let compiler, call_range;
    
    impl BOOL for Bool => {
        @bind_positional
        fn AND(A: positional bool, B: positional bool) {
            A & B
        }
        
        fn FROM(PARAM: content Value) {
            match PARAM {
                Value::Bool(b) => b,
                Value::Int(i) => i != 0,
                Value::Str(s) => !s.is_empty(),
                Value::HTML(h) => !h.is_empty(),
                Value::List(vs) => !vs.is_empty(),
                Value::Dict(vs) => !vs.is_empty(),
                _ => true,
            }
        }
        
        @bind_positional
        fn OR(A: positional bool, B: positional bool) {
            A | B
        }
        
        @bind_content
        fn NEGATE(PARAM: content bool) {
            !PARAM
        }
    }
    
    impl INT for Int => {
        @bind_positional
        fn ADD(ARGS: pos_spread Vec<Int>) {
            ARGS.into_iter().sum::<Int>()
        }
        
        @bind_content
        fn NEGATE(PARAM: content Int) {
            -PARAM
        }
    }
    
    impl STR for Str => {
        @bind_positional
        fn ENDS_WITH(A: positional Str, B: positional Str) {
            A.ends_with(B.as_ref())
        }
        
        @bind_content
        fn ESCAPE_HTML(STR: content Str) {
            compiler.escape_html_impl(STR.into())
        }
        
        fn FROM(PARAM: content Str) {
            PARAM
        }
        
        @bind_content
        fn IS_EMPTY(STR: content Str) {
            STR.is_empty()
        }
        
        @bind_content
        fn IS_WHITESPACE(STR: content Str) {
            text::is_whitespace(STR.as_ref())
        }
        
        @bind_content
        fn LEN(STR: content Str) {
            STR.len() as Int
        }
        
        @bind_content
        fn NODES(STR: content Str) {
            HTML::from(STR).nodes()
        }
        
        @bind_positional
        fn STARTS_WITH(A: positional Str, B: positional Str) {
            A.starts_with(B.as_ref())
        }
        
        @bind_content
        fn TRIM(STR: content Str) {
            STR.trim()
        }
        
        @bind_content
        fn UNIQUE_ID(MAX_LENGTH: named Int = 128, BASE: content Str) {
            if MAX_LENGTH <= 0 {
                let e = RuntimeError::ParamMustBePositive(compiler.get_name(str_ids::MAX_LENGTH).to_string(), MAX_LENGTH);
                compiler.runtime_error(e, call_range);
                return None;
            }
            
            compiler.ctx.unique_ids.get_unique_id(BASE.as_ref(), MAX_LENGTH as usize)
        }
    }
    
    impl REGEX for Regex => {
        fn COMPILE(SOURCE: content Str) {
            compiler.compile_regex(SOURCE.as_ref())
                .map_err(|e| compiler.runtime_error(e, call_range))
                .ok()?
        }
        
        @bind_positional
        fn COUNT(REGEX: positional Regex, STR: content Str) {
            REGEX.count(STR.as_ref()) as Int
        }
        
        @bind_positional
        fn FIND(REGEX: positional Regex, STR: content Str) {
            REGEX.find(STR.as_ref())
        }
        
        @bind_positional
        fn FIND_ALL(REGEX: positional Regex, STR: content Str) {
            REGEX.find_all(STR.as_ref())
        }
        
        @bind_positional
        fn TEST(REGEX: positional Regex, STR: content Str) {
            REGEX.test(STR.as_ref())
        }
    }
    
    impl FUNCTION for Func => {
        @bind_positional
        fn BIND(FUNCTION: positional Func, ARGS: pos_spread List, KWARGS: named_spread Dict, PARAM: content Value) {
            FUNCTION.bind_partial(compiler, ARGS.as_ref(), KWARGS.as_ref(), PARAM, call_range)?
        }
        
        @bind_content
        fn NAME(FUNCTION: content Func) {
            compiler.get_name(FUNCTION.name_id())
        }
    }
    
    impl HTML for HTML => {
        @bind_content
        fn ESCAPE_HTML(HTML: content HTML) {
            compiler.escape_html_impl(HTML)
        }
        
        @bind_content
        fn IS_EMPTY(HTML: content HTML) {
            HTML.is_empty()
        }
        
        @bind_content
        fn IS_WHITESPACE(HTML: content HTML) {
            HTML.is_whitespace()
        }
        
        @bind_content
        fn NODES(HTML: content HTML) {
            HTML.nodes()
        }
    }
    
    impl LIST for List => {
        @bind_content
        fn ALL(FUNCTION: positional Func, LIST: content List) {
            for v in LIST.as_ref() {
                if !compiler.eval_callback::<bool>(FUNCTION.clone(), v.clone(), call_range)? {
                    return Some(false.into());
                }
            }
            true
        }
        
        @bind_content
        fn ANY(FUNCTION: positional Func, LIST: content List) {
            for v in LIST.as_ref() {
                if compiler.eval_callback::<bool>(FUNCTION.clone(), v.clone(), call_range)? {
                    return Some(true.into());
                }
            }
            false
        }
        
        @bind_content
        fn CONTAINS(PARAM: positional Value, LIST: content List) {
            LIST.as_ref()
                .contains(&PARAM)
        }
        
        @bind_content
        fn FILTER(FUNCTION: positional Option<Func> = (), LIST: content List) {
            if let Some(f) = FUNCTION {
                let mut out = Vec::new();
                for v in LIST.as_ref() {
                    if compiler.eval_callback::<bool>(f.clone(), v.clone(), call_range)? {
                        out.push(v.clone());
                    }
                }
                out
            } else {
                LIST.as_ref()
                    .iter()
                    .filter(|&v| !v.is_unit())
                    .cloned()
                    .collect()
            }
        }
        
        @bind_content
        fn FIND(FUNCTION: positional Func, LIST: content List) {
            for v in LIST.as_ref() {
                if compiler.eval_callback::<bool>(FUNCTION.clone(), v.clone(), call_range)? {
                    return Some(v.clone());
                }
            }
            ()
        }
        
        @bind_content
        fn FLAT(LIST: content List) {
            let mut out = Vec::new();
            for child in LIST.as_ref() {
                if let Value::List(children) = child {
                    out.extend(children.as_ref().iter().cloned());
                } else {
                    out.push(child.clone());
                }
            }
            out
        }
        
        @bind_content
        fn IS_EMPTY(LIST: content List) {
            LIST.is_empty()
        }
        
        @bind_content
        fn JOIN(SEP: positional HTML = (), LIST: content Vec<HTML>) {
            if LIST.is_empty() {
                HTML::Empty
            } else if SEP.is_empty() {
                HTML::seq(LIST)
            } else {
                let mut out = Vec::with_capacity(2 * LIST.len() - 1);
                for (i, v) in LIST.into_iter().enumerate() {
                    if i > 0 { out.push(SEP.clone()); }
                    out.push(v);
                }
                HTML::seq(out)
            }
        }
        
        @bind_content
        fn LEN(LIST: content List) {
            LIST.len() as Int
        }
        
        @bind_content
        fn MAP(FUNCTION: positional Func, LIST: content List) {
            let mut out = Vec::new();
            for v in LIST.as_ref() {
                let r: Value = compiler.eval_callback(FUNCTION.clone(), v.clone(), call_range)?;
                out.push(r);
            }
            out
        }
        
        @bind_content
        fn SLICE(A: positional Int, B: positional Option<Int> = (), LIST: content List) {
            let n = LIST.len() as Int;
            let start = A;
            let end = B.unwrap_or(n);
            let a = if start < 0 { 0.max(start + n) } else { start.min(n) };
            let b = if end < 0 { 0.max(end + n) } else { end.min(n) };
            
            LIST.slice(a.min(b) as usize, b as usize)
        }
        
        @bind_content
        fn SORTED(KEY: named Option<Func> = (), REVERSED: named bool = false, LIST: content List) {
            if LIST.is_empty() {
                return Some(LIST.into());
            }
            
            let mut decorated = Vec::with_capacity(LIST.len());
            let mut key_type = Type::Unit;
            for v in LIST.as_ref().iter().cloned() {
                let k = KEY.as_ref()
                    .map_or_else(
                        || Some(v.clone()),
                        |f| compiler.eval_callback::<Value>(f.clone(), v.clone(), call_range),
                    )?;
                
                if !matches!(k, Value::Int(_) | Value::Str(_)) {
                    compiler.type_error(TypeError::SortKeyInvalid(k.get_type()), call_range);
                    return None;
                } else if matches!((&key_type, &k), (Type::Int, Value::Str(_)) | (Type::Str, Value::Int(_))) {
                    compiler.type_error(TypeError::SortKeyHeterogeneous, call_range);
                    return None;
                } else if matches!(key_type, Type::Unit) {
                    key_type = k.get_type();
                }
                
                decorated.push((k, v));
            }
            
            // gives correct stability behaviour
            if REVERSED { decorated.reverse(); }
            
            match decorated.first().unwrap().0 {
                Value::Int(_) => decorated.sort_by_key(|p| p.0.clone().expect_convert::<Int>()),
                Value::Str(_) => decorated.sort_by_key(|p| p.0.clone().expect_convert::<Str>()),
                _ => compiler.ice_at("failed to unwrap sort key", call_range),
            }
            
            let mut undecorated: Vec<Value> = decorated.into_iter()
                .map(|p| p.1)
                .collect();
            if REVERSED { undecorated.reverse(); }
            undecorated
        }
        
        fn REVERSED(LIST: content List) {
            let mut vs = LIST.as_ref().to_vec();
            vs.reverse();
            vs
        }
    }
    
    impl DICT {
        fn IS_EMPTY(DICT: content Dict) {
            DICT.is_empty()
        }
        
        fn ITEMS(DICT: content Dict) {
            DICT.iter()
                .map(|(&k, v)| [compiler.get_name(k).into(), v.clone()].into())
                .collect::<Vec<Value>>()
        }
        
        fn KEYS(DICT: content Dict) {
            DICT.keys()
                .copied()
                .map(|k| compiler.get_name(k).into())
                .collect::<Vec<Value>>()
        }
        
        fn LEN(DICT: content Dict) {
            DICT.len() as Int
        }
        
        fn NEW(KWARGS: named_spread Dict) {
            KWARGS
        }
        
        fn VALUES(DICT: content Dict) {
            DICT.values()
                .cloned()
                .collect::<Vec<Value>>()
        }
    }
    
    impl FILE {
        fn LIST(PATH: content Str) {
            let path = compiler.resolve_relative_path(call_range.src_id, PATH.as_ref(), false);
            let base_path = PATH.trim_end_matches('/');
            relpath::find_papyri_source_files_in_dir(
                    &path,
                    |p, e| compiler.module_error(p, ModuleError::IOError(e), call_range),
                )?
                .into_iter()
                .map(|p| format!("{base_path}/{}", p.to_string_lossy())
                    .strip_suffix(".papyri")
                    .unwrap_or_else(|| compiler.ice_at("Failed to strip .papyri extension", call_range))
                    .into()
                )
                .collect::<Vec<Value>>()
        }
        
        fn WRITE(PATH: positional Str, HTML: content HTML) {
            compiler.ctx.push_out_file(PATH, HTML)
                .map_err(|e| compiler.runtime_error(e, call_range))
                .ok()?;
        }
    }
    
    fn CODE(LANGUAGE: implicit Option<Str> = (), CODE_BLOCK: named bool = false, FIRST_LINE_NO: named Int = 1, SOURCE: content Str) {
        let with_hint = CODE_BLOCK.then(|| text::get_source_language_hint(SOURCE.as_ref()))
            .flatten();
        
        let (language, src) = if let Some((l, s)) = with_hint {
            (Some(l), s)
        } else {
            (LANGUAGE.as_ref().map(|l| l.as_ref()), SOURCE.as_ref())
        };
        
        compiler.native_code_impl(language, CODE_BLOCK, FIRST_LINE_NO, src, call_range)?
    }
    
    fn IMPORT(PATH: content Str) {
        let path = compiler.resolve_relative_path(call_range.src_id, PATH.as_ref(), true);
        let (_, module_exports) = compiler.ctx.load_cached(&path)
            .map_err(|e| compiler.module_error(&path, e, call_range))
            .ok()?;
        
        module_exports
    }
    
    fn INCLUDE(PATH: content Str) {
        let path = compiler.resolve_relative_path(call_range.src_id, PATH.as_ref(), true);
        let (module_out, module_exports) = compiler.ctx.load_cached(&path)
            .map_err(|e| compiler.module_error(&path, e, call_range))
            .ok()?;
        
        for (&k, v) in module_exports.as_ref().iter() {
            compiler.set_var(k, v.clone(), false, call_range);
        }
        module_out
    }
    
    fn RAISE(STR: content Str) {
        compiler.runtime_error(RuntimeError::Raised(STR), call_range);
        return None;
    }
}

impl <'a> Compiler<'a> {
    fn native_code_impl(&mut self, language: Option<&str>, is_block: bool, first_line_no: i64, src: &str, call_range: SourceRange) -> Option<Tag> {
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
        
        Some(tag)
    }
    
    fn escape_html_impl(&mut self, h: HTML) -> String {
        let mut s = Vec::new();
        self.ctx.render(&h, true, &mut s).unwrap();
        String::from_utf8(s).unwrap()
    }
    
    fn resolve_relative_path(&mut self, src_id: SourceFileID, relative_path: &str, add_papyri_suffix: bool) -> std::path::PathBuf {
        let mut path = self.ctx.source_files
            .get(src_id)
            .path
            .to_path_buf();
        
        path.pop();
        if add_papyri_suffix && !relative_path.ends_with(".papyri") {
            path.push(format!("{relative_path}.papyri"));
        } else {
            path.push(relative_path);
        }
        path
    }
    
    fn eval_callback<T: TryConvert>(&mut self, callback: Func, arg: Value, call_range: SourceRange) -> Option<T> {
        let bindings = callback.bind_synthetic_call(self, false, arg, call_range)?;
        self.evaluate_func_call_with_bindings(
            callback,
            bindings,
            &T::as_type(),
            call_range,
        ).map(Value::expect_convert)
    }
}
