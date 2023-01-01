use std::rc::Rc;

use crate::errors::{RuntimeError, RuntimeWarning, ModuleError, TypeError};
use crate::utils::{str_ids, text, NameID, relpath, SliceRef};
use crate::utils::sourcefile::{SourceRange, SourceFileID};
use crate::parser::Type;
use super::base::Compiler;
use super::frame::ActiveFrame;
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
    
    ADD(_0: pos_spread Vec<Int>) {
        _0.into_iter().sum::<Int>()
    }
    
    BIND(_0: positional Func, _1: pos_spread List, KWARGS: named_spread Dict, PARAM: content Value) {
        _0.bind_partial(compiler, _1.as_ref(), KWARGS.as_ref(), PARAM, call_range)?
    }
    
    CODE(LANGUAGE: implicit Option<Str> = (), CODE_BLOCK: named bool = false, FIRST_LINE_NO: named Int = 1, PARAM: content Str) {
        let with_hint = CODE_BLOCK.then(|| text::get_source_language_hint(PARAM.as_ref()))
            .flatten();
        
        let (language, src) = if let Some((l, s)) = with_hint {
            (Some(l), s)
        } else {
            (LANGUAGE.as_ref().map(|l| l.as_ref()), PARAM.as_ref())
        };
        
        compiler.native_code_impl(language, CODE_BLOCK, FIRST_LINE_NO, src, call_range)?
    }
    
    ESCAPE_HTML(PARAM: content HTML) {
        let mut s = Vec::new();
        compiler.ctx.render(&PARAM, true, &mut s).unwrap();
        String::from_utf8(s).unwrap()
    }
    
    FILTER(_0: positional Option<Func> = (), PARAM: content List) {
        if let Some(f) = _0 {
            let mut out = Vec::new();
            for v in PARAM.as_ref() {
                let r = compiler.eval_callback(f.clone(), v.clone(), &Type::Bool, call_range)?;
                if r.expect_convert::<bool>() {
                    out.push(v.clone())
                }
            }
            out
        } else {
            PARAM.as_ref()
                .iter()
                .filter(|v| !v.is_unit())
                .cloned()
                .collect()
        }
    }
    
    IMPORT(PARAM: content Str) {
        let path = compiler.resolve_relative_path(call_range.src_id, PARAM.as_ref(), true);
        let (_, module_exports) = compiler.ctx.load_cached(&path)
            .map_err(|e| compiler.module_error(&path, e, call_range))
            .ok()?;
        
        module_exports
    }
    
    INCLUDE(PARAM: content Str) {
        let path = compiler.resolve_relative_path(call_range.src_id, PARAM.as_ref(), true);
        let (module_out, module_exports) = compiler.ctx.load_cached(&path)
            .map_err(|e| compiler.module_error(&path, e, call_range))
            .ok()?;
        
        for (&k, v) in module_exports.as_ref().iter() {
            compiler.set_var(k, v.clone(), false, call_range);
        }
        module_out
    }
    
    JOIN(_0: positional HTML = (), PARAM: content Vec<HTML>) {
        if PARAM.is_empty() {
            HTML::Empty
        } else if _0.is_empty() {
            HTML::seq(PARAM)
        } else {
            let mut out = Vec::with_capacity(2 * PARAM.len() - 1);
            for (i, v) in PARAM.into_iter().enumerate() {
                if i > 0 { out.push(_0.clone()); }
                out.push(v);
            }
            HTML::seq(out)
        }
    }
    
    LIST_FILES(PARAM: content Str) {
        let path = compiler.resolve_relative_path(call_range.src_id, PARAM.as_ref(), false);
        let base_path = PARAM.trim_end_matches('/');
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
            .collect::<Vec<_>>()
    }
    
    MAP(_0: positional Func, PARAM: content List) {
        let mut out = Vec::new();
        for v in PARAM.as_ref() {
            let r = compiler.eval_callback(_0.clone(), v.clone(), &Type::Any, call_range)?;
            out.push(r);
        }
        out
    }
    
    RAISE(PARAM: content Str) {
        compiler.runtime_error(RuntimeError::Raised(PARAM), call_range);
        return None;
    }
    
    REGEX_COMPILE(PARAM: content Str) {
        compiler.compile_regex(PARAM.as_ref(), call_range)?
    }
    
    REGEX_FIND(_0: positional Regex, PARAM: content Str) {
        _0.find(PARAM.as_ref())
    }
    
    REGEX_FIND_ALL(_0: positional Regex, PARAM: content Str) {
        _0.find_all(PARAM.as_ref())
    }
    
    SLICE(_0: positional Int, _1: positional Option<Int> = (), PARAM: content List) {
        let n = PARAM.len() as Int;
        let start = _0;
        let end = _1.unwrap_or(n);
        let a = if start < 0 { 0.max(start + n) } else { start.min(n) };
        let b = if end < 0 { 0.max(end + n) } else { end.min(n) };
        
        PARAM.slice(a.min(b) as usize, b as usize)
    }
    
    SORTED(KEY: named Option<Func> = (), REVERSED: named bool = false, PARAM: content List) {
        if PARAM.is_empty() {
            return Some(PARAM.into());
        }
        
        let mut decorated = Vec::with_capacity(PARAM.len());
        let mut key_type = Type::Unit;
        for v in PARAM.as_ref().iter().cloned() {
            let k = KEY.as_ref()
                .map_or_else(
                    || Some(v.clone()),
                    |f| compiler.eval_callback(f.clone(), v.clone(), &Type::Any, call_range),
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
    
    UNIQUE_ID(MAX_LENGTH: named Int = 128, PARAM: content Str) {
        if MAX_LENGTH <= 0 {
            let e = RuntimeError::ParamMustBePositive(compiler.get_name(str_ids::MAX_LENGTH).to_string(), MAX_LENGTH);
            compiler.runtime_error(e, call_range);
            return None;
        }
        
        compiler.ctx.unique_ids.get_unique_id(PARAM.as_ref(), MAX_LENGTH as usize)
    }
    
    WRITE_FILE(_0: positional Str, PARAM: content HTML) {
        compiler.ctx.push_out_file(_0, PARAM)
            .map_err(|e| compiler.runtime_error(e, call_range))
            .ok()?;
    }
}

impl NativeDefs {
    pub(super) fn to_frame(&self) -> ActiveFrame {
        impl Func {
            fn entry(&self) -> (NameID, Value) {
                (self.name_id(), self.clone().into())
            }
            fn named_entry(&self, name_id: NameID) -> (NameID, Value) {
                (name_id, self.clone().into())
            }
        }
        
        fn dict_entry<const N: usize>(name_id: NameID, entries: [(NameID, Value); N]) -> (NameID, Value) {
            (name_id, ValueMap::from_iter(entries).into())
        }
        
        let bindings = ValueMap::from_iter([
            dict_entry(str_ids::INT, [
                self.ADD.entry(),
            ]),
            
            dict_entry(str_ids::STR, [
                self.UNIQUE_ID.entry(),
            ]),
            
            dict_entry(str_ids::HTML, [
                self.ESCAPE_HTML.entry(),
            ]),
            
            dict_entry(str_ids::LIST, [
                self.FILTER.entry(),
                self.JOIN.entry(),
                self.MAP.entry(),
                self.SLICE.entry(),
                self.SORTED.entry(),
            ]),
            
            dict_entry(str_ids::FUNCTION, [
                self.BIND.entry(),
            ]),
            
            dict_entry(str_ids::REGEX, [
                self.REGEX_COMPILE.named_entry(str_ids::COMPILE),
                self.REGEX_FIND.named_entry(str_ids::FIND),
                self.REGEX_FIND_ALL.named_entry(str_ids::FIND_ALL),
            ]),
            
            self.CODE.entry(),
            self.IMPORT.entry(),
            self.INCLUDE.entry(),
            self.LIST_FILES.entry(),
            self.RAISE.entry(),
            self.WRITE_FILE.entry(),
        ]);
        
        ActiveFrame::new(None, bindings, None)
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
    
    fn eval_callback(&mut self, callback: Func, arg: Value, expected_type: &Type, call_range: SourceRange) -> Option<Value> {
        let bindings = callback.bind_synthetic_call(self, false, arg, call_range)?;
        self.evaluate_func_call_with_bindings(
            callback,
            bindings,
            expected_type,
            call_range,
        )
    }
}
