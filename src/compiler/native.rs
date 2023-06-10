use reqwest::blocking::Client;
use crate::compiler::html;
use crate::errors;
use crate::utils::{str_ids, text, relpath};
use crate::utils::sourcefile::{SourceRange, SourceFileID};
use crate::parser::Type;
use super::base::Compiler;
use super::func::Func;
use super::html::HTML;
use super::regex_value::RcRegex;
use super::tag::Tag;
use super::value::{Value, Int, RcStr, List, RcDict};
use super::value_convert::TryConvert;

crate::native_defs! {
    let compiler, call_range;
    
    impl BOOL for Bool {
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
    
    impl INT for Int {
        @bind_positional
        fn ADD(ARGS: pos_spread Vec<Int>) {
            ARGS.into_iter().sum::<Int>()
        }
        
        @bind_content
        fn NEGATE(PARAM: content Int) {
            -PARAM
        }
        
        fn PARSE(PARAM: content RcStr) {
            PARAM.parse::<Int>()
                .map_err(errors::RuntimeError::ParseIntError)?
        }
    }
    
    impl STR for Str {
        @bind_positional
        fn ENDS_WITH(A: positional RcStr, B: positional RcStr) {
            A.ends_with(B.as_ref())
        }
        
        @bind_content
        fn ESCAPE_HTML(STR: content RcStr) {
            compiler.escape_html_impl(STR.into())
        }
        
        fn FROM(PARAM: content RcStr) {
            PARAM
        }
        
        @bind_content
        fn IS_EMPTY(STR: content RcStr) {
            STR.is_empty()
        }
        
        @bind_content
        fn IS_WHITESPACE(STR: content RcStr) {
            text::is_whitespace(STR.as_ref())
        }
        
        @bind_content
        fn LEN(STR: content RcStr) {
            STR.len() as Int
        }
        
        @bind_content
        fn LOWER(STR: content RcStr) {
            STR.to_lowercase()
        }
        
        @bind_content
        fn SPLIT(SEP: positional RcStr, STR: content RcStr) {
            STR.as_ref()
                .split(SEP.as_ref())
                .map(Value::from)
                .collect::<Vec<Value>>()
        }
        
        @bind_positional
        fn STARTS_WITH(A: positional RcStr, B: positional RcStr) {
            A.starts_with(B.as_ref())
        }
        
        @bind_content
        fn TRIM(STR: content RcStr) {
            STR.trim()
        }
        
        @bind_content
        fn UNIQUE_ID(MAX_LENGTH: named Int = 128, BASE: content RcStr) {
            if MAX_LENGTH <= 0 {
                let name = compiler.get_name(str_ids::MAX_LENGTH);
                let e = errors::RuntimeError::ParamMustBePositive(name, MAX_LENGTH);
                return Err(e.into());
            }
            
            compiler.ctx.unique_ids.get_unique_id(BASE.as_ref(), MAX_LENGTH as usize)
        }
        
        @bind_content
        fn UPPER(STR: content RcStr) {
            STR.to_uppercase()
        }
    }
    
    impl REGEX for Regex {
        fn COMPILE(SOURCE: content RcStr) {
            compiler.compile_regex(SOURCE.as_ref())?
        }
        
        @bind_positional
        fn COUNT(REGEX: positional RcRegex, STR: content RcStr) {
            REGEX.count(STR.as_ref()) as Int
        }
        
        @bind_positional
        fn FIND(REGEX: positional RcRegex, STR: content RcStr) {
            REGEX.find(STR.as_ref())
        }
        
        @bind_positional
        fn FIND_ALL(REGEX: positional RcRegex, STR: content RcStr) {
            REGEX.find_all(STR.as_ref())
        }
        
        @bind_positional
        fn SPLIT(REGEX: positional RcRegex, STR: content RcStr) {
            REGEX.split(STR.as_ref())
        }
        
        @bind_positional
        fn TEST(REGEX: positional RcRegex, STR: content RcStr) {
            REGEX.test(STR.as_ref())
        }
    }
    
    impl FUNCTION for Func {
        @bind_positional
        fn BIND(FUNCTION: positional Func, ARGS: pos_spread List, KWARGS: named_spread RcDict, PARAM: content Value) {
            FUNCTION.bind_partial(compiler, ARGS.as_ref(), KWARGS.as_ref(), PARAM)?
        }
        
        @bind_content
        fn NAME(FUNCTION: content Func) {
            compiler.get_name(FUNCTION.name_id())
        }
    }
    
    impl HTML for HTML {
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
    }
    
    impl LIST for List {
        @bind_content
        fn ALL(FUNCTION: positional Func, LIST: content List) {
            for v in LIST.as_ref() {
                if !compiler.eval_callback::<bool>(FUNCTION.clone(), v.clone(), call_range)? {
                    return Ok(false.into());
                }
            }
            true
        }
        
        @bind_content
        fn ANY(FUNCTION: positional Func, LIST: content List) {
            for v in LIST.as_ref() {
                if compiler.eval_callback::<bool>(FUNCTION.clone(), v.clone(), call_range)? {
                    return Ok(true.into());
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
        fn ENUMERATE(FROM: named Int = 0, LIST: content List) {
            LIST.as_ref()
                .iter()
                .cloned()
                .enumerate()
                .map(|(i, v)| [Value::Int(i as Int + FROM), v].into())
                .collect::<Vec<Value>>()
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
                    return Ok(v.clone());
                }
            }
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
        fn GET(I: positional Int, LIST: content List) {
            compiler.list_get(LIST, I)?
        }
        
        fn HTML_NODES(HTML: content HTML) {
            HTML.nodes()
                .iter()
                .map(Value::from)
                .collect::<Vec<_>>()
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
                HTML::from_iter(LIST)
            } else {
                let mut out = Vec::with_capacity(2 * LIST.len() - 1);
                for (i, v) in LIST.into_iter().enumerate() {
                    if i > 0 { out.push(SEP.clone()); }
                    out.push(v);
                }
                HTML::from_iter(out)
            }
        }
        
        @bind_content
        fn LEN(LIST: content List) {
            LIST.len() as Int
        }
        
        @bind_content
        fn MAP(FUNCTION: positional Func, LIST: content List) {
            let mut out = Vec::with_capacity(LIST.len());
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
            compiler.native_sorted_impl(KEY, REVERSED, LIST, call_range)?
        }
        
        fn REVERSED(LIST: content List) {
            let mut vs = LIST.as_ref().to_vec();
            vs.reverse();
            vs
        }
    }
    
    impl DICT {
        fn GET(KEY: positional RcStr, DICT: content RcDict) {
            let Some(v) = compiler.ctx.string_pool.get_id_if_present(KEY.as_ref())
                .and_then(|key_id| DICT.get(&key_id)) else {
                    let e = errors::NameError::NoSuchAttribute(Type::Any.dict(), KEY);
                    return Err(e.into());
                };
            v.clone()
        }
        
        fn IS_EMPTY(DICT: content RcDict) {
            DICT.is_empty()
        }
        
        fn ITEMS(DICT: content RcDict) {
            DICT.iter()
                .map(|(&k, v)| [compiler.get_name(k).into(), v.clone()].into())
                .collect::<Vec<Value>>()
        }
        
        fn KEYS(DICT: content RcDict) {
            DICT.keys()
                .copied()
                .map(|k| compiler.get_name(k).into())
                .collect::<Vec<Value>>()
        }
        
        fn LEN(DICT: content RcDict) {
            DICT.len() as Int
        }
        
        fn NEW(KWARGS: named_spread RcDict) {
            KWARGS
        }
        
        fn VALUES(DICT: content RcDict) {
            DICT.values()
                .cloned()
                .collect::<Vec<Value>>()
        }
    }
    
    impl FILE {
        fn LIST(PATH: content RcStr) {
            let path = compiler.resolve_relative_path(call_range.src_id, PATH.as_ref(), false);
            let base_path = PATH.trim_end_matches('/');
            relpath::find_papyri_source_files_in_dir(
                    &path,
                    |p, e| { compiler.report_static(errors::ModuleError::IOError(p.into(), e), call_range); },
                )?
                .into_iter()
                .map(|p| format!("{base_path}/{}", p.to_string_lossy())
                    .strip_suffix(".papyri")
                    .unwrap_or_else(|| compiler.ice_at("Failed to strip .papyri extension", call_range))
                    .into()
                )
                .collect::<Vec<Value>>()
        }
        
        fn READ(PATH: content RcStr) {
            let path = compiler.resolve_relative_path(call_range.src_id, PATH.as_ref(), false);
            std::fs::read_to_string(path)
                .map_err(|e| errors::RuntimeError::FileReadError(PATH, e))?
        }
        
        fn WRITE(PATH: positional RcStr, HTML: content HTML) {
            compiler.ctx.push_out_file(PATH, HTML)?
        }
    }

    impl FETCH {
        fn RAW(PATH: content RcStr) {
            Client::builder()
                .user_agent("Mozilla/5.0 (compatible) Papyri")
                .build()
                .map_err(|e| errors::RuntimeError::NetworkError(e))?
                .get(PATH.as_ref())
                .send()
                .map_err(|e| errors::RuntimeError::NetworkError(e))?
                .text()
                .map_err(|e| errors::RuntimeError::NetworkError(e))?
        }

        fn HTML(PATH: content RcStr) {
            let text = Client::builder()
                .user_agent("Mozilla/5.0 (compatible) Papyri")
                .build()
                .map_err(|e| errors::RuntimeError::NetworkError(e))?
                .get(PATH.as_ref())
                .send()
                .map_err(|e| errors::RuntimeError::NetworkError(e))?
                .text()
                .map_err(|e| errors::RuntimeError::NetworkError(e))?;
            html::parse_html(&text, compiler.string_pool_mut())?
        }
    }
    
    fn CODE(LANGUAGE: implicit Option<RcStr> = (), CODE_BLOCK: named bool = false, FIRST_LINE_NO: named Int = 1, SOURCE: content RcStr) {
        compiler.native_code_impl(LANGUAGE, CODE_BLOCK, FIRST_LINE_NO, SOURCE.as_ref(), call_range)
    }
    
    fn IMPORT(PATH: content RcStr) {
        let path = compiler.resolve_relative_path(call_range.src_id, PATH.as_ref(), true);
        let (_, module_exports) = compiler.ctx.load_cached(path)?;
        module_exports
    }
    
    fn INCLUDE(PATH: content RcStr) {
        let path = compiler.resolve_relative_path(call_range.src_id, PATH.as_ref(), true);
        let (module_out, module_exports) = compiler.ctx.load_cached(path)?;
        
        for (&k, v) in module_exports.as_ref().iter() {
            compiler.set_var(k, v.clone(), false, call_range);
        }
        module_out
    }
    
    fn RAISE(STR: content RcStr) {
        let e = errors::RuntimeError::Raised(STR);
        return Err(e.into());
    }
}

impl <'a> Compiler<'a> {
    fn native_sorted_impl(&mut self, key_func: Option<Func>, reversed: bool, list: List, call_range: SourceRange) -> errors::PapyriResult<List> {
        if list.is_empty() {
            return Ok(list);
        }
        
        let mut decorated = Vec::with_capacity(list.len());
        let mut key_type = Type::Unit;
        for v in list.as_ref().iter().cloned() {
            let k = key_func.as_ref()
                .map_or_else(
                    || Ok(v.clone()),
                    |f| self.eval_callback::<Value>(f.clone(), v.clone(), call_range),
                )?;
            
            if !matches!(k, Value::Int(_) | Value::Str(_)) {
                let e = errors::TypeError::SortKeyInvalid(k.get_type());
                return Err(e.into());
            } else if matches!((&key_type, &k), (Type::Int, Value::Str(_)) | (Type::Str, Value::Int(_))) {
                let e = errors::TypeError::SortKeyHeterogeneous;
                return Err(e.into());
            } else if matches!(key_type, Type::Unit) {
                key_type = k.get_type();
            }
            
            decorated.push((k, v));
        }
        
        // gives correct stability behaviour
        if reversed { decorated.reverse(); }
        
        match decorated.first().unwrap().0 {
            Value::Int(_) => decorated.sort_by_key(|p| p.0.clone().expect_convert::<Int>()),
            Value::Str(_) => decorated.sort_by_key(|p| p.0.clone().expect_convert::<RcStr>()),
            _ => self.ice_at("failed to unwrap sort key", call_range),
        }
        
        let mut undecorated: Vec<Value> = decorated.into_iter()
            .map(|p| p.1)
            .collect();
        if reversed { undecorated.reverse(); }
        
        Ok(undecorated.into())
    }
    
    fn native_code_impl(&mut self, language: Option<RcStr>, is_block: bool, first_line_no: Int, src: &str, call_range: SourceRange) -> Tag {
        use super::highlight::{syntax_highlight, enumerate_lines};
        
        let default = language.as_ref()
            .map(|s| s.as_ref())
            .unwrap_or("none");
        let (language, src) = if is_block {
            text::get_source_language_hint(src, default)
        } else {
            (default, src)
        };
        
        let mut tag = Tag::new(str_ids::CODE, HTML::Empty);
        
        let src = text::fix_indentation(src);
        let (lines, warning) = syntax_highlight(&src, language);
        if let Some(warning) = warning {
            self.report(warning, call_range);
        } else if language != "none" {
            tag.attributes.insert(str_ids::CLASS, Some(format!("syntax-highlight lang-{language}").into()));
        }
        
        tag.content = if is_block {
            enumerate_lines(lines, first_line_no)
        } else {
            if first_line_no != 1 {
                self.report(errors::Warning::InlineHighlightEnumerate, call_range);
            }
            if lines.len() > 1 {
                self.report(errors::Warning::InlineHighlightMultiline, call_range);
            }
            HTML::from_iter(lines)
        };
        
        tag
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
    
    fn eval_callback<T: TryConvert>(&mut self, callback: Func, arg: Value, call_range: SourceRange) -> errors::PapyriResult<T> {
        let bindings = callback.bind_synthetic_call(self, false, arg)?;
        let v = self.evaluate_func_call_with_bindings(
            callback,
            bindings,
            &T::as_type(),
            call_range,
        )?;
        Ok(v.expect_convert())
    }
}
