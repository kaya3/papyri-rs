use std::rc::Rc;
use indexmap::IndexMap;

use crate::errors::{ice_at, RuntimeError, Warning, RuntimeWarning, ModuleError};
use crate::utils::{str_ids, text, NameID, SourceRange, relpath};
use super::frame::ActiveFrame;
use super::func::{FuncSignature, FuncParam, Func};
use super::highlight::{syntax_highlight, enumerate_lines};
use super::html::HTML;
use super::compiler::Compiler;
use super::types::Type;
use super::value::{Value, ValueMap};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NativeFunc {
    Export,
    FixIndentation,
    Implicit,
    Import,
    Include,
    Let,
    ListFiles,
    Map,
    Raise,
    SyntaxHighlight,
    WriteFile,
}

pub fn get_natives_frame() -> ActiveFrame {
    let args_dict = Rc::new(FuncSignature {
        positional_params: Box::new([]),
        spread_param: None,
        named_params: IndexMap::new(),
        spread_named_param: Some(FuncParam::new(str_ids::ARGS, Type::dict(Type::AnyValue))),
        content_param: FuncParam::new(str_ids::CONTENT, Type::Unit),
    });
    
    let content_str = Rc::new(FuncSignature {
        positional_params: Box::new([]),
        spread_param: None,
        named_params: IndexMap::new(),
        spread_named_param: None,
        content_param: FuncParam::new(str_ids::CONTENT, Type::Str),
    });
    
    let map = Rc::new(FuncSignature {
        positional_params: Box::new([
            FuncParam::new(str_ids::_0, Type::Function),
        ]),
        spread_param: None,
        named_params: IndexMap::new(),
        spread_named_param: None,
        content_param: FuncParam::new(str_ids::CONTENT, Type::list(Type::AnyValue)),
    });
    
    let syntax_highlight = Rc::new(FuncSignature {
        positional_params: Box::new([]),
        spread_param: None,
        named_params: IndexMap::from([
            (str_ids::LANGUAGE, FuncParam::new(str_ids::LANGUAGE, Type::optional(Type::Str)).implicit().with_default(Value::UNIT)),
            (str_ids::CODE_BLOCK, FuncParam::new(str_ids::CODE_BLOCK, Type::Bool).with_default(Value::Bool(false))),
            (str_ids::FIRST_LINE_NO, FuncParam::new(str_ids::FIRST_LINE_NO, Type::optional(Type::Int)).with_default(Value::UNIT)),
        ]),
        spread_named_param: None,
        content_param: FuncParam::new(str_ids::CONTENT, Type::Str),
    });
    
    let write_file = Rc::new(FuncSignature {
        positional_params: Box::new([
            FuncParam::new(str_ids::_0, Type::Str),
        ]),
        spread_param: None,
        named_params: IndexMap::new(),
        spread_named_param: None,
        content_param: FuncParam::new(str_ids::CONTENT, Type::AnyHTML),
    });
    
    let bindings: ValueMap = [
        (NativeFunc::Export, args_dict.clone()),
        (NativeFunc::FixIndentation, content_str.clone()),
        (NativeFunc::Implicit, args_dict.clone()),
        (NativeFunc::Import, content_str.clone()),
        (NativeFunc::Include, content_str.clone()),
        (NativeFunc::Let, args_dict),
        (NativeFunc::ListFiles, content_str.clone()),
        (NativeFunc::Map, map),
        (NativeFunc::Raise, content_str),
        (NativeFunc::SyntaxHighlight, syntax_highlight),
        (NativeFunc::WriteFile, write_file),
    ].into_iter().map(|(f, sig)| {
        (f.name_id(), Value::Func(Func::Native(f, sig)))
    }).collect();
    
    ActiveFrame::new(None, bindings, None)
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
            NativeFunc::ListFiles => str_ids::LIST_FILES,
            NativeFunc::Map => str_ids::MAP,
            NativeFunc::Raise => str_ids::RAISE,
            NativeFunc::SyntaxHighlight => str_ids::SYNTAX_HIGHLIGHT,
            NativeFunc::WriteFile => str_ids::WRITE_FILE,
        }
    }
}

impl <'a> Compiler<'a> {
    pub fn evaluate_native_func(&mut self, f: NativeFunc, mut bindings: ValueMap, call_range: &SourceRange) -> Option<Value> {
        let bindings = &mut bindings;
        match f {
            NativeFunc::Export => {
                let Some(Value::Dict(args)) = take_val(bindings, str_ids::ARGS) else {
                    ice_at("failed to unpack", call_range);
                };
                
                for (&k, v) in args.iter() {
                    if self.exports.insert(k, v.clone()).is_some() {
                        let name = self.get_name(k).to_string();
                        self.diagnostics.warning(Warning::NameAlreadyExported(name), call_range);
                    }
                }
            },
            
            NativeFunc::FixIndentation => {
                let Some(Value::Str(content)) = take_val(bindings, str_ids::CONTENT) else {
                    ice_at("failed to unpack", call_range);
                };
                return Some(text::fix_indentation(content.as_ref()).into());
            },
            
            NativeFunc::Import | NativeFunc::Include | NativeFunc::ListFiles => {
                let Some(Value::Str(path_str)) = take_val(bindings, str_ids::CONTENT) else {
                    ice_at("failed to unpack", call_range);
                };
                
                // compute path relative to current source file
                let mut path = call_range.src.path.to_path_buf();
                path.pop();
                if f == NativeFunc::ListFiles || path_str.ends_with(".papyri") {
                    path.push(path_str.as_ref());
                } else {
                    path.push(format!("{}.papyri", path_str));
                }
                
                if f == NativeFunc::ListFiles {
                    let paths = relpath::find_papyri_source_files_in_dir(
                            &path,
                            |p, e| self.diagnostics.module_error(p, ModuleError::IOError(e), call_range),
                        )?
                        .into_iter()
                        .map(|p| Value::from(p.to_string_lossy()
                            .strip_suffix(".papyri")
                            .unwrap_or_else(|| ice_at("Failed to strip .papyri extension", call_range))
                        ))
                        .collect();
                    
                    return Some(Value::list(paths));
                }
                
                let (module_out, module_exports) = self.loader.load_cached(&path, self.diagnostics)
                    .map_err(|e| self.diagnostics.module_error(&path, e, call_range))
                    .ok()?;
                
                return Some(if f == NativeFunc::Import {
                    Value::Dict(module_exports)
                } else {
                    self.set_vars(module_exports.as_ref(), false, call_range);
                    Value::from(&module_out)
                });
            },
            
            NativeFunc::Implicit | NativeFunc::Let => {
                let Some(Value::Dict(args)) = take_val(bindings, str_ids::ARGS) else {
                    ice_at("failed to unpack", call_range);
                };
                self.set_vars(args.as_ref(), f == NativeFunc::Implicit, call_range);
            },
            
            NativeFunc::Map => {
                let (Some(Value::Func(callback)), Some(Value::List(content))) = (
                    take_val(bindings, str_ids::_0),
                    take_val(bindings, str_ids::CONTENT),
                ) else {
                    ice_at("failed to unpack", call_range);
                };
                
                let mut out = Vec::new();
                for v in content.as_ref() {
                    let bindings = callback.signature().bind_synthetic_call(self, false, v.clone(), call_range)?;
                    let r = self.evaluate_func_call_with_bindings(
                        callback.clone(),
                        bindings,
                        &Type::AnyValue,
                        call_range,
                    )?;
                    out.push(self.compile_value(r));
                }
                return Some(HTML::seq(out).into());
            },
            
            NativeFunc::Raise => {
                let Some(Value::Str(content)) = take_val(bindings, str_ids::CONTENT) else {
                    ice_at("failed to unpack", call_range);
                };
                
                self.runtime_error(RuntimeError::Raised(content), call_range);
                return None;
            },
            
            NativeFunc::SyntaxHighlight => {
                let (Some(language), Some(Value::Bool(is_block)), Some(first_line_no), Some(Value::Str(src))) = (
                    take_val(bindings, str_ids::LANGUAGE),
                    take_val(bindings, str_ids::CODE_BLOCK),
                    take_val(bindings, str_ids::FIRST_LINE_NO),
                    take_val(bindings, str_ids::CONTENT),
                ) else {
                    ice_at("failed to unpack", call_range);
                };
                
                let src = if is_block { src.as_ref() } else { src.trim() };
                
                let Value::Str(language) = language else {
                    return Some(HTML::text(src).into());
                };
                
                let first_line_no = if let Value::Int(i) = first_line_no {
                    if !is_block { self.runtime_warning(RuntimeWarning::InlineHighlightEnumerate, call_range); }
                    i
                } else { 1 };
                
                let r = syntax_highlight(src.as_ref(), language.as_ref());
                return Some(if is_block {
                    enumerate_lines(r, first_line_no)
                } else {
                    if r.len() > 1 { self.runtime_warning(RuntimeWarning::InlineHighlightMultiline, call_range); }
                    HTML::seq(r)
                }.into());
            },
            
            NativeFunc::WriteFile => {
                let (Some(Value::Str(path)), Some(Value::HTML(content))) = (
                    take_val(bindings, str_ids::_0),
                    take_val(bindings, str_ids::CONTENT),
                ) else {
                    ice_at("failed to unpack", call_range);
                };
                
                let Some(ref mut sink) = self.out_files else {
                    self.runtime_error(RuntimeError::WriteFileNotAllowed, call_range);
                    return None;
                };
                if !sink.push(path.as_ref(), content) {
                    self.runtime_error(RuntimeError::PathNotInOutDir(path), call_range);
                    return None;
                }
            },
        }
        
        Some(Value::UNIT)
    }
}

fn take_val(bindings: &mut ValueMap, key: NameID) -> Option<Value> {
    bindings.insert(key, Value::UNIT)
}
