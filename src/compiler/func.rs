use std::collections::HashMap;
use std::rc::Rc;

use crate::parser::{ast, AST};
use crate::utils::{Diagnostics, SourceRange, ice_at, NameID};
use super::compiler::Compiler;
use super::frame::InactiveFrame;
use super::native::NativeFunc;
use super::types::Type;
use super::value::{Value, ValueMap};

#[derive(Debug)]
pub struct FuncParam {
    name_id: NameID,
    is_implicit: bool,
    type_: Type,
    default_value: Option<Value>,
}

impl FuncParam {
    pub fn new(name_id: NameID, type_: Type) -> FuncParam {
        FuncParam {
            name_id,
            is_implicit: false,
            type_,
            default_value: None,
        }
    }
    
    pub fn implicit(mut self) -> FuncParam {
        self.is_implicit = true;
        self
    }
    
    pub fn with_default(mut self, default_value: Value) -> FuncParam {
        self.default_value = Some(default_value);
        self
    }
}

#[derive(Debug)]
pub struct FuncSignature {
    pub positional_params: Box<[FuncParam]>,
    pub spread_param: Option<FuncParam>,
    pub named_params: HashMap<NameID, FuncParam>,
    pub spread_named_param: Option<FuncParam>,
    pub content_param: FuncParam,
}

impl FuncSignature {
    pub fn bind_synthetic_call(&self, compiler: &mut Compiler, bind_implicits: bool, content_value: Value, call_range: &SourceRange) -> Option<ValueMap> {
        let mut binder = ParamBinder::new(compiler, self);
        if bind_implicits { binder.bind_implicit_args(call_range); }
        binder.add_content_arg(content_value, call_range);
        binder.build(call_range)
    }
    
    pub fn bind_call(&self, compiler: &mut Compiler, call: &ast::FuncCall) -> Option<ValueMap> {
        let mut binder = ParamBinder::new(compiler, self);
        for arg in call.args.iter() {
            if arg.is_positional() {
                binder.compile_positional_arg(arg);
            } else {
                binder.compile_named_arg(arg);
            }
        }
        binder.bind_implicit_args(&call.range);
        binder.compile_content_arg(&call.content);
        binder.build(&call.range)
    }
}

#[derive(Debug, Clone)]
pub enum Func {
    NonNative(Rc<NonNativeFunc>),
    Native(NativeFunc, Rc<FuncSignature>),
}

impl Func {
    pub fn name_id(&self) -> NameID {
        match self {
            Func::NonNative(f) => f.name_id,
            Func::Native(f, _) => f.name_id(),
        }
    }
    
    pub fn signature(&self) -> &FuncSignature {
        match self {
            Func::NonNative(f) => &f.signature,
            Func::Native(_, sig) => sig,
        }
    }
}

#[derive(Debug)]
pub struct NonNativeFunc {
    name_id: NameID,
    closure: InactiveFrame,
    signature: FuncSignature,
    body: Rc<AST>,
}

pub struct ParamBinder<'a, 'b> {
    compiler: &'a mut Compiler<'b>,
    sig: &'a FuncSignature,
    spread_pos: Vec<Value>,
    spread_named: ValueMap,
    map: ValueMap,
    positional_arg_count: usize,
    any_errors: bool,
}

impl <'a, 'b> ParamBinder<'a, 'b> {
    fn new(compiler: &'a mut Compiler<'b>, sig: &'a FuncSignature) -> ParamBinder<'a, 'b> {
        ParamBinder {
            compiler,
            sig,
            spread_pos: Vec::new(),
            spread_named: HashMap::new(),
            map: HashMap::new(),
            positional_arg_count: 0,
            any_errors: false,
        }
    }
    
    fn compile_positional_arg(&mut self, arg: &ast::Arg) {
        if arg.is_spread() {
            if let Some(Value::List(vs)) = self.compiler.evaluate_node(&arg.value, &Type::list(Type::AnyValue)) {
                for v in vs.as_ref().iter() {
                    self.add_positional_arg(v.clone(), arg.value.range());
                }
            } else {
                self.any_errors = true;
            }
        } else {
            let tmp_type: Type;
            let type_hint = if let Some(param) = self.sig.positional_params.get(self.positional_arg_count) {
                &param.type_
            } else if let Some(param) = &self.sig.spread_param {
                tmp_type = param.type_.component_type();
                &tmp_type
            } else {
                &Type::AnyValue
            };
            if let Some(v) = self.compiler.evaluate_node(&arg.value, type_hint) {
                self.add_positional_arg(v, arg.value.range());
            } else {
                self.any_errors = true;
            }
        }
    }
    
    fn compile_named_arg(&mut self, arg: &ast::Arg) {
        if arg.is_spread() {
            if let Some(Value::Dict(vs)) = self.compiler.evaluate_node(&arg.value, &Type::dict(Type::AnyValue)) {
                for (k, v) in vs.iter() {
                    self.add_named_arg(*k, v.clone(), arg.value.range());
                }
            } else {
                self.any_errors = true;
            }
        } else {
            let tmp_type: Type;
            let type_hint = if let Some(param) = self.sig.named_params.get(&arg.name_id) {
                &param.type_
            } else if let Some(param) = &self.sig.spread_named_param {
                tmp_type = param.type_.component_type();
                &tmp_type
            } else {
                &Type::AnyValue
            };
            if let Some(v) = self.compiler.evaluate_node(&arg.value, type_hint) {
                self.add_named_arg(arg.name_id, v, arg.value.range());
            } else {
                self.any_errors = true;
            }
        }
    }
    
    fn bind_implicit_args(&mut self, call_range: &SourceRange) {
        for param in self.sig.named_params.values() {
            if !param.is_implicit || self.map.contains_key(&param.name_id) {
                // do nothing
            } else if let Some(v) = self.compiler.get_implicit(param.name_id, param.default_value.clone(), call_range) {
                self.map.insert(param.name_id, v.clone());
            } else {
                self.any_errors = true;
            }
        }
    }
    
    fn compile_content_arg(&mut self, node: &AST) {
        let type_hint = &self.sig.content_param.type_;
        if let Some(v) = self.compiler.evaluate_node(node, type_hint) {
            self.add_content_arg(v, node.range());
        } else {
            self.any_errors = true;
        }
    }
    
    pub fn add_positional_arg(&mut self, value: Value, range: &SourceRange) {
        if let Some(param) = self.sig.positional_params.get(self.positional_arg_count) {
            match self.compiler.coerce(value, &param.type_, range) {
                Some(v) => { self.map.insert(param.name_id, v); },
                None => { self.any_errors = true; },
            }
        } else {
            self.any_errors = true;
        }
        self.positional_arg_count += 1;
    }
    
    pub fn add_named_arg(&mut self, name_id: NameID, value: Value, range: &SourceRange) {
        let tmp_type: Type;
        let (type_, map) = if let Some(param) = self.sig.named_params.get(&name_id) {
            (&param.type_, &mut self.map)
        } else if let Some(param) = &self.sig.spread_named_param {
            tmp_type = param.type_.component_type();
            (&tmp_type, &mut self.spread_named)
        } else {
            self.compiler.diagnostics.name_error(&format!("no such parameter '{}'", self.compiler.get_name(name_id)), range);
            self.any_errors = true;
            return;
        };
        match self.compiler.coerce(value, type_, range) {
            Some(v) => {
                if map.insert(name_id, v).is_some() {
                    self.compiler.diagnostics.error(&format!("received multiple values for parameter '{}'", self.compiler.get_name(name_id)), range);
                    self.any_errors = true;
                }
            },
            None => { self.any_errors = true; },
        }
    }
    
    pub fn add_content_arg(&mut self, content_value: Value, range: &SourceRange) {
        if let Some(v) = self.compiler.coerce(content_value, &self.sig.content_param.type_, range) {
            if self.map.insert(self.sig.content_param.name_id, v).is_some() {
                ice_at("multiple content args", range);
            }
        } else {
            self.any_errors = true;
        }
    }
    
    pub fn build(mut self, call_range: &SourceRange) -> Option<ValueMap> {
        if self.positional_arg_count > self.sig.positional_params.len() {
            self.compiler.diagnostics.err_positional_args_count(
                self.sig.positional_params.len(),
                self.positional_arg_count,
                call_range,
            );
            return None;
        } else if self.any_errors {
            return None;
        }
        
        if let Some(param) = &self.sig.spread_param {
            let v = self.compiler.coerce(Value::list(self.spread_pos), &param.type_, call_range)?;
            self.map.insert(param.name_id, v);
        }
        if let Some(param) = &self.sig.spread_named_param {
            let v = self.compiler.coerce(Value::dict(self.spread_named), &param.type_, call_range)?;
            self.map.insert(param.name_id, v);
        }
        if !self.map.contains_key(&self.sig.content_param.name_id) {
            ice_at("no content arg provided", call_range);
        }
        
        if self.positional_arg_count < self.sig.positional_params.len() {
            for param in &self.sig.positional_params[self.positional_arg_count..] {
                let Some(v) = &param.default_value else {
                    self.compiler.diagnostics.err_positional_args_count(
                        self.sig.positional_params.iter().filter(|p| p.default_value.is_none()).count(),
                        self.positional_arg_count,
                        call_range,
                    );
                    return None;
                };
                self.map.insert(param.name_id, v.clone());
            }
        }
        for param in self.sig.named_params.values() {
            if self.map.contains_key(&param.name_id) { continue; }
            let Some(v) = &param.default_value else {
                self.compiler.diagnostics.error(&format!("missing required argument '{}'", self.compiler.get_name(param.name_id)), call_range);
                return None;
            };
            self.map.insert(param.name_id, v.clone());
        }
        
        Some(self.map)
    }
}

impl <'a> Compiler<'a> {
    pub fn compile_func_def(&mut self, def: &ast::FuncDef) -> Func {
        Func::NonNative(Rc::new(NonNativeFunc {
            name_id: def.name_id,
            closure: self.frame.to_inactive(),
            signature: self.compile_func_signature(&def.signature),
            body: def.body.clone(),
        }))
    }
    
    pub fn compile_func_signature(&mut self, sig: &ast::Signature) -> FuncSignature {
        FuncSignature {
            positional_params: sig.positional_params.iter()
                .map(|p| self.compile_param(p))
                .collect(),
            spread_param: sig.spread_param.as_ref()
                .map(|p| self.compile_param(p)),
            named_params: sig.named_params.iter()
                .map(|p| (p.name_id, self.compile_param(p)))
                .collect(),
            spread_named_param: sig.spread_named_param.as_ref()
                .map(|p| self.compile_param(p)),
            content_param: self.compile_param(&sig.content_param),
        }
    }
    
    pub fn compile_param(&mut self, param: &ast::Param) -> FuncParam {
        let mut type_ = param.type_annotation.as_ref()
            .map_or(Type::AnyValue, Type::compile);
        let mut default_value = param.default_value.as_ref()
            .map(|v| self.evaluate_node(v, &type_))
            .flatten();
        
        if param.question_mark {
            type_ = Type::optional(type_);
            default_value = default_value.or(Some(Value::Unit));
        }
        FuncParam {
            name_id: param.name_id,
            is_implicit: param.is_implicit,
            type_,
            default_value,
        }
    }
    
    pub fn evaluate_func_call(&mut self, call: &ast::FuncCall, type_hint: &Type) -> Option<Value> {
        match self.get_var(call.name_id, &call.range)? {
            Value::Func(f) => {
                let bindings = f.signature().bind_call(self, call)?;
                self.evaluate_func_call_with_bindings(f, bindings, type_hint, &call.range)
            },
            _ => {
                self.diagnostics.type_error(&format!("'{}' is not a function", self.get_name(call.name_id)), &call.range);
                None
            },
        }
    }
    
    pub fn evaluate_func_call_with_bindings(&mut self, func: Func, bindings: ValueMap, type_hint: &Type, call_range: &SourceRange) -> Option<Value> {
        match func {
            Func::NonNative(f) => {
                let frame = f.closure.new_child_frame(bindings);
                self.evaluate_in_frame(frame, f.body.as_ref(), type_hint)
            },
            Func::Native(f, _) => {
                self.evaluate_native_func(f, bindings, call_range)
            },
        }
    }
}

impl Diagnostics {
    pub fn err_positional_args_count(&mut self, expected: usize, was: usize, range: &SourceRange) {
        self.type_error(&format!(
            "{} positional arguments (expected {}, was {})",
            if was < expected { "not enough" } else { "too many" },
            expected,
            was,
        ), range);
    }
}
