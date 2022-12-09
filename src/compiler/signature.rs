use std::rc::Rc;
use indexmap::IndexMap;

use crate::utils::{NameID, SourceRange, str_ids};
use crate::errors;
use crate::parser::ast;
use super::compiler::Compiler;
use super::func::Func;
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
    
    pub fn unit_default(mut self) -> FuncParam {
        self.default_value = Some(Value::UNIT);
        self
    }
    
    pub fn with_default<T: Into<Value>>(mut self, default_value: T) -> FuncParam {
        self.default_value = Some(default_value.into());
        self
    }
}

#[derive(Debug)]
pub struct FuncSignature {
    positional_params: Vec<FuncParam>,
    spread_param: Option<FuncParam>,
    named_params: IndexMap<NameID, FuncParam>,
    spread_named_param: Option<FuncParam>,
    content_param: FuncParam,
}

#[derive(Debug, Clone)]
/// A reference-counted pointer to a `FuncSignature`.
pub struct RcFuncSignature(Rc<FuncSignature>);

impl AsRef<FuncSignature> for RcFuncSignature {
    fn as_ref(&self) -> &FuncSignature {
        self.0.as_ref()
    }
}

impl FuncSignature {
    pub fn new() -> FuncSignature {
        FuncSignature {
            positional_params: Vec::new(),
            spread_param: None,
            named_params: IndexMap::new(),
            spread_named_param: None,
            content_param: FuncParam::new(str_ids::ANONYMOUS, Type::Unit),
        }
    }
    
    pub fn positional(mut self, param: FuncParam) -> FuncSignature {
        self.positional_params.push(param);
        self
    }
    
    pub fn named(mut self, param: FuncParam) -> FuncSignature {
        self.named_params.insert(param.name_id, param);
        self
    }
    
    pub fn pos_spread(mut self, param: FuncParam) -> FuncSignature {
        self.spread_param = Some(param);
        self
    }
    
    pub fn named_spread(mut self, param: FuncParam) -> FuncSignature {
        self.spread_named_param = Some(param);
        self
    }
    
    pub fn content(mut self, param: FuncParam) -> FuncSignature {
        self.content_param = param;
        self
    }
    
    pub fn build(self: FuncSignature) -> RcFuncSignature {
        RcFuncSignature(Rc::new(self))
    }
}

#[derive(Debug, Clone)]
pub struct PartialParams {
    positional_arg_count: usize,
    spread_pos: Vec<Value>,
    spread_named: ValueMap,
    map: ValueMap,
}

impl PartialParams {
    pub fn new() -> PartialParams {
        PartialParams {
            positional_arg_count: 0,
            spread_pos: Vec::new(),
            spread_named: ValueMap::new(),
            map: ValueMap::new(),
        }
    }
    
    pub fn open<'a, 'b>(self, compiler: &'a mut Compiler<'b>, sig: RcFuncSignature) -> ParamBinder<'a, 'b> {
        ParamBinder {
            compiler,
            sig,
            bound: self,
            any_errors: false,
        }
    }
}

pub struct ParamBinder<'a, 'b> {
    compiler: &'a mut Compiler<'b>,
    sig: RcFuncSignature,
    bound: PartialParams,
    any_errors: bool,
}

impl <'a, 'b> ParamBinder<'a, 'b> {
    pub fn new(compiler: &'a mut Compiler<'b>, sig: RcFuncSignature) -> ParamBinder<'a, 'b> {
        PartialParams::new()
            .open(compiler, sig)
    }
    
    pub fn close_partial(mut self, call_range: &SourceRange) -> Option<PartialParams> {
        self.check_pos_count(call_range);
        (!self.any_errors)
            .then_some(self.bound)
    }
    
    pub fn close_into_bound_function(self, f: Func, call_range: &SourceRange) -> Option<Func> {
        self.close_partial(call_range)
            .map(|b| Func::Bound(Rc::new((f, b))))
    }
    
    pub fn compile_positional_arg(&mut self, arg: &ast::Arg) {
        let sig = self.sig.as_ref();
        
        if arg.is_spread() {
            if let Some(Value::List(vs)) = self.compiler.evaluate_node(&arg.value, &Type::Any.list()) {
                for v in vs.as_ref().iter() {
                    self.add_positional_arg(v.clone(), arg.value.range());
                }
            } else {
                self.any_errors = true;
            }
        } else {
            let type_hint = if let Some(param) = sig.positional_params.get(self.bound.positional_arg_count) {
                &param.type_
            } else if let Some(param) = &sig.spread_param {
                param.type_.component_type()
            } else {
                &Type::Any
            };
            if let Some(v) = self.compiler.evaluate_node(&arg.value, type_hint) {
                self.add_positional_arg(v, arg.value.range());
            } else {
                self.any_errors = true;
            }
        }
    }
    
    pub fn compile_named_arg(&mut self, arg: &ast::Arg) {
        let sig = self.sig.as_ref();
        
        if arg.is_spread() {
            if let Some(Value::Dict(vs)) = self.compiler.evaluate_node(&arg.value, &Type::Any.dict()) {
                for (&k, v) in vs.iter() {
                    self.add_named_arg(k, v.clone(), arg.value.range());
                }
            } else {
                self.any_errors = true;
            }
        } else {
            let type_hint = if let Some(param) = sig.named_params.get(&arg.name_id) {
                &param.type_
            } else if let Some(param) = &sig.spread_named_param {
                param.type_.component_type()
            } else {
                &Type::Any
            };
            if let Some(v) = self.compiler.evaluate_node(&arg.value, type_hint) {
                self.add_named_arg(arg.name_id, v, arg.value.range());
            } else {
                self.any_errors = true;
            }
        }
    }
    
    pub fn bind_implicit_args(&mut self, call_range: &SourceRange) {
        let sig = self.sig.as_ref();
        
        for param in sig.named_params.values() {
            if !param.is_implicit || self.bound.map.contains_key(&param.name_id) {
                // do nothing
            } else if let Some(v) = self.compiler.get_implicit(param.name_id, param.default_value.clone(), call_range) {
                self.bound.map.insert(param.name_id, v.clone());
            } else {
                self.any_errors = true;
            }
        }
    }
    
    pub fn compile_content_arg(&mut self, node: &ast::AST) {
        let sig = self.sig.as_ref();
        
        let type_hint = if self.bound.map.contains_key(&sig.content_param.name_id) {
            // this happens if the function is already partially bound
            // type hint needs to include 'none'; if the value is not `Value::UNIT`,
            // then we'll report a different error anyway.
            &Type::Any
        } else {
            &sig.content_param.type_
        };
        
        if let Some(v) = self.compiler.evaluate_node(node, type_hint) {
            self.add_content_arg(v, node.range());
        } else {
            self.any_errors = true;
        }
    }
    
    pub fn add_positional_arg(&mut self, value: Value, range: &SourceRange) {
        let sig = self.sig.as_ref();
        
        if let Some(param) = sig.positional_params.get(self.bound.positional_arg_count) {
            match self.compiler.coerce(value, &param.type_, range) {
                Some(v) => { self.bound.map.insert(param.name_id, v); },
                None => self.any_errors = true,
            }
        } else if let Some(param) = &sig.spread_param {
            match self.compiler.coerce(value, &param.type_.component_type(), range) {
                Some(v) => self.bound.spread_pos.push(v),
                None => self.any_errors = true,
            }
        } else {
            self.any_errors = true;
        }
        self.bound.positional_arg_count += 1;
    }
    
    pub fn add_named_arg(&mut self, name_id: NameID, value: Value, range: &SourceRange) {
        let sig = self.sig.as_ref();
        
        let (type_, map) = if let Some(param) = sig.named_params.get(&name_id) {
            (&param.type_, &mut self.bound.map)
        } else if let Some(param) = &sig.spread_named_param {
            (param.type_.component_type(), &mut self.bound.spread_named)
        } else {
            let name = self.compiler.get_name(name_id).to_string();
            self.compiler.name_error(errors::NameError::NoSuchParameter(name), range);
            self.any_errors = true;
            return;
        };
        match self.compiler.coerce(value, type_, range) {
            Some(v) => {
                if map.insert(name_id, v).is_some() {
                    let name = self.compiler.get_name(name_id).to_string();
                    self.compiler.name_error(errors::NameError::NoSuchParameter(name), range);
                    self.any_errors = true;
                }
            },
            None => { self.any_errors = true; },
        }
    }
    
    pub fn add_content_arg(&mut self, content_value: Value, range: &SourceRange) {
        let sig = self.sig.as_ref();
        let name_id = sig.content_param.name_id;
        
        if self.bound.map.contains_key(&name_id) {
            if !content_value.is_unit() {
                self.compiler.type_error(
                    errors::TypeError::ContentAlreadyBound,
                    range,
                );
                self.any_errors = true;
            }
        } else if let Some(v) = self.compiler.coerce(content_value, &sig.content_param.type_, range) {
            self.bound.map.insert(name_id, v);
        } else {
            self.any_errors = true;
        }
    }
    
    fn check_pos_count(&mut self, call_range: &SourceRange) {
        let sig = self.sig.as_ref();
        
        if sig.spread_param.is_none() && self.bound.positional_arg_count > sig.positional_params.len() {
            self.compiler.type_error(
                errors::TypeError::TooManyPositionalArgs(
                    sig.positional_params.len(),
                    self.bound.positional_arg_count,
                ),
                call_range,
            );
            self.any_errors = true;
        }
    }
    
    pub fn build(mut self, call_range: &SourceRange) -> Option<ValueMap> {
        self.check_pos_count(call_range);
        if self.any_errors { return None; }
        
        let sig = self.sig.as_ref();
        
        if let Some(param) = &sig.spread_param {
            let v = self.compiler.coerce(self.bound.spread_pos.into(), &param.type_, call_range)?;
            self.bound.map.insert(param.name_id, v);
        }
        if let Some(param) = &sig.spread_named_param {
            let v = self.compiler.coerce(self.bound.spread_named.into(), &param.type_, call_range)?;
            self.bound.map.insert(param.name_id, v);
        }
        if !self.bound.map.contains_key(&sig.content_param.name_id) {
            errors::ice_at("no content arg provided", call_range);
        }
        
        if self.bound.positional_arg_count < sig.positional_params.len() {
            for param in &sig.positional_params[self.bound.positional_arg_count..] {
                let Some(v) = &param.default_value else {
                    self.compiler.type_error(
                        errors::TypeError::NotEnoughPositionalArgs(
                            sig.positional_params.iter().filter(|p| p.default_value.is_none()).count(),
                            self.bound.positional_arg_count,
                        ),
                        call_range,
                    );
                    return None;
                };
                self.bound.map.insert(param.name_id, v.clone());
            }
        }
        for param in sig.named_params.values() {
            if self.bound.map.contains_key(&param.name_id) { continue; }
            let Some(v) = &param.default_value else {
                let name = self.compiler.get_name(param.name_id).to_string();
                self.compiler.runtime_error(errors::RuntimeError::ParamMissing(name), call_range);
                return None;
            };
            self.bound.map.insert(param.name_id, v.clone());
        }
        
        Some(self.bound.map)
    }
}

impl <'a> Compiler<'a> {
    pub fn compile_func_signature(&mut self, sig: &ast::Signature) -> RcFuncSignature {
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
        }.build()
    }
    
    pub fn compile_param(&mut self, param: &ast::Param) -> FuncParam {
        let type_ = param.type_annotation.as_ref()
            .map_or(Type::Any, Type::compile)
            .option_if(param.question_mark);
        
        let default_value = param.default_value.as_ref()
            .map(|v| self.evaluate_node(v, &type_))
            .flatten()
            .or(param.question_mark.then_some(Value::UNIT));
        
        FuncParam {
            name_id: param.name_id,
            is_implicit: param.is_implicit,
            type_,
            default_value,
        }
    }
}
