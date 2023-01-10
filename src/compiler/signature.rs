use std::rc::Rc;

use crate::utils::{NameID, str_ids, NameIDMap};
use crate::errors;
use crate::parser::{ast, Type};
use super::base::Compiler;
use super::func::Func;
use super::value::{Value, Dict, List, RcDict};

#[derive(Debug)]
pub struct FuncParam {
    name_id: NameID,
    is_implicit: bool,
    type_: Type,
    default_value: Option<Box<Value>>,
}

impl FuncParam {
    pub(super) fn new(name_id: NameID, type_: Type) -> FuncParam {
        FuncParam {
            name_id,
            is_implicit: false,
            type_,
            default_value: None,
        }
    }
    
    pub(super) fn with_default(mut self, default_value: Value) -> FuncParam {
        self.default_value = Some(Box::new(default_value));
        self
    }
}

#[derive(Debug)]
pub struct FuncSignature {
    positional_params: Vec<FuncParam>,
    spread_param: Option<Box<FuncParam>>,
    named_params: NameIDMap<FuncParam>,
    spread_named_param: Option<Box<FuncParam>>,
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
    pub(super) fn builder() -> FuncSignature {
        FuncSignature {
            positional_params: Vec::new(),
            spread_param: None,
            named_params: NameIDMap::default(),
            spread_named_param: None,
            content_param: FuncParam::new(str_ids::ANONYMOUS, Type::Unit),
        }
    }
    
    pub(super) fn positional(mut self, param: FuncParam) -> FuncSignature {
        self.positional_params.push(param);
        self
    }
    
    pub(super) fn named(mut self, param: FuncParam) -> FuncSignature {
        self.named_params.insert(param.name_id, param);
        self
    }
    
    pub(super) fn implicit(self, mut param: FuncParam) -> FuncSignature {
        param.is_implicit = true;
        self.named(param)
    }
    
    pub(super) fn pos_spread(mut self, param: FuncParam) -> FuncSignature {
        self.spread_param = Some(Box::new(param));
        self
    }
    
    pub(super) fn named_spread(mut self, param: FuncParam) -> FuncSignature {
        self.spread_named_param = Some(Box::new(param));
        self
    }
    
    pub(super) fn content(mut self, param: FuncParam) -> FuncSignature {
        self.content_param = param;
        self
    }
    
    pub(super) fn build(self: FuncSignature) -> RcFuncSignature {
        RcFuncSignature(Rc::new(self))
    }
}

#[derive(Debug, Clone)]
pub struct PartialParams {
    positional_arg_count: usize,
    spread_pos: Vec<Value>,
    spread_named: Dict,
    map: Dict,
}

impl Default for PartialParams {
    fn default() -> PartialParams {
        PartialParams::new()
    }
}

impl PartialParams {
    pub(super) fn new() -> PartialParams {
        PartialParams {
            positional_arg_count: 0,
            spread_pos: Vec::new(),
            spread_named: Dict::default(),
            map: Dict::default(),
        }
    }
    
    pub(super) fn open<'a, 'b>(self, compiler: &'a mut Compiler<'b>, sig: RcFuncSignature) -> ParamBinder<'a, 'b> {
        ParamBinder {
            compiler,
            sig,
            bound: self,
        }
    }
}

pub(super) struct ParamBinder<'a, 'b> {
    compiler: &'a mut Compiler<'b>,
    sig: RcFuncSignature,
    bound: PartialParams,
}

impl <'a, 'b> ParamBinder<'a, 'b> {
    pub(super) fn close_into_bound_function(mut self, f: Func) -> errors::PapyriResult<Func> {
        self.check_pos_count()?;
        let pair = (f, self.bound);
        Ok(Func::Bound(Rc::new(pair)))
    }
    
    pub(super) fn compile_arg(&mut self, arg: &ast::Arg) -> errors::Reported {
        if arg.is_positional() {
            self.compile_positional_arg(arg)
        } else {
            self.compile_named_arg(arg)
        }.map_err(|e| self.compiler.report(e, arg.range))
    }
    
    fn compile_positional_arg(&mut self, arg: &ast::Arg) -> errors::PapyriResult {
        let sig = self.sig.as_ref();
        
        if arg.is_spread() {
            let vs: List = self.compiler.evaluate_node(&arg.value, &Type::Any.list())?
                .expect_convert();
            for v in vs.as_ref().iter().cloned() {
                self.add_positional_arg(v)?;
            }
            Ok(())
        } else {
            let type_hint = if let Some(param) = sig.positional_params.get(self.bound.positional_arg_count) {
                &param.type_
            } else if let Some(param) = &sig.spread_param {
                param.type_.component_type()
            } else {
                &Type::Any
            };
            let v = self.compiler.evaluate_node(&arg.value, type_hint)?;
            self.add_positional_arg(v)
        }
    }
    
    fn compile_named_arg(&mut self, arg: &ast::Arg) -> errors::PapyriResult {
        let sig = self.sig.as_ref();
        
        if arg.is_spread() {
            let vs: RcDict = self.compiler.evaluate_node(&arg.value, &Type::Any.dict())?
                .expect_convert();
            for (&k, v) in vs.iter() {
                self.add_named_arg(k, v.clone())?;
            }
            Ok(())
        } else {
            let type_hint = if let Some(param) = sig.named_params.get(&arg.name_id) {
                &param.type_
            } else if let Some(param) = &sig.spread_named_param {
                param.type_.component_type()
            } else {
                &Type::Any
            };
            let v = self.compiler.evaluate_node(&arg.value, type_hint)?;
            self.add_named_arg(arg.name_id, v)
        }
    }
    
    pub(super) fn bind_implicit_args(&mut self) -> errors::PapyriResult {
        let sig = self.sig.as_ref();
        
        for param in sig.named_params.values() {
            if !param.is_implicit || self.bound.map.contains_key(&param.name_id) {
                // do nothing
                continue;
            }
            
            let v = self.compiler.get_implicit(param.name_id, param.default_value.as_ref().map(|v| *v.clone()))?;
            self.bound.map.insert(param.name_id, v);
        }
        Ok(())
    }
    
    pub(super) fn compile_content_arg(&mut self, node: &ast::Expr) -> errors::Reported {
        let sig = self.sig.as_ref();
        
        let type_hint = if self.bound.map.contains_key(&sig.content_param.name_id) {
            // this happens if the function is already partially bound
            // type hint needs to include 'none'; if the value is not `Value::UNIT`,
            // then we'll report a different error anyway.
            &Type::Any
        } else {
            &sig.content_param.type_
        };
        
        let v = self.compiler.evaluate_node(node, type_hint)?;
        self.add_content_arg(v)
            .map_err(|e| self.compiler.report(e, node.range()))
    }
    
    pub(super) fn add_positional_arg(&mut self, value: Value) -> errors::PapyriResult {
        let sig = self.sig.as_ref();
        
        if let Some(param) = sig.positional_params.get(self.bound.positional_arg_count) {
            let v = self.compiler.coerce(value, &param.type_)?;
            self.bound.map.insert(param.name_id, v);
        } else if let Some(param) = &sig.spread_param {
            let v = self.compiler.coerce(value, param.type_.component_type())?;
            self.bound.spread_pos.push(v);
        } else {
            // do nothing; `TypeError::TooManyPositionalArgs` will be reported later
        }
        self.bound.positional_arg_count += 1;
        Ok(())
    }
    
    pub(super) fn add_named_arg(&mut self, name_id: NameID, value: Value) -> errors::PapyriResult {
        let sig = self.sig.as_ref();
        
        let (type_, map) = if let Some(param) = sig.named_params.get(&name_id) {
            (&param.type_, &mut self.bound.map)
        } else if let Some(param) = &sig.spread_named_param {
            (param.type_.component_type(), &mut self.bound.spread_named)
        } else {
            let name = self.compiler.get_name(name_id);
            let e = errors::NameError::NoSuchParameter(name);
            return Err(e.into());
        };
        
        let v = self.compiler.coerce(value, type_)?;
        if map.insert(name_id, v).is_some() {
            let name = self.compiler.get_name(name_id);
            let e = errors::RuntimeError::ParamMultipleValues(name);
            return Err(e.into());
        }
        Ok(())
    }
    
    pub(super) fn add_content_arg(&mut self, content_value: Value) -> errors::PapyriResult {
        let sig = self.sig.as_ref();
        let name_id = sig.content_param.name_id;
        
        if self.bound.map.contains_key(&name_id) {
            if !content_value.is_unit() {
                let e = errors::TypeError::ContentAlreadyBound;
                return Err(e.into());
            }
        } else {
            let v = self.compiler.coerce(content_value, &sig.content_param.type_)?;
            if !name_id.is_anonymous() {
                self.bound.map.insert(name_id, v);
            }
        }
        Ok(())
    }
    
    /// Checks that this binder has not received too many positional arguments.
    fn check_pos_count(&mut self) -> errors::PapyriResult {
        let sig = self.sig.as_ref();
        
        if sig.spread_param.is_none() && self.bound.positional_arg_count > sig.positional_params.len() {
            let e = errors::TypeError::TooManyPositionalArgs(
                sig.positional_params.len(),
                self.bound.positional_arg_count,
            );
            Err(e.into())
        } else {
            Ok(())
        }
    }
    
    pub(super) fn build(mut self) -> errors::PapyriResult<Dict> {
        self.check_pos_count()?;
        
        let sig = self.sig.as_ref();
        
        if let Some(param) = &sig.spread_param {
            let v = self.compiler.coerce(self.bound.spread_pos.into(), &param.type_)?;
            self.bound.map.insert(param.name_id, v);
        }
        if let Some(param) = &sig.spread_named_param {
            let v = self.compiler.coerce(self.bound.spread_named.into(), &param.type_)?;
            self.bound.map.insert(param.name_id, v);
        }
        
        let content_name_id = sig.content_param.name_id;
        if !content_name_id.is_anonymous() && !self.bound.map.contains_key(&content_name_id) {
            errors::ice("No content arg provided");
        }
        
        if self.bound.positional_arg_count < sig.positional_params.len() {
            for param in &sig.positional_params[self.bound.positional_arg_count..] {
                let Some(v) = &param.default_value else {
                    let expected_count = sig.positional_params.iter().filter(|p| p.default_value.is_none()).count();
                    let e = errors::TypeError::NotEnoughPositionalArgs(expected_count, self.bound.positional_arg_count);
                    return Err(e.into());
                };
                self.bound.map.insert(param.name_id, *v.clone());
            }
        }
        for param in sig.named_params.values() {
            if self.bound.map.contains_key(&param.name_id) { continue; }
            let Some(v) = &param.default_value else {
                let name = self.compiler.get_name(param.name_id);
                let e = errors::RuntimeError::ParamMissing(name);
                return Err(e.into());
            };
            self.bound.map.insert(param.name_id, *v.clone());
        }
        
        Ok(self.bound.map)
    }
}

impl <'a> Compiler<'a> {
    pub(super) fn compile_func_signature(&mut self, sig: &ast::Signature) -> RcFuncSignature {
        let mut out = FuncSignature::builder();
        let mut params = sig.params.iter()
            .map(|p| self.compile_param(p));
        
        for _ in 0..sig.positional_count {
            out = out.positional(params.next().unwrap());
        }
        if sig.positional_spread {
            out = out.pos_spread(params.next().unwrap());
        }
        for _ in 0..sig.named_count {
            out = out.named(params.next().unwrap());
        }
        if sig.named_spread {
            out = out.named_spread(params.next().unwrap());
        }
        if sig.has_content {
            out = out.content(params.next().unwrap());
        }
        out.build()
    }
    
    pub(super) fn compile_param(&mut self, param: &ast::Param) -> FuncParam {
        let type_ = param.type_annotation.clone()
            .option_if(param.question_mark);
        
        let default_value = param.default_value.as_ref()
            .and_then(|v| match self.evaluate_node(v, &type_) {
                Ok(v) => Some(v),
                Err(errors::AlreadyReported) => None,
            })
            .or_else(|| param.question_mark.then_some(Value::UNIT))
            .map(Box::new);
        
        FuncParam {
            name_id: param.name_id,
            is_implicit: param.is_implicit,
            type_,
            default_value,
        }
    }
}
