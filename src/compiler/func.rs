use std::rc::Rc;

use crate::errors;
use crate::parser::ast;
use crate::utils::{SourceRange, NameID};
use super::base::Compiler;
use super::frame::InactiveFrame;
use super::native::NativeFunc;
use super::signature::{RcFuncSignature, PartialParams};
use super::types::Type;
use super::value::{Value, ValueMap};

#[derive(Debug, Clone)]
pub enum Func {
    NonNative(Rc<NonNativeFunc>),
    Native(NativeFunc, RcFuncSignature),
    Bound(Rc<(Func, PartialParams)>),
}

impl Func {
    pub fn name_id(&self) -> NameID {
        match self {
            Func::NonNative(f) => f.name_id,
            Func::Native(f, _) => f.name_id(),
            Func::Bound(f) => f.0.name_id(),
        }
    }
    
    pub fn signature(&self) -> RcFuncSignature {
        match self {
            Func::NonNative(f) => f.signature.clone(),
            Func::Native(_, sig) => sig.clone(),
            Func::Bound(f) => f.0.signature(),
        }
    }
    
    fn get_partials(&self) -> PartialParams {
        match self {
            Func::Bound(f) => f.1.clone(),
            _ => PartialParams::new(),
        }
    }
    
    pub fn bind_content(&self, compiler: &mut Compiler, content_arg: Value, range: &SourceRange) -> Option<Func> {
        let mut binder = self.get_partials()
            .open(compiler, self.signature());
        
        binder.add_content_arg(content_arg, range);
        binder.close_into_bound_function(self.clone(), range)
    }
    
    pub fn bind_pos_arg(&self, compiler: &mut Compiler, arg: Value, range: &SourceRange) -> Option<Func> {
        let mut binder = self.get_partials()
            .open(compiler, self.signature());
        
        binder.add_positional_arg(arg, range);
        binder.close_into_bound_function(self.clone(), range)
    }
    
    pub fn bind_partial(&self, compiler: &mut Compiler, positional_args: &[Value], named_args: &ValueMap, content_arg: Value, call_range: &SourceRange) -> Option<Func> {
        let mut binder = self.get_partials()
            .open(compiler, self.signature());
        
        for arg in positional_args.as_ref().iter().cloned() {
            binder.add_positional_arg(arg, call_range);
        }
        for (&name_id, arg) in named_args.iter() {
            binder.add_named_arg(name_id, arg.clone(), call_range);
        }
        if !content_arg.is_unit() {
            binder.add_content_arg(content_arg, call_range);
        }
        
        binder.close_into_bound_function(self.clone(), call_range)
    }
    
    pub fn bind_synthetic_call(&self, compiler: &mut Compiler, bind_implicits: bool, content_value: Value, call_range: &SourceRange) -> Option<ValueMap> {
        let mut binder = self.get_partials()
            .open(compiler, self.signature());
        if bind_implicits {
            binder.bind_implicit_args(call_range);
        }
        binder.add_content_arg(content_value, call_range);
        binder.build(call_range)
    }
    
    fn bind_call(&self, compiler: &mut Compiler, call: &ast::FuncCall) -> Option<ValueMap> {
        let mut binder = self.get_partials()
            .open(compiler, self.signature());
        
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

#[derive(Debug)]
pub struct NonNativeFunc {
    name_id: NameID,
    closure: InactiveFrame,
    signature: RcFuncSignature,
    body: Rc<ast::AST>,
}

impl <'a> Compiler<'a> {
    pub fn compile_func_def(&mut self, def: &ast::FuncDef) -> Func {
        Func::NonNative(Rc::new(NonNativeFunc {
            name_id: def.name_id,
            closure: self.frame().to_inactive(),
            signature: self.compile_func_signature(&def.signature),
            body: def.body.clone(),
        }))
    }
    
    pub fn evaluate_func_call(&mut self, call: &ast::FuncCall, type_hint: &Type) -> Option<Value> {
        let func_type = Type::Function
            .option_if(call.func.is_coalescing());
        
        match self.evaluate_name(&call.func, &func_type)? {
            Value::Func(f) => {
                let bindings = f.bind_call(self, call)?;
                self.evaluate_func_call_with_bindings(f, bindings, type_hint, &call.range)
            },
            f if f.is_unit() => Some(Value::UNIT),
            _ => errors::ice_at("failed to coerce", call.func.range()),
        }
    }
    
    pub fn evaluate_func_call_with_bindings(&mut self, func: Func, bindings: ValueMap, type_hint: &Type, call_range: &SourceRange) -> Option<Value> {
        match func {
            Func::NonNative(ref f) => {
                let frame = f.closure.new_child_frame(bindings, func.clone(), call_range);
                self.evaluate_in_frame(frame, |_self| _self.evaluate_node(f.body.as_ref(), type_hint))
            },
            Func::Native(f, _) => {
                self.evaluate_native_func(f, bindings, call_range)
            },
            Func::Bound(f) => {
                self.evaluate_func_call_with_bindings(f.0.clone(), bindings, type_hint, call_range)
            },
        }
    }
}
