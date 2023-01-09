use std::rc::Rc;

use crate::errors;
use crate::parser::{ast, Type};
use crate::utils::NameID;
use crate::utils::sourcefile::SourceRange;
use super::base::Compiler;
use super::frame::InactiveFrame;
use super::native::NativeFunc;
use super::signature::{RcFuncSignature, PartialParams};
use super::value::{Value, Dict};

#[derive(Debug, Clone)]
pub enum Func {
    NonNative(Rc<NonNativeFunc>),
    Native(NativeFunc, NameID, RcFuncSignature),
    Bound(Rc<(Func, PartialParams)>),
}

impl Func {
    pub(super) fn name_id(&self) -> NameID {
        match self {
            Func::NonNative(f) => f.name_id,
            Func::Native(_, name_id, _) => *name_id,
            Func::Bound(f) => f.0.name_id(),
        }
    }
    
    pub(super) fn signature(&self) -> RcFuncSignature {
        match self {
            Func::NonNative(f) => f.signature.clone(),
            Func::Native(.., sig) => sig.clone(),
            Func::Bound(f) => f.0.signature(),
        }
    }
    
    fn get_partials(&self) -> PartialParams {
        match self {
            Func::Bound(f) => f.1.clone(),
            _ => PartialParams::new(),
        }
    }
    
    pub(super) fn bind_content(self, compiler: &mut Compiler, content_arg: Value) -> Result<Func, errors::PapyriError> {
        let mut binder = self.get_partials()
            .open(compiler, self.signature());
        
        binder.add_content_arg(content_arg)?;
        binder.close_into_bound_function(self)
    }
    
    pub(super) fn bind_positional(self, compiler: &mut Compiler, arg: Value) -> Result<Func, errors::PapyriError> {
        let mut binder = self.get_partials()
            .open(compiler, self.signature());
        
        binder.add_positional_arg(arg)?;
        binder.close_into_bound_function(self)
    }
    
    pub(super) fn bind_partial(self, compiler: &mut Compiler, positional_args: &[Value], named_args: &Dict, content_arg: Value) -> Result<Func, errors::PapyriError> {
        let mut binder = self.get_partials()
            .open(compiler, self.signature());
        
        for arg in positional_args.iter().cloned() {
            binder.add_positional_arg(arg)?;
        }
        for (&name_id, arg) in named_args.iter() {
            binder.add_named_arg(name_id, arg.clone())?;
        }
        if !content_arg.is_unit() {
            binder.add_content_arg(content_arg)?;
        }
        
        binder.close_into_bound_function(self)
    }
    
    pub(super) fn bind_synthetic_call(&self, compiler: &mut Compiler, bind_implicits: bool, content_value: Value) -> Result<Dict, errors::PapyriError> {
        let mut binder = self.get_partials()
            .open(compiler, self.signature());
        if bind_implicits {
            binder.bind_implicit_args()?;
        }
        binder.add_content_arg(content_value)?;
        binder.build()
    }
    
    fn bind_call(&self, compiler: &mut Compiler, call: &ast::FuncCall) -> Result<Dict, errors::PapyriError> {
        let mut binder = self.get_partials()
            .open(compiler, self.signature());
        
        for arg in call.args.iter() {
            binder.compile_arg(arg)?;
        }
        binder.bind_implicit_args()?;
        binder.compile_content_arg(&call.content)?;
        binder.build()
    }
}

#[derive(Debug)]
pub struct NonNativeFunc {
    name_id: NameID,
    closure: InactiveFrame,
    signature: RcFuncSignature,
    body: Rc<ast::Expr>,
}

impl <'a> Compiler<'a> {
    pub(super) fn compile_func_def(&mut self, def: &ast::FuncDef) -> Func {
        Func::NonNative(Rc::new(NonNativeFunc {
            name_id: def.name_id,
            closure: self.frame().to_inactive(),
            signature: self.compile_func_signature(&def.signature),
            body: def.body.clone(),
        }))
    }
    
    pub(super) fn evaluate_func_call(&mut self, call: &ast::FuncCall, type_hint: &Type) -> Result<Value, errors::AlreadyReported> {
        let func_type = Type::Function
            .option_if(call.func.is_coalescing());
        let Some(func) = self.evaluate_name(&call.func, &func_type)?
            .expect_convert::<Option<Func>>() else {
                return Ok(Value::UNIT);
            };
        
        let bindings = func.bind_call(self, call)
            .map_err(|e| self.report(e, call.range))?;
        self.evaluate_func_call_with_bindings(func, bindings, type_hint, call.range)
    }
    
    pub(super) fn evaluate_func_call_with_bindings(&mut self, func: Func, bindings: Dict, type_hint: &Type, call_range: SourceRange) -> Result<Value, errors::AlreadyReported> {
        match func {
            Func::NonNative(ref f) => {
                let frame = f.closure.new_child_frame(bindings, func.clone(), call_range);
                self.evaluate_in_frame(frame, |_self| _self.evaluate_node(f.body.as_ref(), type_hint))
            },
            Func::Native(f, ..) => {
                self.evaluate_native_func(f, bindings, call_range)
                    .and_then(|v| self.coerce(v, type_hint))
                    .map_err(|e| self.report(e, call_range))
            },
            Func::Bound(f) => {
                self.evaluate_func_call_with_bindings(f.0.clone(), bindings, type_hint, call_range)
            },
        }
    }
}
