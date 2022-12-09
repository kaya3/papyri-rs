use std::cell::RefCell;
use std::rc::Rc;
use indexmap::IndexSet;

use crate::errors::{NameError, RuntimeError, Warning, StackTrace, RuntimeWarning};
use crate::utils::{SourceRange, NameID};
use super::compiler::Compiler;
use super::func::Func;
use super::value::{Value, ValueMap};

#[derive(Debug)]
pub struct ActiveFrame {
    f: Rc<RefCell<Frame>>,
    call: Option<(Func, SourceRange)>,
}

#[derive(Debug, Clone)]
pub struct InactiveFrame {
    f: Rc<RefCell<Frame>>,
}

#[derive(Debug)]
struct Frame {
    lexical_parent: Option<InactiveFrame>,
    locals: ValueMap,
    implicit: IndexSet<NameID>,
}

impl ActiveFrame {
    pub fn new(lexical_parent: Option<InactiveFrame>, locals: ValueMap, call: Option<(Func, SourceRange)>) -> ActiveFrame {
        ActiveFrame {
            f: Rc::new(RefCell::new(Frame {
                lexical_parent,
                locals,
                implicit: IndexSet::new(),
            })),
            call,
        }
    }
    
    pub fn to_inactive(&self) -> InactiveFrame {
        InactiveFrame {f: self.f.clone()}
    }
    
    pub fn get(&self, name_id: NameID, require_implicit: bool) -> Option<Value> {
        let f = self.f.as_ref().borrow();
        f.get(name_id, require_implicit)
    }
    
    /// Sets the value of a variable in this frame. Returns `true` if a variable
    /// of that name was already present.
    pub fn set(&self, name_id: NameID, value: Value, implicit: bool) -> bool {
        let mut f = self.f.as_ref().borrow_mut();
        if implicit { f.implicit.insert(name_id); }
        f.set(name_id, value)
    }
}

impl InactiveFrame {
    pub fn new_child_frame(&self, locals: ValueMap, func: Func, call_range: &SourceRange) -> ActiveFrame {
        ActiveFrame::new(
            Some(self.clone()),
            locals,
            Some((func, call_range.clone())),
        )
    }
    
    pub fn new_empty_child_frame(&self) -> ActiveFrame {
        ActiveFrame::new(
            Some(self.clone()),
            ValueMap::new(),
            None,
        )
    }
}

impl From<ActiveFrame> for InactiveFrame {
    fn from(frame: ActiveFrame) -> InactiveFrame {
        InactiveFrame {f: frame.f}
    }
}

impl Frame {
    fn get(&self, name_id: NameID, require_implicit: bool) -> Option<Value> {
        let r = (!require_implicit || self.implicit.contains(&name_id))
            .then(|| self.locals.get(&name_id))
            .flatten();
        if let Some(r) = r {
            Some(r.clone())
        } else {
            self.lexical_parent.as_ref()?.f.borrow().get(name_id, require_implicit)
        }
    }
    
    /// Sets the value of a variable in this frame. Returns `true` if a variable
    /// of that name was already present.
    fn set(&mut self, name_id: NameID, value: Value) -> bool {
        self.locals.insert(name_id, value).is_some()
    }
}

impl <'a> Compiler<'a> {
    pub fn frame(&self) -> &ActiveFrame {
        self.call_stack.last().unwrap()
    }
    
    pub fn stack_trace(&self) -> StackTrace {
        self.call_stack.iter()
            .filter_map(|frame| frame.call.as_ref())
            .map(|(func, call_range)| {
                let func_name = format!("@{}", self.get_name(func.name_id()));
                (func_name, call_range.clone())
            })
            .collect()
    }
    
    pub fn get_var(&mut self, name_id: NameID, range: &SourceRange) -> Option<Value> {
        let r = self.frame().get(name_id, false);
        if r.is_none() {
            let name = self.get_name(name_id).to_string();
            self.name_error(NameError::NoSuchVariable(name), range);
        }
        r
    }
    
    pub fn get_implicit(&mut self, name_id: NameID, default_value: Option<Value>, range: &SourceRange) -> Option<Value> {
        let r = self.frame().get(name_id, true);
        if r.is_none() {
            if default_value.is_none() {
                let name = self.get_name(name_id).to_string();
                self.runtime_error(RuntimeError::ParamMissingImplicit(name), range);
            }
            if self.frame().get(name_id, false).is_some() {
                let name = self.get_name(name_id).to_string();
                self.runtime_warning(RuntimeWarning::NameNotImplicit(name), range);
            }
        }
        r.or(default_value)
    }
    
    pub fn set_var(&mut self, name_id: NameID, value: Value, implicit: bool, range: &SourceRange) {
        if self.frame().set(name_id, value, implicit) {
            let name = self.get_name(name_id).to_string();
            self.warning(Warning::NameAlreadyDeclared(name), range);
        }
    }
    
    pub fn set_vars(&mut self, dict: &ValueMap, implicit: bool, range: &SourceRange) {
        for (&k, v) in dict {
            self.set_var(k, v.clone(), implicit, range);
        }
    }
    
    pub fn evaluate_in_frame<T>(&mut self, frame: ActiveFrame, f: impl FnOnce(&mut Compiler) -> T) -> T {
        self.call_stack.push(frame);
        let result = f(self);
        self.call_stack.pop();
        result
    }
}
