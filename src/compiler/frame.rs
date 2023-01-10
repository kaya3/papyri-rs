use std::cell::RefCell;
use std::rc::Rc;

use crate::errors;
use crate::utils::{NameID, NameIDSet};
use crate::utils::sourcefile::SourceRange;
use super::base::Compiler;
use super::func::Func;
use super::value::{Value, Dict};

#[derive(Debug)]
pub(super) struct ActiveFrame {
    f: Rc<RefCell<Frame>>,
    call: Option<(Func, SourceRange)>,
}

#[derive(Debug, Clone)]
pub(super) struct InactiveFrame {
    f: Rc<RefCell<Frame>>,
}

#[derive(Debug)]
struct Frame {
    lexical_parent: Option<InactiveFrame>,
    locals: Dict,
    implicit: NameIDSet,
}

impl ActiveFrame {
    pub(super) fn new(lexical_parent: Option<InactiveFrame>, locals: Dict, call: Option<(Func, SourceRange)>) -> ActiveFrame {
        ActiveFrame {
            f: Rc::new(RefCell::new(Frame {
                lexical_parent,
                locals,
                implicit: NameIDSet::default(),
            })),
            call,
        }
    }
    
    pub(super) fn to_inactive(&self) -> InactiveFrame {
        InactiveFrame {f: self.f.clone()}
    }
    
    pub(super) fn get(&self, name_id: NameID, require_implicit: bool) -> Option<Value> {
        let f = self.f.as_ref().borrow();
        f.get(name_id, require_implicit)
    }
    
    /// Sets the value of a variable in this frame. Returns `true` if a variable
    /// of that name was already present.
    pub(super) fn set(&self, name_id: NameID, value: Value, implicit: bool) -> bool {
        let mut f = self.f.as_ref().borrow_mut();
        if implicit { f.implicit.insert(name_id); }
        f.set(name_id, value)
    }
    
    pub(super) fn unset_all(&self, name_ids: &[NameID]) {
        let mut f = self.f.as_ref().borrow_mut();
        for &name_id in name_ids.iter() {
            f.unset(name_id);
        }
    }
    
    pub(super) fn set_all_if_not_present(&self, name_ids: &[NameID], value: Value) {
        let mut f = self.f.as_ref().borrow_mut();
        for &name_id in name_ids.iter() {
            if !f.has(name_id) {
                f.set(name_id, value.clone());
            }
        }
    }
}

impl InactiveFrame {
    pub(super) fn new_child_frame(&self, locals: Dict, func: Func, call_range: SourceRange) -> ActiveFrame {
        ActiveFrame::new(
            Some(self.clone()),
            locals,
            Some((func, call_range)),
        )
    }
    
    pub(super) fn new_empty_child_frame(&self) -> ActiveFrame {
        ActiveFrame::new(
            Some(self.clone()),
            Dict::default(),
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
    
    fn has(&self, name_id: NameID) -> bool {
        self.locals.contains_key(&name_id)
    }
    
    /// Sets the value of a variable in this frame. Returns `true` if a variable
    /// of that name was already present.
    fn set(&mut self, name_id: NameID, value: Value) -> bool {
        self.locals.insert(name_id, value).is_some()
    }
    
    fn unset(&mut self, name_id: NameID) {
        self.locals.remove(&name_id);
        self.implicit.remove(&name_id);
    }
}

impl <'a> Compiler<'a> {
    pub(super) fn frame(&self) -> &ActiveFrame {
        self.call_stack.last().unwrap()
    }
    
    pub(super) fn stack_trace(&self) -> errors::StackTrace {
        self.call_stack.iter()
            .filter_map(|frame| frame.call.as_ref())
            .map(|&(ref func, call_range)| {
                let func_name = format!("@{}", self.get_name(func.name_id()));
                errors::DiagSourceRange::at_func(
                    self.get_source_file(call_range),
                    call_range,
                    &func_name,
                )
            })
            .collect()
    }
    
    pub(super) fn get_var(&mut self, name_id: NameID) -> errors::PapyriResult<Value> {
        self.frame()
            .get(name_id, false)
            .ok_or_else(|| {
                let name = self.get_name(name_id);
                errors::NameError::NoSuchVariable(name).into()
            })
    }
    
    pub(super) fn get_implicit(&mut self, name_id: NameID, default_value: Option<Value>) -> errors::PapyriResult<Value> {
        self.frame()
            .get(name_id, true)
            .or(default_value)
            .ok_or_else(|| {
                let name = self.get_name(name_id);
                if self.frame().get(name_id, false).is_some() {
                    errors::RuntimeError::ParamExistsButNotImplicit(name)
                } else {
                    errors::RuntimeError::ParamMissingImplicit(name)
                }.into()
            })
    }
    
    pub(super) fn set_var(&mut self, name_id: NameID, value: Value, implicit: bool, range: SourceRange) {
        if self.frame().set(name_id, value, implicit) {
            let name = self.get_name(name_id);
            self.report_static(errors::Warning::NameAlreadyDeclared(name), range);
        }
    }
    
    pub(super) fn evaluate_in_frame<T>(&mut self, frame: ActiveFrame, f: impl FnOnce(&mut Compiler) -> T) -> T {
        self.call_stack.push(frame);
        let result = f(self);
        self.call_stack.pop();
        result
    }
}
