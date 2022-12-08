use crate::errors;
use crate::parser::ast;
use crate::utils::{NameID, SourceRange};
use super::compiler::Compiler;
use super::types::Type;
use super::value::Value;

impl <'a> Compiler<'a> {
    pub fn export(&mut self, name_id: NameID, value: Value, range: &SourceRange) {
        if self.exports.insert(name_id, value).is_some() {
            let name = self.get_name(name_id).to_string();
            self.warning(errors::Warning::NameAlreadyExported(name), range);
        }
    }
    
    pub fn compile_export(&mut self, e: &ast::Export) {
        match e {
            ast::Export::Names(_, vars) => {
                for &(name_id, ref arg) in vars.iter() {
                    if let Some(v) = self.evaluate_node(arg, &Type::AnyValue) {
                        self.export(name_id, v, arg.range());
                    }
                }
            },
            ast::Export::LetIn(_, let_in) => {
                self.evaluate_let_in(let_in, &Type::Unit, true);
            },
            ast::Export::FuncDef(_, func_def) => {
                let f = self.compile_func_def(func_def);
                let name_id = f.name_id();
                let range = &func_def.signature.range;
                if name_id.is_anonymous() {
                    self.warning(errors::Warning::AnonymousFunctionNotExpected, range);
                } else {
                    let f = Value::from(f);
                    self.export(name_id, f.clone(), range);
                    self.set_var(name_id, f, false, range);
                }
            },
        }
    }
}
