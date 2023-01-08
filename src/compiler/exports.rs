use crate::errors;
use crate::parser::{ast, Type};
use crate::utils::NameID;
use crate::utils::sourcefile::SourceRange;
use super::base::Compiler;
use super::value::Value;

impl <'a> Compiler<'a> {
    pub(super) fn export(&mut self, name_id: NameID, value: Value, range: SourceRange) {
        if self.exports.insert(name_id, value).is_some() {
            let name = self.get_name(name_id);
            self.report_static(errors::Warning::NameAlreadyExported(name), range);
        }
    }
    
    pub(super) fn compile_export(&mut self, e: &ast::Export) -> Result<(), errors::AlreadyReported> {
        match e {
            ast::Export::Names(_, vars) => {
                for &(name_id, ref arg) in vars.iter() {
                    let v = self.evaluate_node(arg, &Type::Any)?;
                    self.export(name_id, v, arg.range());
                }
            },
            ast::Export::LetIn(_, let_in) => {
                self.evaluate_let_in(let_in, &Type::Unit, true)?;
            },
            ast::Export::FuncDef(_, func_def) => {
                let f = self.compile_func_def(func_def);
                let name_id = f.name_id();
                let f = Value::from(f);
                let range = func_def.signature.range;
                self.export(name_id, f.clone(), range);
                self.set_var(name_id, f, false, range);
            },
        }
        Ok(())
    }
}
