use crate::utils::SourceRange;
use super::sink_base::{DiagnosticSink, Severity};
use super::module_error::ModuleError;
use super::runtime_error::{RuntimeError, NameError};
use super::syntax_error::SyntaxError;
use super::type_error::TypeError;
use super::warning::Warning;

pub enum PapyriError {
    ModuleError(std::path::PathBuf, ModuleError),
    NameError(NameError),
    RuntimeError(RuntimeError),
    SyntaxError(SyntaxError),
    TypeError(TypeError),
    Warning(Warning),
}

impl std::fmt::Display for PapyriError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PapyriError::ModuleError(path, e) => write!(f, "Module error: {e} in \"{}\"", path.to_string_lossy()),
            PapyriError::NameError(e) => write!(f, "Name error: {e}"),
            PapyriError::RuntimeError(e) => write!(f, "Runtime error: {e}"),
            PapyriError::SyntaxError(e) => write!(f, "Syntax error: {e}"),
            PapyriError::TypeError(e) => write!(f, "Type error: {e}"),
            PapyriError::Warning(e) => write!(f, "Warning: {e}"),
        }
    }
}

pub type Diagnostics = DiagnosticSink<PapyriError>;

impl Diagnostics {
    /// Reports a syntax error in a Papyri source file.
    pub fn syntax_error(&mut self, e: SyntaxError, range: &SourceRange) {
        self.add(Severity::Error, PapyriError::SyntaxError(e), range);
    }
    
    /// Reports a type error in a Papyri source file.
    pub fn type_error(&mut self, e: TypeError, range: &SourceRange) {
        self.add(Severity::Error, PapyriError::TypeError(e), range);
    }
    
    /// Reports an error which occurs while importing a Papyri module.
    pub fn module_error(&mut self, path: std::path::PathBuf, e: ModuleError, range: &SourceRange) {
        self.add(Severity::Error, PapyriError::ModuleError(path, e), range);
    }
    
    /// Reports a name error which occurs during compilation of a Papyri source
    /// file. A name error indicates that a variable or parameter of some name
    /// has not been declared.
    pub fn name_error(&mut self, e: NameError, range: &SourceRange) {
        self.add(Severity::Error, PapyriError::NameError(e), range);
    }
    
    /// Reports a runtime error by adding it to the collection. This occurs
    /// only when the Papyri program itself uses the native `@raise` function
    /// to raise an error.
    pub fn runtime_error(&mut self, e: RuntimeError, range: &SourceRange) {
        self.add(Severity::Error, PapyriError::RuntimeError(e), range);
    }
    
    /// Reports a warning.
    pub fn warning(&mut self, e: Warning, range: &SourceRange) {
        self.add(Severity::Warning, PapyriError::Warning(e), range);
    }
}

