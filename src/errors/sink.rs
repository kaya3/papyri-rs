use crate::utils::SourceRange;
use super::sink_base::{DiagnosticSink, Severity, StackTrace};
use super::module_error::ModuleError;
use super::runtime_error::{RuntimeError, NameError};
use super::syntax_error::SyntaxError;
use super::type_error::TypeError;
use super::warning::{Warning, RuntimeWarning};

#[allow(missing_docs)]
/// Represents an error or warning which occurs while attempting to compile a
/// Papyri source file.
pub enum PapyriError {
    ModuleError(Box<std::path::Path>, ModuleError),
    NameError(NameError),
    RuntimeError(RuntimeError),
    SyntaxError(SyntaxError),
    TypeError(TypeError),
    Warning(Warning),
    RuntimeWarning(RuntimeWarning),
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
            PapyriError::RuntimeWarning(e) => write!(f, "Warning: {e}"),
        }
    }
}

/// A diagnostic sink for `PapyriError`s. It has convenience methods for
/// reporting each kind of error or warning.
pub type Diagnostics = DiagnosticSink<PapyriError>;

impl Diagnostics {
    /// Reports a syntax error in a Papyri source file.
    pub fn syntax_error(&mut self, e: SyntaxError, range: &SourceRange) {
        self.add(Severity::Error, PapyriError::SyntaxError(e), range, None);
    }
    
    /// Reports a type error in a Papyri source file.
    pub fn type_error(&mut self, e: TypeError, range: &SourceRange) {
        self.add(Severity::Error, PapyriError::TypeError(e), range, None);
    }
    
    /// Reports an error which occurs while loading a Papyri module.
    pub fn module_error<P: AsRef<std::path::Path>>(&mut self, path: P, e: ModuleError, range: &SourceRange) {
        let path = Box::from(path.as_ref());
        self.add(Severity::Error, PapyriError::ModuleError(path, e), range, None);
    }
    
    /// Reports a name error which occurs during compilation of a Papyri source
    /// file. A name error indicates that a variable or parameter of some name
    /// has not been declared.
    pub fn name_error(&mut self, e: NameError, range: &SourceRange) {
        self.add(Severity::Error, PapyriError::NameError(e), range, None);
    }
    
    /// Reports a runtime error by adding it to the collection. This occurs
    /// only when the Papyri program itself uses the native `@raise` function
    /// to raise an error.
    pub fn runtime_error(&mut self, e: RuntimeError, trace: StackTrace, range: &SourceRange) {
        self.add(Severity::Error, PapyriError::RuntimeError(e), range, Some(trace));
    }
    
    /// Reports a warning.
    pub fn warning(&mut self, e: Warning, range: &SourceRange) {
        self.add(Severity::Warning, PapyriError::Warning(e), range, None);
    }
    
    /// Reports a warning with a stack trace.
    pub fn runtime_warning(&mut self, e: RuntimeWarning, trace: StackTrace, range: &SourceRange) {
        self.add(Severity::Warning, PapyriError::RuntimeWarning(e), range, Some(trace));
    }
}

