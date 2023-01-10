use std::rc::Rc;

use crate::utils::sourcefile::{SourceRange, SourceFile};
use super::sink_base::{DiagnosticSink, Severity, StackTrace};
use super::module_error::ModuleError;
use super::runtime_error::{RuntimeError, NameError};
use super::syntax_error::SyntaxError;
use super::type_error::TypeError;
use super::warning::Warning;

#[allow(missing_docs)]
#[derive(Debug)]
/// Represents an error or warning which occurs while attempting to compile a
/// Papyri source file.
pub enum PapyriError {
    ModuleError(ModuleError),
    NameError(NameError),
    RuntimeError(RuntimeError),
    SyntaxError(SyntaxError),
    TypeError(TypeError),
    Warning(Warning),
    AlreadyReported,
}

impl PapyriError {
    fn severity(&self) -> Severity {
        match self {
            PapyriError::ModuleError(..) |
            PapyriError::NameError(..) |
            PapyriError::RuntimeError(..) |
            PapyriError::SyntaxError(..) |
            PapyriError::TypeError(..) => Severity::Error,
            PapyriError::Warning(..) => Severity::Warning,
            PapyriError::AlreadyReported => Severity::Debug,
        }
    }
}

/// Represents that a failure occurred but a diagnostic was already reported
/// for it.
pub struct AlreadyReported;

impl From<AlreadyReported> for PapyriError {
    fn from(_: AlreadyReported) -> PapyriError {
        PapyriError::AlreadyReported
    }
}
impl From<ModuleError> for PapyriError {
    fn from(e: ModuleError) -> PapyriError {
        PapyriError::ModuleError(e)
    }
}
impl From<NameError> for PapyriError {
    fn from(e: NameError) -> PapyriError {
        PapyriError::NameError(e)
    }
}
impl From<RuntimeError> for PapyriError {
    fn from(e: RuntimeError) -> PapyriError {
        PapyriError::RuntimeError(e)
    }
}
impl From<SyntaxError> for PapyriError {
    fn from(e: SyntaxError) -> PapyriError {
        PapyriError::SyntaxError(e)
    }
}
impl From<TypeError> for PapyriError {
    fn from(e: TypeError) -> PapyriError {
        PapyriError::TypeError(e)
    }
}
impl From<Warning> for PapyriError {
    fn from(e: Warning) -> PapyriError {
        PapyriError::Warning(e)
    }
}

impl std::fmt::Display for PapyriError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PapyriError::ModuleError(e) => write!(f, "Module error: {e}"),
            PapyriError::NameError(e) => write!(f, "Name error: {e}"),
            PapyriError::RuntimeError(e) => write!(f, "Runtime error: {e}"),
            PapyriError::SyntaxError(e) => write!(f, "Syntax error: {e}"),
            PapyriError::TypeError(e) => write!(f, "Type error: {e}"),
            PapyriError::Warning(e) => write!(f, "Warning: {e}"),
            PapyriError::AlreadyReported => f.write_str("Diagnostic already reported"),
        }
    }
}

/// A diagnostic sink for `PapyriError`s. It has convenience methods for
/// reporting each kind of error or warning.
pub type Diagnostics = DiagnosticSink<PapyriError>;

impl Diagnostics {
    /// Reports a static diagnostic in a Papyri source file (i.e. one with no
    /// stack trace).
    pub fn report_static<T: Into<PapyriError>>(&mut self, e: T, src: Rc<SourceFile>, range: SourceRange) {
        let e = e.into();
        self.add(e.severity(), e, src, range, None);
    }
    
    /// Reports a diagnostic in a Papyri source file, with an associated stack
    /// trace.
    pub fn report<T: Into<PapyriError>>(&mut self, e: T, trace: StackTrace, src: Rc<SourceFile>, range: SourceRange) {
        let e = e.into();
        self.add(e.severity(), e, src, range, Some(trace));
    }
}

