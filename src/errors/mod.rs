//! This module contains type definitions for errors and warnings which can
//! occur during compilation of a Papyri source file.

mod module_error;
mod runtime_error;
mod syntax_error;
mod type_error;
mod warning;
pub use module_error::ModuleError;
pub use runtime_error::{NameError, RuntimeError};
pub use syntax_error::SyntaxError;
pub use type_error::TypeError;
pub use warning::Warning;

mod sink;
mod sink_base;
pub use sink_base::{StackTrace, ReportingLevel, DiagSourceRange};
pub use sink::{Diagnostics, PapyriError, AlreadyReported};

/// Reports an internal compiler error, indicating a bug or mistake in the
/// Papyri compiler. Use `ice_at` instead if the error corresponds with some
/// code in a Papyri source file.
pub fn ice(msg: &str) -> ! {
    panic!("Internal compiler error: {msg}");
}

/// Reports an internal compiler error, indicating a bug or mistake in the
/// Papyri compiler. Use `ice` instead if the error does not correspond with
/// any particular code in a Papyri source file.
pub fn ice_at(msg: &str, src_file: &crate::utils::sourcefile::SourceFile, range: crate::utils::sourcefile::SourceRange) -> ! {
    let (line, col) = src_file.index_to_line_col(range.start);
    panic!("Internal compiler error: {msg}\n    File \"{}\", line {line}, col {col}", src_file.path_str);
}
