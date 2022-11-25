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
pub use sink::{Diagnostics, PapyriError};

/// Reports an internal compiler error, indicating a bug or mistake in the
/// Papyri compiler. Use `ice_at` instead if the error corresponds with some
/// code in a Papyri source file.
pub fn ice(msg: &str) -> ! {
    panic!("Internal compiler error: {msg}");
}

/// Reports an internal compiler error, indicating a bug or mistake in the
/// Papyri compiler. Use `ice` instead if the error does not correspond with
/// any particular code in a Papyri source file.
pub fn ice_at(msg: &str, range: &crate::utils::SourceRange) -> ! {
    panic!("Internal compiler error: {msg} at {range:?}");
}
