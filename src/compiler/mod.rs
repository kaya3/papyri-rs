//! This module contains the backend of the Papyri compiler; it is responsible
//! for compiling an abstract syntax tree into HTML (or plain text).

mod compiler;
mod context;
mod frame;
mod func;
mod highlight;
mod highlight_papyri;
mod html;
mod matcher;
mod module_loader;
mod native;
mod render;
mod sequence;
mod tag;
mod types;
mod unique_id;
mod value;

pub use compiler::CompileResult;
pub use context::Context;
pub use html::HTML;
pub use types::Type;
pub use value::Value;
