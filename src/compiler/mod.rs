//! This module contains the backend of the Papyri compiler; it is responsible
//! for compiling an abstract syntax tree into HTML (or plain text).

mod compiler;
mod context;
mod frame;
mod func;
mod highlight;
mod highlight_papyri;
mod html;
mod loader;
mod matcher;
mod native;
mod render;
mod sequence;
mod tag;
mod types;
mod value;

pub use compiler::{CompileResult, compile};
pub use context::Context;
pub use html::HTML;
pub use loader::ModuleLoader;
pub use render::Renderer;
pub use types::Type;
pub use value::Value;
