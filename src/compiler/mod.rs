//! This module contains the backend of the Papyri compiler; it is responsible
//! for compiling an abstract syntax tree into HTML (or plain text).

mod base;
mod context;
mod exports;
mod frame;
mod func;
mod highlight;
mod highlight_papyri;
mod html;
mod html_parse;
mod matcher;
mod module_loader;
mod names;
mod native;
mod native_gen;
mod regex_value;
mod render;
mod sequence;
mod signature;
mod tag;
mod types;
mod value;
mod value_convert;

pub use base::CompileResult;
pub use context::Context;
pub use html::HTML;
pub use value::Value;
