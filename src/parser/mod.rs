//! This module contains the frontend of the Papyri compiler; it is responsible
//! for parsing a Papyri source file into an abstract syntax tree.

pub(crate) mod ast;
mod err;
mod func;
mod matcher;
mod base;
mod queue;
mod tag;
mod template;
pub(crate) mod text;
pub mod token;
mod tokenizer;
mod types;

pub(crate) use ast::AST;
pub use base::parse;
pub use tokenizer::tokenize;
