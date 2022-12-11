//! This module contains the frontend of the Papyri compiler; it is responsible
//! for parsing a Papyri source file into an abstract syntax tree.

pub mod ast;
mod func;
mod matcher;
mod base;
mod queue;
mod tag;
mod template;
pub mod text;
mod token;
mod tokenizer;
mod types;

pub use ast::AST;
pub use base::parse;
pub use token::{Token, TokenKind};
pub use tokenizer::tokenize;
