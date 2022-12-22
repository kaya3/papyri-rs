
#![forbid(unsafe_code)]
#![warn(missing_docs)]

//! This crate provides a compiler for the [Papyri markup language](https://kaya3.github.io/papyri).

pub mod compiler;
pub mod errors;
pub mod parser;
pub mod utils;

/// Compiles Papyri source given as a string into HTML, as a string. If any
/// errors or warnings occur during compilation, the diagnostics are returned
/// instead.
pub fn compile_str(src: &str) -> Result<String, errors::Diagnostics> {
    let mut ctx = compiler::Context::new(errors::ReportingLevel::Warning, None);
    let src = ctx.source_files.load_synthetic("string", src);
    let result = ctx.compile(src);
    
    if ctx.diagnostics.is_empty() {
        let mut out = Vec::new();
        ctx.render(&result.out, true, &mut out)
            .unwrap();
        
        let out = String::from_utf8(out).unwrap();
        Ok(out)
    } else {
        Err(ctx.diagnostics)
    }
}
