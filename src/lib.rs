
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
    let mut loader = compiler::ModuleLoader::new();
    let mut diagnostics = errors::Diagnostics::new(errors::ReportingLevel::Warning);
    let result = compiler::compile(
        utils::SourceFile::synthetic("string", src),
        &mut loader,
        &mut diagnostics,
        None,
    );
    
    if diagnostics.is_empty() {
        let mut out = Vec::new();
        compiler::Renderer::new(&loader.string_pool, true, &mut out)
            .render(&result.out)
            .unwrap();
        
        let out = String::from_utf8(out).unwrap();
        Ok(out)
    } else {
        Err(diagnostics)
    }
}
