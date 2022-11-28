
#![forbid(unsafe_code)]

pub mod compiler;
pub mod config;
pub mod errors;
pub mod parser;
pub mod utils;

pub fn compile_str(src: &str) -> Result<String, errors::Diagnostics> {
    let mut loader = compiler::ModuleLoader::new();
    let mut diagnostics = errors::Diagnostics::new(errors::ReportingLevel::Warning);
    let result = compiler::compile(
        utils::SourceFile::synthetic("<string>", src),
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
