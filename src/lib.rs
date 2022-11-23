pub mod compiler;
pub mod parser;
pub mod utils;

pub fn compile_str(src: &str) -> Result<String, utils::Diagnostics> {
    let mut loader = compiler::ModuleLoader::new();
    let mut diagnostics = utils::Diagnostics::new();
    let result = compiler::compile(
        utils::SourceFile::synthetic("<string>", src),
        &mut loader,
        &mut diagnostics,
    );
    
    if diagnostics.is_empty() {
        let mut out = Vec::new();
        result.out.render_to(&mut out, &mut loader.string_pool, true).unwrap();
        let out = String::from_utf8(out).unwrap();
        Ok(out)
    } else {
        Err(diagnostics)
    }
}
