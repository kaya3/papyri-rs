#[derive(Debug)]
#[allow(missing_docs)]
/// Represents an error which occurs when loading a module.
pub enum ModuleError {
    IOError(Box<std::path::Path>, std::io::Error),
    CircularImport(Box<std::path::Path>),
    PreviousError(Box<std::path::Path>),
}

impl std::fmt::Display for ModuleError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let path = match self {
            ModuleError::IOError(path, e) => { e.fmt(f)?; path },
            ModuleError::CircularImport(path) => { f.write_str("circular import detected")?; path },
            ModuleError::PreviousError(path) => { f.write_str("previous error")?; path },
        };
        write!(f, " in \"{}\"", path.to_string_lossy())
    }
}
