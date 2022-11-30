#[allow(missing_docs)]
/// Represents an error which occurs when loading a module.
pub enum ModuleError {
    IOError(std::io::Error),
    CircularImport,
    PreviousError,
}

impl std::fmt::Display for ModuleError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ModuleError::IOError(e) => e.fmt(f),
            ModuleError::CircularImport => f.write_str("circular import detected"),
            ModuleError::PreviousError => f.write_str("previous error loading module"),
        }
    }
}
