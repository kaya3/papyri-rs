use super::range::SourceRange;

#[derive(Debug, Eq, PartialEq)]
pub enum DiagnosticKind {
    Warning,
    Error,
    NameError,
    RuntimeError,
    SyntaxError,
    TypeError,
}

/// Holds information about an error or warning which has occurred during
/// compilation of a Papyri source file.
struct Diagnostic {
    kind: DiagnosticKind,
    msg: String,
    range: SourceRange,
}

impl Diagnostic {
    pub fn to_string(&self) -> String {
        format!("{:?}: {}\n    in \"{}\" at {}", self.kind, self.msg, self.range.src.path_str, self.range.str_start())
    }
}

/// Collects diagnostics during the compilation of a Papyri source file.
pub struct Diagnostics {
    v: Vec<Diagnostic>,
    pub num_errors: usize,
    pub num_warnings: usize,
}

impl Diagnostics {
    /// Creates a new empty collection of diagnostics.
    pub fn new() -> Diagnostics {
        Diagnostics {
            v: Vec::new(),
            num_errors: 0,
            num_warnings: 0,
        }
    }
    
    /// Indicates whether the collection is empty (no errors and no warnings).
    pub fn is_empty(&self) -> bool {
        self.num_errors == 0 && self.num_warnings == 0
    }
    
    /// Clears the collection, making it empty.
    pub fn clear(&mut self) {
        self.v.clear();
        self.num_errors = 0;
        self.num_warnings = 0;
    }
    
    /// Prints the diagnostics in this collection to stderr. If `ignore_warnings`
    /// is true, only errors are printed.
    pub fn print(&self, ignore_warnings: bool) {
        for diag in &self.v {
            if !ignore_warnings || diag.kind != DiagnosticKind::Warning {
                eprintln!("{}", diag.to_string());
            }
        }
    }
    
    fn add(&mut self, kind: DiagnosticKind, msg: &str, range: &SourceRange) {
        if kind == DiagnosticKind::Warning {
            self.num_warnings += 1;
        } else {
            self.num_errors += 1;
        }
        self.v.push(Diagnostic {kind, msg: msg.to_string(), range: range.clone()});
    }
    
    /// Reports a syntax error in a Papyri source file.
    pub fn syntax_error(&mut self, msg: &str, range: &SourceRange) {
        self.add(DiagnosticKind::SyntaxError, msg, range);
    }
    
    /// Reports a type error in a Papyri source file.
    pub fn type_error(&mut self, msg: &str, range: &SourceRange) {
        self.add(DiagnosticKind::TypeError, msg, range);
    }
    
    /// Reports a name error which occurs during compilation of a Papyri source
    /// file. A name error indicates that a variable or parameter of some name
    /// has not been declared.
    pub fn name_error(&mut self, msg: &str, range: &SourceRange) {
        self.add(DiagnosticKind::NameError, msg, range);
    }
    
    /// Reports a runtime error by adding it to the collection. This occurs
    /// only when the Papyri program itself uses the native `@raise` function
    /// to raise an error.
    pub fn runtime_error(&mut self, msg: &str, range: &SourceRange) {
        self.add(DiagnosticKind::RuntimeError, msg, range);
    }
    
    /// Reports a general kind of error which does not fall into any of the
    /// other defined categories.
    pub fn error(&mut self, msg: &str, range: &SourceRange) {
        self.add(DiagnosticKind::Error, msg, range);
    }
    
    /// Reports a warning.
    pub fn warning(&mut self, msg: &str, range: &SourceRange) {
        self.add(DiagnosticKind::Warning, msg, range);
    }
}

/// Reports an internal compiler error, indicating a bug or mistake in the
/// Papyri compiler. Use `ice_at` instead if the error corresponds with some
/// code in a Papyri source file.
pub fn ice(msg: &str) -> ! {
    panic!("Internal compiler error: {}", msg);
}

/// Reports an internal compiler error, indicating a bug or mistake in the
/// Papyri compiler. Use `ice` instead if the error does not correspond with
/// any particular code in a Papyri source file.
pub fn ice_at(msg: &str, range: &SourceRange) -> ! {
    ice(&format!("{} at {:?}", msg, range));
}
