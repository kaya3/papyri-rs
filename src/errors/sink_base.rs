use std::fmt;

use crate::utils::{SourceRange, text};

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum Severity {
    Warning,
    Error,
}

impl fmt::Display for Severity {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Severity::Error => "Error",
            Severity::Warning => "Warning",
        })
    }
}

/// Holds information about an error or warning which has occurred during
/// compilation of a Papyri source file.
struct Diagnostic<T: fmt::Display> {
    severity: Severity,
    msg: T,
    range: SourceRange,
}

impl <T: fmt::Display> fmt::Display for Diagnostic<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}: {}\n    in \"{}\" at {}",
            self.severity,
            self.msg,
            self.range.src.path_str,
            self.range.str_start(),
        )
    }
}

/// Collects diagnostics during the compilation of a Papyri source file.
pub struct DiagnosticSink<T: fmt::Display> {
    v: Vec<Diagnostic<T>>,
    pub num_errors: usize,
    pub num_warnings: usize,
}

impl <T: fmt::Display> fmt::Debug for DiagnosticSink<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.summary())?;
        f.write_str("\n")?;
        for d in self.v.iter() {
            f.write_str(&d.to_string())?;
            f.write_str("\n")?;
        }
        Ok(())
    }
}

impl <T: fmt::Display> DiagnosticSink<T> {
    /// Creates a new empty collection of diagnostics.
    pub fn new() -> DiagnosticSink<T> {
        DiagnosticSink {
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
    
    /// Returns a string summarising the numbers of errors and warnings in this
    /// collection.
    pub fn summary(&self) -> String {
        let errors = self.num_errors;
        let warnings = self.num_warnings;
        if errors > 0 {
            format!(
                "failed, {errors} error{}, {warnings} warning{}",
                text::pluralise(errors),
                text::pluralise(warnings),
            )
        } else if warnings > 0 {
            format!(
                "OK, {warnings} warning{}",
                text::pluralise(warnings),
            )
        } else {
            "OK".to_string()
        }
    }
    
    /// Prints the diagnostics in this collection to stderr. If `ignore_warnings`
    /// is true, only errors are printed.
    pub fn print(&self, ignore_warnings: bool) {
        for diag in &self.v {
            if !ignore_warnings || diag.severity != Severity::Warning {
                eprintln!("{diag}");
            }
        }
    }
    
    pub fn add(&mut self, severity: Severity, msg: T, range: &SourceRange) {
        if severity == Severity::Warning {
            self.num_warnings += 1;
        } else {
            self.num_errors += 1;
        }
        self.v.push(Diagnostic {severity, msg, range: range.clone()});
    }
}
