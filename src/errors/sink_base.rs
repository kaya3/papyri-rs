use std::fmt;

use crate::utils::{SourceRange, text};

#[derive(Debug, Clone, Copy, Eq, PartialEq, PartialOrd, Ord)]
pub enum Severity {Warning, Error}

#[derive(Debug, Clone, Copy, Eq, PartialEq, PartialOrd, Ord)]
pub enum ReportingLevel {All, Warning, Error, IgnoreAll}

impl ReportingLevel {
    fn should_report(self, severity: Severity) -> bool {
        match self {
            ReportingLevel::All => true,
            ReportingLevel::Warning => severity >= Severity::Warning,
            ReportingLevel::Error => severity >= Severity::Error,
            ReportingLevel::IgnoreAll => false,
        }
    }
}

impl fmt::Display for Severity {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Severity::Warning => "Warning",
            Severity::Error => "Error",
        })
    }
}

pub type StackTrace = Box<[(String, SourceRange)]>;

/// Holds information about an error or warning which has occurred during
/// compilation of a Papyri source file.
struct Diagnostic<T: fmt::Display> {
    msg: T,
    range: SourceRange,
    trace: Option<StackTrace>,
}

fn write_trace_line(f: &mut fmt::Formatter<'_>, range: &SourceRange, func_name: Option<&str>) -> fmt::Result {
    write!(f, "    File \"{}\", {}", range.src.path_str, range.str_start())?;
    if let Some(func_name) = func_name {
        write!(f, ", in {func_name}")?;
    }
    writeln!(f)
}

impl <T: fmt::Display> fmt::Display for Diagnostic<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(trace) = &self.trace {
            writeln!(f, "Traceback (most recent call last):")?;
            let mut in_func = None;
            for (func_name, call_range) in trace.iter() {
                write_trace_line(f, call_range, in_func)?;
                in_func = Some(func_name.as_str());
            }
            write_trace_line(f, &self.range, in_func)?;
            writeln!(f, "{}", self.msg)?;
        } else {
            writeln!(f, "{}", self.msg)?;
            write_trace_line(f, &self.range, None)?;
        }
        Ok(())
    }
}

/// Collects diagnostics during the compilation of a Papyri source file.
pub struct DiagnosticSink<T: fmt::Display> {
    v: Vec<Diagnostic<T>>,
    pub num_errors: usize,
    pub num_warnings: usize,
    reporting_level: ReportingLevel,
}

impl <T: fmt::Display> fmt::Debug for DiagnosticSink<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for d in self.v.iter() {
            writeln!(f, "{}", d)?;
        }
        writeln!(f, "{}", &self.summary())
    }
}

impl <T: fmt::Display> DiagnosticSink<T> {
    /// Creates a new empty collection of diagnostics.
    pub fn new(reporting_level: ReportingLevel) -> DiagnosticSink<T> {
        DiagnosticSink {
            v: Vec::new(),
            num_errors: 0,
            num_warnings: 0,
            reporting_level,
        }
    }
    
    /// Indicates whether the collection is empty (no errors and no warnings).
    pub fn is_empty(&self) -> bool {
        self.num_errors == 0 && self.num_warnings == 0
    }
    
    /// Indicates whether the collection has any diagnostics matching the given
    /// predicate. Used for tests to assert that a diagnostic is reported.
    pub fn has_any(&self, predicate: impl Fn(&T) -> bool) -> bool {
        self.v.iter().any(|d| predicate(&d.msg))
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
    pub fn print(&self) {
        for diag in self.v.iter() {
            eprintln!("{diag}");
        }
    }
    
    pub fn add(&mut self, severity: Severity, msg: T, range: &SourceRange, trace: Option<StackTrace>) {
        if severity == Severity::Warning {
            self.num_warnings += 1;
        } else {
            self.num_errors += 1;
        }
        if self.reporting_level.should_report(severity) {
            self.v.push(Diagnostic {msg, range: range.clone(), trace});
        }
    }
}
