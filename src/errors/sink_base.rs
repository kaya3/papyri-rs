use std::rc::Rc;

use crate::utils::sourcefile::{SourceRange, SourceFile};
use crate::utils::text;

#[derive(Debug, Clone, Copy, Eq, PartialEq, PartialOrd, Ord)]
#[allow(missing_docs)]
/// The severity of a diagnostic which occurs while attempting to compiling a
/// Papyri source file.
pub enum Severity {Debug, Warning, Error}

#[derive(Debug, Clone, Copy, Eq, PartialEq, PartialOrd, Ord)]
#[allow(missing_docs)]
/// Controls which severity of diagnostics are reported by the Papyri compiler.
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

impl std::fmt::Display for Severity {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Severity::Debug => "Debug",
            Severity::Warning => "Warning",
            Severity::Error => "Error",
        })
    }
}


#[derive(Clone)]
/// Represents a span of code in a source file, while also holding a reference
/// to the source file itself.
pub struct DiagSourceRange {
    src: Rc<SourceFile>,
    start: u32,
    //end: u32,
    in_func: Option<Box<str>>,
}

impl DiagSourceRange {
    pub(crate) fn at(src: Rc<SourceFile>, range: SourceRange) -> DiagSourceRange {
        DiagSourceRange {
            src,
            start: range.start,
            //end: range.end,
            in_func: None,
        }
    }
    
    pub(crate) fn at_func(src: Rc<SourceFile>, range: SourceRange, func_name: &str) -> DiagSourceRange {
        DiagSourceRange {
            src,
            start: range.start,
            //end: range.end,
            in_func: Some(Box::from(func_name)),
        }
    }
}

/// Represents a stack trace, which is associated with a diagnostic.
/// 
/// Each line in the trace is a pair of (name, pos), where a function with that
/// name was called at that source position. The source position for the direct
/// cause of the diagnostic is held in the `Diagnostic` struct. The pairs are
/// in order from least recent to most recent.
pub type StackTrace = Box<[DiagSourceRange]>;

/// Holds information about an error or warning which has occurred during
/// compilation of a Papyri source file.
struct Diagnostic<T: std::fmt::Display> {
    msg: T,
    range: DiagSourceRange,
    trace: Option<StackTrace>,
}

impl <T: std::fmt::Display> std::fmt::Display for Diagnostic<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fn write_trace_line(f: &mut std::fmt::Formatter<'_>, range: &DiagSourceRange, func_name: Option<&str>) -> std::fmt::Result {
            let (line, col) = range.src.index_to_line_col(range.start);
            write!(f, "    File \"{}\", line {line}, col {col}", range.src.path_str)?;
            if let Some(func_name) = func_name {
                write!(f, ", in {func_name}")?;
            }
            writeln!(f)
        }
        
        if let Some(trace) = &self.trace {
            writeln!(f, "Traceback (most recent call last):")?;
            let mut in_func = None;
            for call_range in trace.iter() {
                write_trace_line(f, call_range, in_func)?;
                in_func = call_range.in_func.as_ref().map(Box::as_ref);
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
pub struct DiagnosticSink<T: std::fmt::Display> {
    v: Vec<Diagnostic<T>>,
    pub num_errors: u32,
    pub num_warnings: u32,
    reporting_level: ReportingLevel,
}

impl <T: std::fmt::Display> DiagnosticSink<T> {
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
    
    /// Prints the diagnostics in this collection to stderr.
    pub fn print_to_stderr(&self) {
        for diag in self.v.iter() {
            eprintln!("{}", diag);
        }
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
    
    /// Adds a new diagnostic to this collection. The diagnostic is not added
    /// if its severity is lower than the specified reporting level.
    pub fn add(&mut self, severity: Severity, msg: T, source_file: Rc<SourceFile>, range: SourceRange, trace: Option<StackTrace>) {
        match severity {
            Severity::Warning => self.num_warnings += 1,
            Severity::Error => self.num_errors += 1,
            Severity::Debug => {},
        }
        if self.reporting_level.should_report(severity) {
            let range = DiagSourceRange::at(source_file, range);
            self.v.push(Diagnostic {msg, range, trace});
        }
    }
}

impl <T: std::fmt::Display> std::fmt::Debug for DiagnosticSink<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for diag in self.v.iter() {
            std::fmt::Display::fmt(diag, f)?;
        }
        writeln!(f, "{}", self.summary())
    }
}