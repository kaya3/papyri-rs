use super::range::SourceRange;

#[derive(Debug, Eq, PartialEq)]
pub enum DiagnosticKind {
    SyntaxError,
    TypeError,
    NameError,
    Error,
    Warning,
}

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

pub struct Diagnostics {
    v: Vec<Diagnostic>,
    pub num_errors: usize,
    pub num_warnings: usize,
}

impl Diagnostics {
    pub fn new() -> Diagnostics {
        Diagnostics {
            v: Vec::new(),
            num_errors: 0,
            num_warnings: 0,
        }
    }
    
    pub fn is_empty(&self) -> bool {
        self.num_errors == 0 && self.num_warnings == 0
    }
    
    pub fn clear(&mut self) {
        self.v.clear();
        self.num_errors = 0;
        self.num_warnings = 0;
    }
    
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
    
    pub fn syntax_error(&mut self, msg: &str, range: &SourceRange) {
        self.add(DiagnosticKind::SyntaxError, msg, range);
    }
    
    pub fn type_error(&mut self, msg: &str, range: &SourceRange) {
        self.add(DiagnosticKind::TypeError, msg, range);
    }
    
    pub fn name_error(&mut self, msg: &str, range: &SourceRange) {
        self.add(DiagnosticKind::NameError, msg, range);
    }
    
    pub fn error(&mut self, msg: &str, range: &SourceRange) {
        self.add(DiagnosticKind::Error, msg, range);
    }
    
    pub fn warning(&mut self, msg: &str, range: &SourceRange) {
        self.add(DiagnosticKind::Warning, msg, range);
    }
}

pub fn ice(msg: &str) -> ! {
    panic!("Internal compiler error: {}", msg);
}

pub fn ice_at(msg: &str, range: &SourceRange) -> ! {
    ice(&format!("{} at {:?}", msg, range));
}
