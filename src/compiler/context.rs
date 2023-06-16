use std::rc::Rc;

use crate::errors;
use crate::utils::{OutFiles, NameID, StringPool, text};
use crate::utils::sourcefile::{SourceRange, SourceFileCache, SourceFile};
use super::base::Compiler;
use super::frame::InactiveFrame;
use super::html::HTML;
use super::module_loader::ModuleCache;
use super::native::NativeDefs;
use super::value::RcStr;

/// Holds the context for a compilation job.
pub struct Context {
    /// The source files loaded in this context.
    pub(crate) source_files: SourceFileCache,
    
    /// The diagnostic collector for this compiler context.
    pub diagnostics: errors::Diagnostics,
    
    /// The module cache for this compiler context.
    pub(super) module_cache: ModuleCache,
    
    /// A cache containing the native functions.
    pub(super) natives: NativeDefs,
    
    /// A stack frame containing the native functions.
    pub(super) natives_frame: InactiveFrame,
    
    /// The pool of interned names for this compiler context.
    pub(super) string_pool: StringPool,
    
    /// The unique ID generator for this compiler context.
    pub(super) unique_ids: text::UniqueIDGenerator,
    
    /// The output files collector for this compiler context, if it has one.
    pub out_files: Option<OutFiles<HTML>>,
    
    /// An HTTP client used for net requests, if enabled.
    pub http_client: Option<reqwest::blocking::Client>,
}

impl Context {
    /// Creates a new compiler context. This includes compiling the standard
    /// library.
    pub fn new(reporting_level: errors::ReportingLevel, out_dir: Option<&std::path::Path>, http_client: Option<reqwest::blocking::Client>) -> Context {
        let natives = NativeDefs::build();
        let natives_frame = natives.to_frame().to_inactive();
        let mut ctx = Context {
            source_files: SourceFileCache::new(),
            string_pool: StringPool::new(),
            diagnostics: errors::Diagnostics::new(reporting_level),
            module_cache: ModuleCache::new(),
            natives,
            natives_frame,
            unique_ids: text::UniqueIDGenerator::new(),
            out_files: out_dir.map(OutFiles::new),
            http_client,
        };
        ctx.compile_stdlib();
        ctx
    }
    
    /// Adds an output file to this context's collector. The operation may fail
    /// if this context has no output file collector, or if the path is not
    /// within the output directory.
    pub(super) fn push_out_file(&mut self, path: RcStr, content: HTML) -> errors::PapyriResult {
        let Some(sink) = self.out_files.as_mut() else {
            let e = errors::RuntimeError::WriteFileNotAllowed;
            return Err(e.into());
        };
        if sink.try_push(path.as_ref(), content) {
            Ok(())
        } else {
            let e = errors::RuntimeError::PathNotInOutDir(path);
            Err(e.into())
        }
    }
    
    /// Clears any state from the previous compile job. Any `out_files` must
    /// already have been handled before calling this method.
    pub fn reset(&mut self) {
        self.diagnostics.clear();
        self.unique_ids.clear();
        if matches!(&self.out_files, Some(o) if !o.is_empty()) {
            errors::ice("Output files were not consumed");
        }
    }
}

impl <'a> Compiler<'a> {
    pub(super) fn get_source_file(&self, range: SourceRange) -> Rc<SourceFile> {
        self.ctx.source_files.get(range.src_id)
    }
    
    pub(crate) fn get_source_str(&self, range: SourceRange) -> &str {
        self.ctx.source_files.get_str(range)
    }
    
    pub(super) fn ice_at(&self, msg: &str, range: SourceRange) -> ! {
        errors::ice_at(msg, self.get_source_file(range).as_ref(), range)
    }
    
    pub(super) fn report_static<T: Into<errors::PapyriError>>(&mut self, e: T, range: SourceRange) -> errors::AlreadyReported {
        self.ctx.diagnostics.report_static(e, self.get_source_file(range), range);
        errors::AlreadyReported
    }
    
    pub(super) fn report<T: Into<errors::PapyriError>>(&mut self, e: T, range: SourceRange) -> errors::AlreadyReported {
        self.ctx.diagnostics.report(e.into(), self.stack_trace(), self.get_source_file(range), range);
        errors::AlreadyReported
    }
    
    pub(super) fn string_pool_mut(&mut self) -> &mut StringPool {
        &mut self.ctx.string_pool
    }
    
    pub(super) fn get_name(&self, name_id: NameID) -> RcStr {
        self.ctx.string_pool.get(name_id)
    }
}
