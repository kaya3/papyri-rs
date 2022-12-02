use std::{fs, io};
use std::path::{Path, PathBuf};
use arg::Args;
use indexmap::IndexSet;

use papyri_lang::{compiler, errors, utils};

fn main() {
    let mut args: ProgramArgs = arg::parse_args();
    
    if args.print_version {
        let version = env!("CARGO_PKG_VERSION");
        println!("Papyri compiler version {version}");
        std::process::exit(0);
    }
    
    if args.paths.is_empty() {
        args.paths.push(".".to_string());
    }
    
    if let Err(msg) = Main::new(args).run() {
        eprintln!("{msg}");
        std::process::exit(1);
    }
}

#[derive(Args)]
///papyri
///Compiles Papyri to HTML.
pub struct ProgramArgs {
    #[arg(long = "version")]
    ///Print version number and then exit
    print_version: bool,
    
    #[arg(short = "u", long = "skip-unchanged")]
    ///Skip unchanged source files
    pub skip_unchanged: bool,
    
    #[arg(short, long)]
    ///Suppress output when there are no errors or warnings
    pub silent: bool,
    
    #[arg(long = "ignore-warnings")]
    ///Suppress diagnostic information for warnings
    pub ignore_warnings: bool,
    
    #[arg(short, long)]
    ///Compile to text instead of HTML
    pub text: bool,
    
    #[arg(short, long = "out")]
    ///Output directory (default is the current directory)
    pub out_dir: Option<std::path::PathBuf>,
    
    ///The Papyri source file(s) to compile. If none are specified, the current
    ///directory is searched for Papyri source files.
    pub paths: Vec<String>,
}

struct Main {
    options: ProgramArgs,
    ctx: compiler::Context,
}

enum SourceFileResult {
    OkWroteFiles(u32),
    SkippedLibrary,
    SkippedUnchanged,
    SkippedNoOutput,
    Failed,
}

impl Main {
    fn new(options: ProgramArgs) -> Main {
        let reporting_level = if options.ignore_warnings {
            errors::ReportingLevel::Error
        } else {
            errors::ReportingLevel::All
        };
        
        let ctx = compiler::Context::new(reporting_level, options.out_dir.as_deref());
        Main {options, ctx}
    }
    
    fn run(&mut self) -> Result<(), String> {
        let in_dir = std::env::current_dir()
            .and_then(fs::canonicalize)
            .map_err(|e| format!("File error: {e} in current working directory"))?;
        
        let mut num_ok = 0;
        let mut num_failed = 0;
        let mut num_skipped = 0;
        let mut num_files_written = 0;
        
        for src_path in self.get_source_paths()? {
            match self.process_source_file(&src_path, &in_dir)? {
                SourceFileResult::OkWroteFiles(k) => {
                    num_ok += 1;
                    num_files_written += k;
                },
                SourceFileResult::SkippedLibrary |
                SourceFileResult::SkippedUnchanged |
                SourceFileResult::SkippedNoOutput => {
                    num_skipped += 1;
                },
                SourceFileResult::Failed => {
                    num_failed += 1;
                },
            }
        }
        
        let msg = format!(
            "{num_files_written} file{} written; {num_ok} OK, {num_failed} failed, {num_skipped} skipped",
            utils::text::pluralise(num_files_written),
        );
        if num_failed > 0 {
            Err(msg)
        } else {
            if !self.options.silent { println!("{msg}"); }
            Ok(())
        }
    }
    
    fn get_source_paths(&self) -> Result<IndexSet<PathBuf>, String> {
        let mut source_paths = IndexSet::new();
        for path_str in self.options.paths.iter() {
            if utils::text::looks_like_glob(path_str) {
                // glob pattern
                let paths = glob::glob(path_str)
                    .map_err(|e| format!("Pattern error: {e} at \"{path_str}\""))?;
                
                for entry in paths {
                    let path = entry
                        .map_err(|e| format!("File error: {e} at \"{path_str}\""))?;
                    if utils::is_papyri_file(&path) {
                        source_paths.insert(path);
                    }
                }
            } else {
                let path = PathBuf::from(path_str);
                if path.is_dir() {
                    // directory
                    let paths = utils::relpath::find_papyri_source_files_in_dir(
                        &path,
                        |p, e| eprintln!("File error: {e} in \"{}\"", p.to_string_lossy()),
                    );
                    if let Some(paths) = paths {
                        for p in paths {
                            source_paths.insert(path.join(p));
                        }
                    }
                } else if utils::is_papyri_file(&path) {
                    // source file
                    source_paths.insert(path);
                } else {
                    eprintln!("File \"{path_str}\" is not a Papyri source file");
                }
            }
        }
        source_paths.sort();
        Ok(source_paths)
    }
    
    fn process_source_file(&mut self, src_path: &Path, in_dir: &Path) -> Result<SourceFileResult, String> {
        let src_path_str = src_path.to_string_lossy();
        
        if utils::is_papyri_library(&src_path) {
            if !self.options.silent {
                println!("{src_path_str} (library, skipping)");
            }
            return Ok(SourceFileResult::SkippedLibrary);
        }
        
        let out_path = self.get_out_path(&src_path, &in_dir)?;
        
        if self.options.skip_unchanged && is_unchanged(&src_path, &out_path) {
            if !self.options.silent {
                println!("{src_path_str} (unchanged, skipping)");
            }
            return Ok(SourceFileResult::SkippedUnchanged);
        }
        
        self.ctx.reset();
        let result = self.ctx
            .load_uncached(&src_path)
            .map_err(|e| format!("Error loading \"{src_path_str}\": {e}"))?;
        
        let mut to_write = self.ctx.out_files
            .as_mut()
            .map_or_else(Vec::new, |o| o.take_iter().collect());
        if !result.out.is_empty() {
            to_write.push((out_path, result.out));
        }
        
        let diagnostics = &mut self.ctx.diagnostics;
        diagnostics.print();
        
        if !diagnostics.is_empty() {
            eprintln!("{src_path_str} ({})", diagnostics.summary());
        } else if !self.options.silent {
            println!("{src_path_str} ({})", if to_write.is_empty() { "no output, skipping" } else { "OK" });
        }
        
        if diagnostics.num_errors > 0 {
            Ok(SourceFileResult::Failed)
        } else if to_write.is_empty() {
            Ok(SourceFileResult::SkippedNoOutput)
        } else {
            let k = to_write.len() as u32;
            for (out_path, html) in to_write.into_iter() {
                self.write_out_file(&out_path, html)?;
            }
            Ok(SourceFileResult::OkWroteFiles(k))
        }
    }
    
    fn get_out_path(&self, src_path: &Path, in_dir: &Path) -> Result<PathBuf, String> {
        let mut out_path = if let Some(ref out_dir) = self.options.out_dir {
            if let Some(p) = utils::relpath::make_relative(&in_dir, &src_path) {
                out_dir.join(p)
            } else {
                return Err(format!("No sensible output path for \"{}\"", src_path.to_string_lossy()));
            }
        } else {
            src_path.to_path_buf()
        };
        
        if out_path.set_extension(if self.options.text { "txt" } else { "html" }) {
            Ok(out_path)
        } else {
            errors::ice(&format!("Failed to set extension of \"{}\"", out_path.to_string_lossy()));
        }
    }
    
    fn write_out_file(&mut self, path: &Path, html: compiler::HTML) -> Result<(), String> {
        let path_str = path.to_string_lossy();
        
        path.parent()
            .map_or(Ok(()), fs::create_dir_all)
            .map_err(|e| format!("Failed to create directory \"{path_str}\": {e}"))?;
        
        let mut out_writer = fs::File::create(&path)
            .map(io::BufWriter::new)
            .map_err(|e| format!("Failed to create file \"{path_str}\": {e}"))?;
        
        self.ctx.render(&html, !self.options.text, &mut out_writer)
            .map_err(|e| format!("Failed to write file \"{path_str}\": {e}"))?;
        
        if !self.options.silent {
            println!("    => {path_str}");
        }
        Ok(())
    }
}

fn is_unchanged(src_path: &Path, out_path: &Path) -> bool {
    let modified_time = |p: &Path| fs::metadata(p).and_then(|m| m.modified());
    
    match (modified_time(src_path), modified_time(out_path)) {
        (Ok(src_time), Ok(out_time)) => src_time <= out_time,
        _ => false,
    }
}
