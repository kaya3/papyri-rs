
use std::{fs, io, path};
use indexmap::IndexSet;

use papyri_lang::{compiler, config, errors, utils};

fn modified_time(p: &path::Path) -> Result<std::time::SystemTime, io::Error> {
    fs::metadata(p).and_then(|m| m.modified())
}

fn is_unchanged(src_path: &path::Path, out_path: &path::Path) -> bool {
    match (modified_time(src_path), modified_time(out_path)) {
        (Ok(src_time), Ok(out_time)) => src_time <= out_time,
        _ => false,
    }
}

fn run_main() -> Result<(), String> {
    let options = config::get_config_from_args()?;
    
    if options.print_version {
        let version = env!("CARGO_PKG_VERSION");
        println!("Papyri compiler version {version}");
        return Ok(());
    }
    
    let mut source_paths = IndexSet::new();
    for pattern in options.paths {
        if pattern.chars().any(|c| matches!(c, '*' | '?' | '[')) {
            // glob pattern
            let paths = glob::glob(&pattern)
                .map_err(|e| format!("Pattern error in \"{pattern}\": {e}"))?;
            
            for entry in paths {
                let path = entry.map_err(|e| format!("File error in \"{pattern}\": {e}"))?;
                if utils::is_papyri_file(&path) {
                    source_paths.insert(path);
                }
            }
        } else {
            // filename
            let path = path::PathBuf::from(&pattern)
                .canonicalize()
                .map_err(|e| format!("File error in \"{pattern}\": {e}"))?;
            if utils::is_papyri_file(&path) {
                source_paths.insert(path);
            } else {
                eprintln!("Source file \"{pattern}\" is not a Papyri file");
            }
        }
    }
    
    let mut source_paths: Vec<path::PathBuf> = source_paths.into_iter().collect();
    source_paths.sort();
    
    let mut num_ok = 0;
    let mut num_failed = 0;
    let mut num_skipped = 0;
    let mut num_files_written = 0;
    
    let allow_write_file = options.out_dir.is_some();
    let mut loader = compiler::ModuleLoader::new();
    let mut output_files = utils::OutFiles::new(options.out_dir.map(Box::from));
    let mut diagnostics = errors::Diagnostics::new(if options.ignore_warnings {
        errors::ReportingLevel::Error
    } else {
        errors::ReportingLevel::Warning
    });
    
    for src_path in source_paths {
        let src_path_str = src_path.to_string_lossy();
        
        if utils::is_papyri_library(&src_path) {
            if !options.silent {
                println!("{src_path_str} (library, skipping)");
            }
            num_skipped += 1;
            continue;
        }
        
        let mut out_path = src_path.clone();
        if !out_path.set_extension(if options.text { "txt" } else { "html" }) {
            errors::ice(&format!("Failed to set extension of \"{}\"", out_path.to_string_lossy()));
        }
        
        if options.skip_unchanged && is_unchanged(&src_path, &out_path) {
            if !options.silent {
                println!("{src_path_str} (unchanged, skipping)");
            }
            num_skipped += 1;
            continue;
        }
        
        diagnostics.clear();
        output_files.clear();
        
        let out_files_ref = if allow_write_file { Some(&mut output_files) } else { None };
        let result = loader.load_uncached(&src_path, &mut diagnostics, out_files_ref)
            .map_err(|e| format!("Error loading \"{src_path_str}\": {e}"))?;
        
        if !result.out.is_empty() {
            let out_html = if options.text {
                result.out
            } else {
                let title = result.exports.get(&utils::str_ids::TITLE)
                    .cloned()
                    .map(|v| compiler::HTML::from_value(v, &mut loader.string_pool));
                result.out.wrap_page(title, &options.web_root)
            };
            output_files.push(out_path, out_html);
        }
        
        diagnostics.print();
        if !diagnostics.is_empty() {
            eprintln!("{src_path_str} ({})", diagnostics.summary());
        } else if !options.silent {
            println!("{src_path_str} ({})", if output_files.is_empty() { "no output, skipping" } else { "OK" });
        }
        
        if diagnostics.num_errors > 0 {
            num_failed += 1;
            continue;
        } else if output_files.is_empty() {
            num_skipped += 1;
            continue;
        } else {
            num_ok += 1;
        }
        
        for (out_path, out_html) in output_files.iter() {
            let out_path_str = out_path.to_string_lossy();
            
            out_path.parent()
                .map_or(Ok(()), fs::create_dir_all)
                .map_err(|e| format!("Failed to create directory \"{out_path_str}\": {e}"))?;
            
            let mut out_writer = fs::File::create(&out_path)
                .map(io::BufWriter::new)
                .map_err(|e| format!("Failed to create file \"{out_path_str}\": {e}"))?;
            
            compiler::Renderer::new(&loader.string_pool, !options.text, &mut out_writer)
                .render(&out_html)
                .map_err(|e| format!("Failed to write file \"{out_path_str}\": {e}"))?;
            
            if !options.silent {
                println!("    => {out_path_str}");
            }
            num_files_written += 1;
        }
    }
    
    let msg = format!("{num_files_written} file{} written: {num_ok} OK, {num_failed} failed, {num_skipped} skipped", utils::text::pluralise(num_files_written));
    if num_failed > 0 {
        Err(msg)
    } else {
        if !options.silent { println!("{msg}"); }
        Ok(())
    }
}

fn main() {
    if let Err(msg) = run_main() {
        eprintln!("{msg}");
        std::process::exit(1);
    }
}
