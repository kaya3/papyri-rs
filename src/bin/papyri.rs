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
    
    let in_dir = std::env::current_dir()
        .and_then(fs::canonicalize)
        .map_err(|e| format!("File error: {e} in current working directory"))?;
    
    let mut source_paths = IndexSet::new();
    for path_str in options.paths {
        if utils::text::looks_like_glob(&path_str) {
            // glob pattern
            let paths = glob::glob(&path_str)
                .map_err(|e| format!("Pattern error: {e} at \"{path_str}\""))?;
            
            for entry in paths {
                let path = entry
                    .map_err(|e| format!("File error: {e} at \"{path_str}\""))?;
                if utils::is_papyri_file(&path) {
                    source_paths.insert(path);
                }
            }
        } else {
            let path = path::PathBuf::from(&path_str);
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
    
    let mut source_paths: Vec<path::PathBuf> = source_paths.into_iter().collect();
    source_paths.sort();
    
    let mut num_ok = 0;
    let mut num_failed = 0;
    let mut num_skipped = 0;
    let mut num_files_written = 0;
    
    let out_dir = options.out_dir;
    let mut loader = compiler::ModuleLoader::new();
    let mut output_files = out_dir.as_ref().map(utils::OutFiles::new);
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
        
        let mut out_path = if let Some(out_dir) = out_dir.as_ref() {
            if let Some(p) = utils::relpath::make_relative(&in_dir, &src_path) {
                out_dir.join(p)
            } else {
                return Err(format!("No sensible output path for \"{src_path_str}\""));
            }
        } else {
            src_path.clone()
        };
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
        if matches!(output_files, Some(ref o) if !o.is_empty()) { errors::ice("Output files were not consumed"); }
        
        let result = loader.load_uncached(&src_path, &mut diagnostics, output_files.as_mut())
            .map_err(|e| format!("Error loading \"{src_path_str}\": {e}"))?;
        
        let mut to_write = output_files.as_mut()
            .map_or_else(Vec::new, |o| o.take_iter().collect());
        if !result.out.is_empty() {
            to_write.push((out_path, result.out));
        }
        
        diagnostics.print();
        if !diagnostics.is_empty() {
            eprintln!("{src_path_str} ({})", diagnostics.summary());
        } else if !options.silent {
            println!("{src_path_str} ({})", if to_write.is_empty() { "no output, skipping" } else { "OK" });
        }
        
        if diagnostics.num_errors > 0 {
            num_failed += 1;
            continue;
        } else if to_write.is_empty() {
            num_skipped += 1;
            continue;
        } else {
            num_ok += 1;
        }
        
        for (out_path, out_html) in to_write.into_iter() {
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
    
    let msg = format!(
        "{num_files_written} file{} written; {num_ok} OK, {num_failed} failed, {num_skipped} skipped",
        utils::text::pluralise(num_files_written),
    );
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
