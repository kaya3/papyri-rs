
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
                .map_err(|e| format!("Pattern error in \"{pattern}\":\n{e}"))?;
            
            for entry in paths {
                let path = entry.map_err(|e| format!("File error in \"{pattern}\":\n{e}"))?;
                if utils::is_papyri_file(&path) {
                    source_paths.insert(path);
                }
            }
        } else {
            // filename
            let path = path::PathBuf::from(&pattern)
                .canonicalize()
                .map_err(|e| format!("File error in \"{pattern}\":\n{e}"))?;
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
    
    let mut loader = compiler::ModuleLoader::new();
    let mut diagnostics = errors::Diagnostics::new();
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
        
        if !options.force && is_unchanged(&src_path, &out_path) {
            if !options.silent {
                println!("{src_path_str} (unchanged, skipping)");
            }
            num_skipped += 1;
            continue;
        }
        
        diagnostics.clear();
        let result = loader.load_uncached(&src_path, &mut diagnostics)
            .map_err(|e| format!("Error loading \"{src_path_str}\":\n{e}"))?;
        
        let out = if options.text {
            result.out
        } else {
            let title = result.exports.get(&utils::str_ids::TITLE)
                .cloned()
                .map(|v| compiler::HTML::from_value(v, &mut loader.string_pool));
            result.out.wrap_page(title, &options.web_root)
        };
        
        diagnostics.print(options.ignore_warnings);
        if !diagnostics.is_empty() {
            eprintln!("{src_path_str} ({})", diagnostics.summary());
        } else if !options.silent {
            println!("{src_path_str} (OK)");
        }
        
        if diagnostics.num_errors == 0 {
            let out_file = fs::File::create(&out_path)
                .map_err(|e| format!("Failed to create \"{}\":\n{e}", out_path.to_string_lossy()))?;
            
            let mut out_writer = io::BufWriter::new(out_file);
            out.render_to(&mut out_writer, &mut loader.string_pool, !options.text)
                .map_err(|e| format!("Failed to write \"{}\":\n{e}", out_path.to_string_lossy()))?;
            
            num_ok += 1;
        } else {
            num_failed += 1;
        }
    }
    
    let msg = format!("{num_ok} OK, {num_failed} failed, {num_skipped} skipped");
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
