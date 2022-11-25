
use std::{fs, io};
use std::path::PathBuf;
use indexmap::IndexSet;

use papyri_lang::{compiler, config, errors, utils};

fn is_unchanged(src_path: &PathBuf, out_path: &PathBuf) -> Result<bool, String> {
    let src_modified = match fs::metadata(src_path)
        .map_err(|e| format!("Failed to read file metadata of \"{}\": {}", src_path.to_string_lossy(), e))?
        .modified() {
            Ok(src_modified) => src_modified,
            Err(_) => return Ok(false),
        };
    
    let out_modified = match fs::metadata(out_path)
        .map(|metadata| metadata.modified()) {
            Ok(Ok(out_modified)) => out_modified,
            _ => return Ok(false),
        };
    
    Ok(src_modified <= out_modified)
}

fn run_main() -> Result<(), String> {
    let options = config::get_config_from_args()?;
    
    if options.print_version {
        println!("Papyri compiler version {}", env!("CARGO_PKG_VERSION"));
        return Ok(());
    }
    
    let mut source_paths = IndexSet::new();
    for pattern in options.paths {
        if pattern.chars().any(|c| matches!(c, '*' | '?' | '[')) {
            // glob pattern
            let paths = glob::glob(&pattern)
                .map_err(|e| format!("Pattern error in \"{}\": {}", &pattern, e))?;
            
            for entry in paths {
                let path = entry.map_err(|e| format!("Path error: {}", e))?;
                if utils::is_papyri_file(&path) {
                    source_paths.insert(path);
                }
            }
        } else {
            // filename
            let path = PathBuf::from(&pattern)
                .canonicalize()
                .map_err(|e| format!("\"{}\": {}", pattern, e))?;
            if utils::is_papyri_file(&path) {
                source_paths.insert(path);
            } else {
                eprintln!("source file \"{}\" is not a Papyri file", &pattern);
            }
        }
    }
    
    let mut source_paths: Vec<PathBuf> = source_paths.into_iter().collect();
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
                println!("{} (library, skipping)", src_path_str);
            }
            num_skipped += 1;
            continue;
        }
        
        let mut out_path = src_path.clone();
        if !out_path.set_extension(if options.text { "txt" } else { "html" }) {
            errors::ice(&format!("Failed to set extension of \"{}\"", out_path.to_string_lossy()));
        }
        
        if !options.force && is_unchanged(&src_path, &out_path)? {
            if !options.silent {
                println!("{} (unchanged, skipping)", src_path.to_string_lossy());
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
            eprintln!("{} ({})", src_path_str, diagnostics.summary());
        } else if !options.silent {
            println!("{} (OK)", src_path_str);
        }
        
        if diagnostics.num_errors == 0 {
            let out_file = fs::File::create(&out_path)
                .map_err(|e| format!("Failed to create \"{}\": {}", out_path.to_string_lossy(), e))?;
            
            let mut out_writer = io::BufWriter::new(out_file);
            out.render_to(&mut out_writer, &mut loader.string_pool, !options.text)
                .map_err(|e| format!("Failed to write \"{}\": {}", out_path.to_string_lossy(), e))?;
            
            num_ok += 1;
        } else {
            num_failed += 1;
        }
    }
    
    let msg = format!("{} OK, {} failed, {} skipped", num_ok, num_failed, num_skipped);
    if num_failed > 0 {
        Err(msg)
    } else {
        if !options.silent { println!("{}", msg); }
        Ok(())
    }
}

fn main() {
    if let Err(msg) = run_main() {
        eprintln!("{}", msg);
        std::process::exit(1);
    }
}
