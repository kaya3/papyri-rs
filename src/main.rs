
#![allow(dead_code)]

use std::collections::HashSet;
use std::fs;
use std::path::PathBuf;

mod config;
mod parser;
mod compiler;
mod utils;

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

fn pluralise(k: usize) -> &'static str {
    if k == 1 { "" } else { "s" }
}

fn run_main() -> Result<(), String> {
    let options = config::get_config_from_args()?;
    
    let mut source_paths = HashSet::new();
    for pattern in options.paths {
        if !pattern.chars().any(|c| matches!(c, '*' | '?' | '[')) {
            let path = PathBuf::from(&pattern)
                .canonicalize()
                .map_err(|e| format!("Path error: failed to canonicalise \"{}\"", e))?;
            if utils::has_papyri_extension(&path) {
                source_paths.insert(path);
            } else {
                eprintln!("source file \"{}\" does not have .papyri extension", &pattern);
            }
            continue;
        }
        
        let paths = glob::glob(&pattern)
            .map_err(|e| format!("Pattern error in \"{}\": {}", &pattern, e))?;
        
        for entry in paths {
            let path = entry.map_err(|e| format!("Path error: {}", e))?;
            if utils::has_papyri_extension(&path) {
                source_paths.insert(path);
            }
        }
    }
    
    let mut source_paths: Vec<PathBuf> = source_paths.into_iter().collect();
    source_paths.sort();
    
    let mut num_ok = 0;
    let mut num_failed = 0;
    let mut num_skipped = 0;
    
    let mut loader = compiler::ModuleLoader::new();
    let mut diagnostics = utils::Diagnostics::new();
    for src_path in source_paths {
        let mut out_path = src_path.clone();
        if !out_path.set_extension(if options.text { "txt" } else { "html" }) {
            utils::ice(&format!("Failed to set extension of \"{}\"", out_path.to_string_lossy()));
        }
        
        if !options.force && is_unchanged(&src_path, &out_path)? {
            if !options.silent {
                println!("{} (unchanged, skipping)", src_path.to_string_lossy());
            }
            num_skipped += 1;
            continue;
        }
        
        diagnostics.clear();
        let result = loader.load_uncached(&src_path, &mut diagnostics)?;
        
        let out = if options.text {
            result.out.clone()
        } else {
            let title = result.exports.get(&utils::str_ids::TITLE)
                .map(compiler::Value::clone)
                .map(|v| compiler::HTML::from_value(v, &mut loader.string_pool));
            result.out.clone().wrap_page(title, &options.web_root)
        };
        
        diagnostics.print(options.ignore_warnings);
        
        let num_errors = diagnostics.num_errors;
        let num_warnings = diagnostics.num_warnings;
        if num_errors == 0 {
            let out_file = fs::File::create(&out_path)
                .map_err(|e| format!("Failed to create \"{}\": {}", out_path.to_string_lossy(), e))?;
            
            let mut out_writer = std::io::BufWriter::new(out_file);
            out.render_to(&mut out_writer, &mut loader.string_pool, !options.text)
                .map_err(|e| format!("Failed to write \"{}\": {}", out_path.to_string_lossy(), e))?;
            
            if !options.silent {
                if num_warnings == 0 {
                    println!("{} (OK)", src_path.to_string_lossy());
                } else {
                    eprintln!(
                        "{} (OK, {} warning{})",
                        src_path.to_string_lossy(),
                        num_warnings, pluralise(num_warnings),
                    );
                }
            }
            num_ok += 1;
        } else {
            eprintln!(
                "{} (failed, {} error{}, {} warning{})",
                src_path.to_string_lossy(),
                num_errors, pluralise(num_errors),
                num_warnings, pluralise(num_warnings),
            );
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
