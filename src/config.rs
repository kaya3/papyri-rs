//! This module handles parsing of command-line arguments for the Papyri
//! compiler application.

use std::env;
use arg::Args;

#[derive(Args, Debug)]
/// papyri
/// Compiles Papyri to HTML.
pub struct ProgramArgs {
    #[arg(long = "version")]
    ///Print version number and then exit
    pub print_version: bool,
    
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
    
    ///The Papyri source file(s) to compile
    pub paths: Vec<String>,
}

/// Parses the command-line arguments and returns the options selected, or a
/// string containing the `--help` message if that flag is present.
pub fn get_config_from_args() -> Result<ProgramArgs, String> {
    let raw_args: Vec<String> = env::args().skip(1).collect();
    let mut args = ProgramArgs::from_args(raw_args.iter().map(String::as_str))
        .map_err(|e| e.to_string())?;
    
    if args.paths.is_empty() { args.paths.push(".".to_string()); }
    Ok(args)
}
