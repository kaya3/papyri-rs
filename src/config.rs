use std::env;
use arg::Args;

const DEFAULT_WEB_ROOT: &str = "https://kaya3.github.io/papyri/";

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
    
    #[arg(short, long = "web-root", default_value = "DEFAULT_WEB_ROOT.to_string()")]
    ///URL path of directory containing papyri.css and papyri.js
    pub web_root: String,
    
    ///The Papyri source file(s) to compile
    pub paths: Vec<String>,
}

pub fn get_config_from_args() -> Result<ProgramArgs, String> {
    let raw_args: Vec<String> = env::args().skip(1).collect();
    let mut args = ProgramArgs::from_args(raw_args.iter().map(String::as_str))
        .map_err(|e| e.to_string())?;
    
    if args.paths.is_empty() { args.paths.push("./**/*.papyri".to_string()); }
    if !args.web_root.ends_with("/") { args.web_root += "/"; }
    
    Ok(args)
}
