mod const_strs;
mod range;
mod sliceref;
mod sourcefile;
mod stringpool;
pub mod taginfo;
pub mod text;

pub use const_strs::str_ids;
pub use range::SourceRange;
pub use sliceref::SliceRef;
pub use sourcefile::{SourceFile, is_papyri_file, is_papyri_library};
pub use stringpool::{StringPool, NameID};
