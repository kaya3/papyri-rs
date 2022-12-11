//! This module contains various utility or convenience functions which may be
//! used in multiple other modules.

mod const_strs;
mod outfiles;
mod range;
pub mod relpath;
mod sliceref;
mod sourcefile;
mod string_pool;
pub(crate) mod taginfo;
pub mod text;

pub(crate) use const_strs::str_ids;
pub use outfiles::OutFiles;
pub(crate) use range::SourceRange;
pub use sliceref::SliceRef;
pub use sourcefile::{SourceFile, is_papyri_file, is_papyri_library};
pub use string_pool::{StringPool, NameID};
