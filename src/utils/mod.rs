//! This module contains various utility or convenience functions which may be
//! used in multiple other modules.

mod const_strs;
mod outfiles;
pub mod relpath;
mod sliceref;
pub mod sourcefile;
mod string_pool;
pub(crate) mod taginfo;
pub mod text;

pub(crate) use const_strs::str_ids;
pub use outfiles::OutFiles;
pub use sliceref::SliceRef;
pub use string_pool::{StringPool, NameID, NameIDSet};
