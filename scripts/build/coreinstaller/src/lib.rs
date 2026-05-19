use include_dir::{Dir, include_dir};

pub static BIN: Dir<'_> = include_dir!("$TARGET_PKG_DIR");
