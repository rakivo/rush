pub mod syntax {
    pub const RULE: &str = "rule";
    pub const PHONY: &str = "phony";
    pub const BUILD: &str = "build";

    pub const COMMAND: &str = "command";
    pub const DEPFILE: &str = "depfile";
    pub const DEFAULT: &str = "default";
    pub const DESCRIPTION: &str = "description";

    pub const COMMENT: char = '#';
    pub const LINE_ESCAPE: char = '$';
}

pub const CLEAN_TARGET: &str = "clean";
pub const PHONY_TARGETS: &[&str] = &["all", CLEAN_TARGET];

pub const BUILD_DIR_VARIABLE: &str = "builddir";
