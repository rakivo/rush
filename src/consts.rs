pub mod syntax {
    pub const RULE: &str = "rule";
    pub const PHONY: &str = "phony";
    pub const BUILD: &str = "build";

    pub const COMMAND: &str = "command";
    pub const DEPFILE: &str = "depfile";
    pub const DESCRIPTION: &str = "description";

    pub const COMMENT: char = '#';
    pub const LINE_ESCAPE: char = '$';
}

pub const PHONY_TARGETS: &[&str] = &["all", "clean"];
