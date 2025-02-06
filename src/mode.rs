use flager::{Flag, Parser as FlagParser, new_flag};

const QUIET_MODE: Flag = new_flag!("-q", "--quiet");
const VERBOSE_MODE: Flag = new_flag!("-v", "--verbose");
const ALWAYS_BUILD_MODE: Flag = new_flag!("-B", "--always-build");

#[repr(packed)]
pub struct Mode {
    quiet: bool,
    verbose: bool,
    always_build: bool
}

macro_rules! getter {
    ($($f: ident: $ty: ty), *) => {
        $(#[inline(always)]
        pub fn $f(&self) -> $ty {
            self.$f
        })*
    };
}

macro_rules! new {
    ($($flag: ident = $method: ident($($arg: expr), *)), *) => {
        #[inline(always)]
        pub fn new(flag_parser: &FlagParser) -> Self {
            Self { $($flag: flag_parser.$method($($arg)*)), * }
        }
    };
}

impl Mode {
    getter!{
        quiet: bool,
        verbose: bool,
        always_build: bool
    }

    new!{
        quiet = passed(&QUIET_MODE),
        verbose = passed(&VERBOSE_MODE),
        always_build = passed(&ALWAYS_BUILD_MODE)
    }
}
