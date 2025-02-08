macro_rules! mode_getter {
    ($($f: ident: $bit: path), *) => {
        $(#[inline(always)] pub fn $f(&self) -> bool {(self.0 & (1 << $bit as u64)) != 0})*
    }
}

macro_rules! mode_new {
    ($($flag: expr => $method: ident($($arg: expr), *)), *) => {
        #[inline(always)]
        pub fn new(flag_parser: &flager::Parser) -> Self {
            let mut mode = 0;
            $(if flag_parser.$method($($arg)*) {
                mode |= (1 << $flag as u64)
            })*
            Self(mode)
        }
    }
}

macro_rules! define_modes {
    ($([$short: literal, $long: literal, $name: ident, $description: literal]), *) => { paste::paste! {
        $(
            const [<$name:snake:upper _MODE>]: flager::Flag = flager::Flag::new(
                concat!("-", $short),
                concat!("--", $long),
                None
            ).help($description);
        )*

        #[repr(u64)]
        enum FlagBit { $([<$name:camel>]), * }

        #[repr(transparent)]
        #[cfg_attr(feature = "dbg", derive(Debug))]
        pub struct Mode(u64);

        impl Mode {
            pub fn print_help() {
                $(println!("{flag}", flag = [<$name:snake:upper _MODE>]);)*
            }

            mode_getter! { $($name: FlagBit:: [<$name:camel>]), * }
            mode_new! { $(FlagBit:: [<$name:camel>] => passed(&[<$name:snake:upper _MODE>])), * }
        }
    }}
}

macro_rules! flag_getter {
    ($($f: ident: $ty: ty), *) => {
        $(#[inline(always)] pub fn $f(&self) -> Option::<&$ty> { self.$f.as_ref() })*
    }
}

macro_rules! flag_new {
    ($($($def: expr)?; $name: ident), *) => { paste::paste! {
        #[inline(always)]
        pub fn new(flag_parser: &flager::Parser) -> Self {
            Self {
                mode: Mode::new(flag_parser),
                $($name: if flag_parser.passed(&[<$name:snake:upper _MODE>]) {
                    flag_parser.parse(&[<$name:snake:upper _MODE>])$(.or(Some($def)))?
                } else {
                    None
                }), *
            }
        }
    }}
}

macro_rules! define_flags {
    ($([$($def: expr)?; $short: literal, $long: literal, $name: ident: $ty: ty, $description: literal]), *) => { paste::paste! {
        $(
            const [<$name:snake:upper _MODE>]: flager::Flag::<$ty> = flager::Flag::new(
                concat!("-", $short),
                concat!("--", $long),
                None
            ).help($description);
        )*

        #[cfg_attr(feature = "dbg", derive(Debug))]
        pub struct Flags {
            mode: Mode,
            $($name: Option::<$ty>), *
        }

        impl std::ops::Deref for Flags {
            type Target = Mode;

            #[inline(always)]
            fn deref(&self) -> &Self::Target {
                &self.mode
            }
        }

        impl Flags {
            pub fn print_help() {
                Mode::print_help();
                $(println!("{flag}", flag = [<$name:snake:upper _MODE>]);)*
            }

            flag_getter! { $($name: $ty), * }
            flag_new! { $($($def)*; $name), * }
        }
    }};
}

/*
    Modes are flags that don't take any value, for example: `-v`, `-B`, `--help`.

    Flags are flags that may or may not take a value, if default is specified,
    for example: `-j`, this is going to be the equivalent to `-j=1`, or `-j 1`,
    the same goes for `-k` flag.
*/

define_modes! {
    ["h", "help",           help,                  "print this text and exit"],
    ["u", "up-to-date",     check_is_up_to_date,   "check if up-to-date, without running anything"],
    ["p", "print-commands", print_commands,        "only print commands, without running anything"],
    ["q", "quiet",          quiet,                 "print commands only if stderr is not empty, else: stay quiet"],
    ["v", "verbose",        verbose,               "print both command and description while executing job. Also print `already built` text for every job"],
    ["B", "always-build",   always_build,          "always build job, no matter is it up-to-date or not"]
}

define_flags! {
    [; "f",  "file",        file_path:      String, "specify custom file path to `rush` script"],
    [; "t",  "target",      default_target: String, "specify custom default target"],
    [1; "j", "jobs",        parallelalism:  i64,    "specify custom maximum amount of jobs can be executed in parallel"],
    [1; "k", "keep-going",  max_fail_count: usize,  "specify custom count of fails until exit"]
}
