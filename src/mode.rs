macro_rules! getter {
    ($($f: ident: $bit: path), *) => {
        $(#[inline(always)]
        pub fn $f(&self) -> bool {
            (self.0 & (1 << $bit as u64)) != 0
        })*
    }
}

macro_rules! new {
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

macro_rules! define_flags {
    ($([$short: ident, $name: ident]), *) => { paste::paste! {
        $(
            const [<$name:snake:upper _MODE>]: flager::Flag = flager::Flag::new(
                concat!("-", stringify!($short)),
                concat!("--", stringify!($name)),
                None
            );
        )*

        #[repr(u64)]
        enum FlagBit { $([<$name:camel>]), * }

        #[repr(transparent)]
        #[cfg_attr(feature = "dbg", derive(Debug))]
        pub struct Mode(u64);

        impl Mode {
            getter! { $($name: FlagBit:: [<$name:camel>]), * }
            new! { $(FlagBit:: [<$name:camel>] => passed(&[<$name:snake:upper _MODE>])), * }
        }
    }}
}

define_flags! {
    [q, quiet],
    [v, verbose],
    [B, always_build]
}
