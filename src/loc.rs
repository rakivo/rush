#[repr(transparent)]
#[derive(Copy, Clone, Default)]
#[cfg_attr(feature = "dbg", derive(Debug))]
pub struct Loc(pub usize);

impl Loc {
    #[inline(always)]
    #[cfg_attr(feature = "dbg", track_caller)]
    pub fn report(literal: &str) -> ! {
        #[cfg(feature = "dbg")] { panic!("{literal}") }
        #[cfg(not(feature = "dbg"))] {
            eprintln!("{literal}");
            std::process::exit(1)
        }
    }
}

#[macro_export]
macro_rules! report_fmt {
    ($loc: expr, $($arg:tt)*) => {
        format!{
            "{f}:{row}: {msg}",
            f = crate::parser::Parser::RUSH_FILE_NAME,
            row = $loc.0,
            msg = std::fmt::format(format_args!($($arg)*))
        }
    };
    ($loc: expr, $lit: literal) => {
        format!{
            "{f}:{row}: {msg}",
            f = crate::parser::Parser::RUSH_FILE_NAME,
            row = $loc.0,
            msg = $lit
        }
    }
}

#[macro_export]
macro_rules! report {
    ($loc: expr, $($arg:tt)*) => { crate::loc::Loc::report(&report_fmt!($loc, $($arg)*)) }
}
