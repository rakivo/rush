#[derive(Copy, Clone)]
#[cfg_attr(feature = "dbg", derive(Debug))]
pub struct Loc<'a> {
    pub row: usize,
    pub file_path: &'a str
}

impl<'a> Loc<'a> {
    #[inline(always)]
    pub fn new(row: usize, file_path: &'a str) -> Self  {
        Self { row, file_path }
    }

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
    ($loc: expr, $($arg: tt)*) => {
        format!{
            "{f}:{row}: {msg}",
            f =  $loc.file_path,
            row = $loc.row,
            msg = std::fmt::format(format_args!($($arg)*))
        }
    };
    ($loc: expr, $lit: literal) => {
        format!{
            "{f}:{row}: {msg}",
            f =  $loc.file_path,
            row = $loc.row,
            msg = $lit
        }
    }
}

#[macro_export]
macro_rules! report_panic {
    ($literal: literal) => { $crate::loc::Loc::report(&std::fmt::format(format_args!($literal))) };
    ($loc: expr, $($arg: tt)*) => { $crate::loc::Loc::report(&report_fmt!($loc, $($arg)*)) }
}
