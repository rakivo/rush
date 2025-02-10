#[repr(transparent)]
#[derive(Copy, Clone)]
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

pub static mut RUSH_FILE_PATH_LEN: usize = 0;
pub static mut RUSH_FILE_PATH_PTR: *const u8 = std::ptr::null();

/* SAFETY:
    RUSH_FILE_PATH_PTR and RUSH_FILE_PATH_LEN are initialized once at the start of the program,
    before any threads are spawned, and are never mutated afterward. This ensures that:

    1. RUSH_FILE_PATH_PTR always points to a valid UTF-8 string with a "static" lifetime.
    2. RUSH_FILE_PATH_LEN always matches the length of the string pointed to by RUSH_FILE_PATH_PTR.
    3. No mutable references to these variables are created, ensuring thread safety in multithreaded contexts.
*/

#[macro_export]
macro_rules! report_fmt {
    ($loc: expr, $($arg: tt)*) => {
        format!{
            "{f}:{row}: {msg}",
            f = unsafe {
                std::str::from_utf8_unchecked(std::slice::from_raw_parts($crate::loc::RUSH_FILE_PATH_PTR, $crate::loc::RUSH_FILE_PATH_LEN))
            },
            row = $loc.0,
            msg = std::fmt::format(format_args!($($arg)*))
        }
    };
    ($loc: expr, $lit: literal) => {
        format!{
            "{f}:{row}: {msg}",
            f = unsafe {
                std::str::from_utf8_unchecked(std::slice::from_raw_parts($crate::loc::RUSH_FILE_PATH_PTR, $crate::loc::RUSH_FILE_PATH_LEN))
            },
            row = $loc.0,
            msg = $lit
        }
    }
}

#[macro_export]
macro_rules! report_panic {
    ($loc: expr, $($arg: tt)*) => { $crate::loc::Loc::report(&report_fmt!($loc, $($arg)*)) }
}
