use crate::ux;
use crate::loc::Loc;
use crate::parser::Compiled;
use crate::dbg_unwrap::DbgUnwrap;

use std::fs::File;
use std::path::Path;
use std::process::exit;
use std::fmt::{Arguments, Write};
use std::io::{self, Read, Error, ErrorKind};

use bumpalo::Bump;

#[inline]
#[cfg_attr(feature = "dbg", track_caller)]
pub fn report_undefined_target(target: &str, loc: Option::<&Loc>, context: &Compiled) -> ! {
    let targets = context.pretty_print_targets();
    let mut msg = String::with_capacity(
        "undefined target: ".len()  +
        target.len()                +
        "did you mean: ".len()      +
        64                          +
        "available targets: ".len() +
        256
    );

    let mut print = |args: Arguments| {
        msg.write_fmt(args).unwrap_dbg()
    };

    print(format_args!("undefined target: {target}\n"));

    if let Some(this) = ux::did_you_mean_compiled(
        target,
        &context.edges,
        &context.rules,
    ) {
        print(format_args!("note: did you mean: {this}?\n"))
    }

    print(format_args!("available targets: [{targets}]"));

    if let Some(loc) = loc {
        eprintln!{
            "{report}",
            report = report_fmt!(loc, "{msg}")
        };
    } else {
        eprintln!("{msg}")
    }

    exit(1)
}

#[inline]
pub fn pretty_print_slice<T>(slice: &[T], sep: &str) -> String
where
    T: std::fmt::Display
{
    let mut buf = String::with_capacity(256);
    slice.first().map(|s| buf.push_str(&s.to_string()));
    slice.iter().skip(1).for_each(|s| {
        buf.push_str(sep);
        buf.push_str(&s.to_string())
    }); buf
}

#[inline(always)]
#[cfg_attr(feature = "dbg", track_caller)]
pub fn unreachable() -> ! {
    #[cfg(feature = "dbg")] { unreachable!() }
    #[cfg(not(feature = "dbg"))] unsafe {
        std::hint::unreachable_unchecked()
    }
}

#[inline]
#[cfg_attr(feature = "dbg", track_caller)]
pub fn read_file_into_arena<'bump, P>(arena: &'bump Bump, path: P) -> io::Result::<&'bump mut [u8]>
where
    P: AsRef::<Path>
{
    let mut file = File::open(path)?;

    let file_size = file.metadata().unwrap().len() as usize;
    let content = arena.alloc_slice_fill_default(file_size);

    file.read_exact(content).map(|_| content)
}

#[inline]
#[cfg_attr(feature = "dbg", track_caller)]
pub fn read_file_into_arena_str<'bump, P>(arena: &'bump Bump, path: P) -> io::Result::<&'bump str>
where
    P: AsRef::<Path>
{
    match read_file_into_arena(arena, path) {
        Ok(bytes) => {
            std::str::from_utf8(&*bytes).map_err(|e| {
                let kind = ErrorKind::InvalidData;
                let err = Error::new(kind, e);
                err
            })
        }
        Err(e) => Err(e)
    }
}
