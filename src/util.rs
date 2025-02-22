use std::fs::File;
use std::path::Path;
use std::io::{self, Read, Error, ErrorKind};

use bumpalo::Bump;

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
