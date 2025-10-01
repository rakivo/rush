use crate::parser::read_file;
use crate::types::StrDashMap;

use std::fs;
use std::fmt::Display;
use std::path::Path;
use std::io::{self, Write};

use memmap2::Mmap;
use dashmap::mapref::one::Ref;
use fxhash::FxBuildHasher;

#[repr(transparent)]
#[cfg_attr(feature = "dbg", derive(Debug))]
pub struct Metadata {
    pub command_hash: u64,
}

impl Display for Metadata {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.command_hash.fmt(f)
    }
}

type Result<T> = std::result::Result<T, ()>;
type MetadataMap<'a> = StrDashMap<'a, Metadata>;

#[repr(transparent)]
#[cfg_attr(feature = "dbg", derive(Debug))]
pub struct Db<'a> {
    map: MetadataMap<'a>,
}

impl<'a> Db<'a> {
    pub const RUSH_FILE_NAME: &'static str = ".rush_cache";

    #[inline(always)]
    pub fn read_cache(cache_file_path: &str) -> Option<Mmap> {
        read_file(cache_file_path).or(read_file(Self::RUSH_FILE_NAME))
    }

    #[inline(always)]
    pub fn metadata_read(&self, output: &str) -> Option<Ref<'_, &'a str, Metadata>> {
        self.map.get(output)
    }

    #[inline(always)]
    pub fn metadata_write(&self, output: &'a str, md: Metadata) {
        self.map.insert(output, md);
    }

    #[inline(always)]
    fn new() -> Self {
        Self {
            map: MetadataMap::with_capacity_and_hasher(32, FxBuildHasher::default()),
        }
    }

    pub fn read(content: &'a str) -> Result<Self> {
        let db = Self::new();
        for line in content.lines() {
            let mut tokens = line.split_ascii_whitespace();

            let Some(output) = tokens.next() else {
                return Err(());
            };
            let Some(command_hash) = tokens.next() else {
                return Err(());
            };
            let Ok(command_hash) = command_hash.parse() else {
                return Err(());
            };

            let md = Metadata { command_hash };
            db.map.insert(output, md);
        }
        Ok(db)
    }

    #[inline(always)]
    pub fn write() -> Self {
        Self::new()
    }

    pub fn write_finish(&self, cache_file_path: &str) -> io::Result<()> {
        if self.map.is_empty() {
            if fs::exists::<&Path>(cache_file_path.as_ref()).unwrap_or(false) {
                fs::remove_file(cache_file_path)?
            }

            if fs::exists::<&Path>(Self::RUSH_FILE_NAME.as_ref()).unwrap_or(false) {
                fs::remove_file(Self::RUSH_FILE_NAME)?
            }

            return Ok(());
        }

        let mut buf = String::with_capacity(256 + 20 + 1);
        let mut file = fs::File::create(cache_file_path)?;
        for e in self.map.iter() {
            buf.clear();
            buf.push_str(e.key());
            buf.push(' ');
            buf.push_str(&e.value().to_string());
            buf.push('\n');

            file.write_all(buf.as_bytes())?
        }
        Ok(())
    }
}
