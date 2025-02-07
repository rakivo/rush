use crate::mode::Mode;
use crate::graph::Graph;
use crate::types::StrDashMap;
use crate::parser::comp::Job;

use std::io;
use std::ptr;
use std::mem;
use std::ops::Add;
use std::fs::File;
use std::path::Path;
use std::ffi::CString;
use std::time::SystemTime;
use std::os::fd::{IntoRawFd, FromRawFd};

use dashmap::DashMap;
use rayon::prelude::*;
use fxhash::FxBuildHasher;

#[cfg_attr(feature = "dbg", derive(Debug))]
pub struct Command {
    pub command: String,
    pub description: Option::<String>
}

// Custom implementation of `Command` to avoid fork + exec overhead
impl Command {
    fn create_pipe() -> io::Result::<(File, File)> {
        let mut fds = [0; 2];
        if unsafe { libc::pipe(fds.as_mut_ptr()) } != 0 {
            return Err(io::Error::last_os_error())
        }
        let r = unsafe { File::from_raw_fd(fds[0]) };
        let w = unsafe { File::from_raw_fd(fds[1]) };
        Ok((r, w))
    }

    pub fn execute(self) -> io::Result::<CommandOutput> {
        let Self { command, description } = self;

        let (mut stdout_reader, stdout_writer) = Self::create_pipe()?;
        let (mut stderr_reader, stderr_writer) = Self::create_pipe()?;

        let cmd = CString::new(command.as_bytes())?;
        let args = [c"/bin/sh".as_ptr(), c"-c".as_ptr(), cmd.as_ptr(), ptr::null()];

        let stdout_writer_fd = stdout_writer.into_raw_fd();
        let stderr_writer_fd = stderr_writer.into_raw_fd();

        let mut file_actions = unsafe { mem::zeroed() };
        unsafe {
            libc::posix_spawn_file_actions_init(&mut file_actions);
            libc::posix_spawn_file_actions_adddup2(&mut file_actions, stdout_writer_fd, libc::STDOUT_FILENO);
            libc::posix_spawn_file_actions_adddup2(&mut file_actions, stderr_writer_fd, libc::STDERR_FILENO);
        }

        let mut attr = unsafe { mem::zeroed() };
        unsafe {
            libc::posix_spawnattr_init(&mut attr);
        }

        let env = [c"PATH=/usr/bin:/bin".as_ptr(), ptr::null()];

        let mut pid = 0;
        let ret = unsafe {
            libc::posix_spawn(
                &mut pid,
                c"/bin/sh".as_ptr(),
                &file_actions,
                &attr,
                args.as_ptr() as *const *mut _,
                env.as_ptr() as *const *mut _
            )
        };

        if ret != 0 {
            return Err(io::Error::last_os_error())
        }

        unsafe {
            libc::close(stdout_writer_fd);
            libc::close(stderr_writer_fd);
        }

        let stdout = io::read_to_string(&mut stdout_reader)?;
        let stderr = io::read_to_string(&mut stderr_reader)?;

        let mut status = 0;
        unsafe {
            libc::waitpid(pid, &mut status, 0);
        }

        unsafe {
            libc::posix_spawn_file_actions_destroy(&mut file_actions);
            libc::posix_spawnattr_destroy(&mut attr);
        }

        Ok(CommandOutput {command, stdout, stderr, description})
    }
}

pub struct CommandOutput {
    pub stdout: String,
    pub stderr: String,
    pub command: String,
    pub description: Option::<String>
}

impl CommandOutput {
    #[inline]
    pub fn to_string(&self, mode: &Mode) -> String {
        if mode.quiet() {
            let CommandOutput { stderr, command, description, .. } = self;
            if stderr.is_empty() { return const { String::new() } }
            let command = description.as_ref().unwrap_or(command);
            let mut buf = String::with_capacity(command.len() + stderr.len());
            buf.push_str(command);
            buf.push_str(stderr);
            return buf
        }

        let CommandOutput { stdout, stderr, command, description } = self;
        let n = description.as_ref().map_or(0, |d| 1 + d.len() + 1)
            .add(command.len())
            .add(1)
            .add(stdout.len())
            .add(stderr.len());

        let mut buf = String::with_capacity(n);
        if mode.verbose() {
            if let Some(ref d) = description {
                buf.push('[');
                buf.push_str(d);
                buf.push(']');
                buf.push('\n');
            }
            buf.push_str(&command)
        } else if let Some(ref d) = description {
            buf.push('[');
            buf.push_str(d);
            buf.push(']');
        } else {
            buf.push_str(&command)
        }

        buf.push('\n');
        buf.push_str(&stdout);
        buf.push_str(&stderr);
        buf
    }
}

#[cfg_attr(feature = "dbg", derive(Debug))]
pub struct MetadataCache<'a> {
    files: StrDashMap::<'a, SystemTime>
}

impl<'a> MetadataCache<'a> {
    #[inline]
    pub fn new(files_count: usize) -> Self {
        Self {
            files: DashMap::with_capacity_and_hasher(files_count, FxBuildHasher::default())
        }
    }

    #[inline]
    pub fn mtime(&self, f: &'a str) -> io::Result::<SystemTime> {
        if let Some(mtime) = self.files.get(f) {
            Ok(*mtime)
        } else {
            let p: &Path = f.as_ref();
            let m = p.metadata()?.modified()?;
            self.files.insert(f, m);
            Ok(m)
        }
    }

    #[inline]
    pub fn needs_rebuild(&self, job: &Job<'a>, transitive_deps: &Graph<'a>) -> bool {
        // TODO: do something here if dependent file does not exist
        let mtimes = unsafe {
            transitive_deps.get(job.target).unwrap_unchecked()
        }.iter().filter_map(|dep| {
            self.mtime(*dep).ok()
        }).collect::<Vec::<_>>();

        let Ok(target_mtime) = self.mtime(job.target) else {
            return true
        };

        mtimes.into_par_iter().any(|src_mtime| src_mtime > target_mtime)
    }
}
