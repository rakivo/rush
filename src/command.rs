#[cfg(feature = "dbg")]
use crate::cr::Stdout;
use crate::flags::Mode;
use crate::graph::Graph;
use crate::parser::comp::Job;
use crate::types::StrDashMap;
use crate::cr::{Subprocess, SubprocessMap};

use std::io;
use std::ptr;
use std::mem;
use std::ops::Add;
use std::fs::File;
use std::sync::Arc;
use std::path::Path;
use std::ffi::CString;
use std::time::SystemTime;
use std::os::fd::{AsFd, AsRawFd};
use std::os::fd::{IntoRawFd, FromRawFd};
use std::sync::atomic::{Ordering, AtomicUsize};

use dashmap::DashMap;
use fxhash::FxBuildHasher;
use crossbeam_channel::Sender;
use nix::poll::{PollFd, PollFlags};

#[cfg(feature = "dbg")]
macro_rules! cmd_print {
    ($stdout: expr, $($arg:tt)*) => {{
        #[cfg(feature = "dbg")] {
            _ = $crate::command::Command::print(&$stdout, format!{
                "{f}:{l}:{c}:\n",
                f = file!(), l = line!(), c = column!()
            });
        }
        _ = $crate::command::Command::print(
            &$stdout,
            std::fmt::format(format_args!($($arg)*))
        );
    }};
}

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

    #[inline]
    pub fn print_command(self) -> CommandOutput {
        let Self { command, description } = self;
        CommandOutput {
            command,
            description,

            status: 0,
            stdout: String::new(),
            stderr: String::new(),
        }
    }

    #[inline(always)]
    #[cfg(feature = "dbg")]
    fn print(stdout: &Stdout, s: String) {
        #[cfg(feature = "dbg")] {
            stdout.send(s).unwrap()
        } #[cfg(not(feature = "dbg"))] {
            _ = stdout.send(s)
        }
    }

    pub fn execute(
        &self,
        curr_subprocess_id: Arc::<AtomicUsize>,
        #[cfg(feature = "dbg")] stdout: Stdout,
        poll_fds_sender: Sender::<PollFd>,
        fd_to_command: &Arc::<SubprocessMap>,
        active_fds: &Arc::<AtomicUsize>,
    ) -> io::Result::<()> {
        let cmd = CString::new(self.command.as_bytes())?;
        let args = [
            c"/bin/sh".as_ptr(),
            c"-c".as_ptr(),
            cmd.as_ptr(),
            ptr::null(),
        ];

        let (stdout_reader, stdout_writer) = Self::create_pipe()?;
        let (stderr_reader, stderr_writer) = Self::create_pipe()?;

        let stdout_reader_fd = stdout_reader.as_raw_fd();
        let stderr_reader_fd = stderr_reader.as_raw_fd();

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
                env.as_ptr() as *const *mut _,
            )
        };

        if ret != 0 {
            return Err(io::Error::last_os_error())
        }

        unsafe {
            libc::close(stdout_writer_fd);
            libc::close(stderr_writer_fd);
        }

        let id = curr_subprocess_id.load(Ordering::Relaxed);

        fd_to_command.insert(stdout_reader_fd, Subprocess {
            id,
            command: self.command.to_owned()
        });

        fd_to_command.insert(stderr_reader_fd, Subprocess {
            id,
            command: self.command.to_owned()
        });

        curr_subprocess_id.fetch_add(1, Ordering::Relaxed);

        active_fds.fetch_add(1, Ordering::Relaxed);

        let stdout_reader_fd: &'static _ = Box::leak(Box::new(stdout_reader));
        let stderr_reader_fd: &'static _ = Box::leak(Box::new(stderr_reader));

        let stdout_pollfd = PollFd::new(stdout_reader_fd.as_fd(), PollFlags::POLLIN);
        let stderr_pollfd = PollFd::new(stderr_reader_fd.as_fd(), PollFlags::POLLIN);

        #[cfg(feature = "dbg_hardcore")] {
            {
                let mut stdout_ = format!("sending: FD: {stdout_pollfd:?} ");
                if let Some(revents) = stdout_pollfd.revents() {
                    let revents = format!("revents: {revents:?}");
                    stdout_.push_str(&revents)
                }
                stdout_.push('\n');
                Self::print(&stdout, stdout_);
            }

            {
                let mut stderr = format!("sending: FD: {stdout_pollfd:?} ");
                if let Some(revents) = stdout_pollfd.revents() {
                    let revents = format!("revents: {revents:?}");
                    stderr.push_str(&revents)
                }
                stderr.push('\n');
                Self::print(&stdout, stderr);
            }
        }

        poll_fds_sender.send(stdout_pollfd).unwrap();
        poll_fds_sender.send(stderr_pollfd).unwrap();

        Ok(())
    }
}

pub struct CommandOutput {
    pub status: i32,
    pub stdout: String,
    pub stderr: String,
    pub command: String,
    pub description: Option::<String>
}

impl CommandOutput {
    #[inline]
    pub fn to_string(&self, mode: &Mode) -> String {
        if mode.print_commands() {
            let mut buf = String::with_capacity(self.command.len());
            buf.push_str(&self.command);
            buf.push('\n');
            return buf
        }

        if mode.quiet() {
            let CommandOutput { stderr, command, description, .. } = self;
            if stderr.is_empty() { return const { String::new() } }
            let command = description.as_ref().unwrap_or(command);
            let mut buf = String::with_capacity(command.len() + stderr.len());
            buf.push_str(command);
            buf.push_str(stderr);
            return buf
        }

        let CommandOutput { stdout, stderr, command, description, .. } = self;
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

        mtimes.into_iter().any(|src_mtime| src_mtime > target_mtime)
    }
}
